# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(readr)  # Ensure this is loaded for read_csv

# Assuming your data is in a dataframe called 'data' with the columns 'PitcherHand', 'BatterSide', 'PitchCall', and 'PitchType'
data <- read_csv("AnalyticsQuestionnairePitchData.csv")

# Original statCalculator function
statCalculator <- function(df) {
  
  valid_PitchCall <- c("single", "double", "triple", "home_run", "field_out", "strikeout", "strikeout_double_play", 
                       "fielders_choice", "sac_bunt_double_play", "sac_fly_double_play", "other_out",
                       "grounded_into_double_play", "force_out","double_play", "fielders_choice_out", 
                       "field_error", "catcher_interf", "triple_play", "sac_bunt", "sac_fly", 
                       "walk", "hit_by_pitch")
  
  df <- df %>% filter(PitchCall %in% valid_PitchCall)
  
  df %>% 
    summarise(
      singles = sum(PitchCall == "single"),
      doubles = sum(PitchCall == "double"),
      triples = sum(PitchCall == "triple"),
      homers = sum(PitchCall == "home_run"),
      outs = sum(PitchCall %in% c("field_out", "strikeout", "strikeout_double_play", "fielders_choice", 
                                  "sac_bunt_double_play", "sac_fly_double_play", "other_out", 
                                  "grounded_into_double_play", "force_out", "double_play", 
                                  "fielders_choice_out", "field_error", "catcher_interf", "triple_play")),
      strikeouts = sum(PitchCall %in% c("strikeout", "strikeout_double_play")),
      sac = sum(PitchCall %in% c("sac_bunt", "sac_fly")),
      walks = sum(PitchCall == "walk"),
      hbps = sum(PitchCall == "hit_by_pitch"),
      hits = singles + doubles + triples + homers,
      onBaseNum = hits + hbps + walks,
      pas = n(),
      abs = hits + outs,
      AVG = hits / abs,
      OBP = onBaseNum / (abs + hbps + walks + sac),
      SLG = (singles + 2 * doubles + 3 * triples + 4 * homers) / abs,
      OPS = OBP + SLG,
      wOBA = (.691 * walks + .721 * hbps + .882 * singles + 1.254 * doubles + 1.588 * triples + 2.045 * homers) / 
        (abs + walks + sac + hbps)
    ) %>%
    mutate(
      AVG = sprintf("%.3f", AVG),
      OBP = sprintf("%.3f", OBP),
      SLG = sprintf("%.3f", SLG),
      OPS = sprintf("%.3f", OPS),
      wOBA = sprintf("%.3f", wOBA)
    ) %>%
    select(ABs = abs, H = hits, K = strikeouts, AVG, OBP, SLG, OPS, wOBA)
}

# Define a function to create a Punnett square plot for a specific pitch type
create_punnett_square <- function(data, pitch_type, title) {
  # Filter data by pitch type
  filtered_data <- data %>%
    filter(PitchType %in% pitch_type) %>%
    group_by(PitcherHand, BatterSide) %>%
    summarise(wOBA = statCalculator(cur_data())$wOBA, .groups = 'drop') # Use statCalculator to get wOBA
  
  # Create the plot
  plot <- ggplot(filtered_data, aes(x = PitcherHand, y = BatterSide, fill = as.numeric(wOBA))) +
    geom_tile(color = "black") +
    scale_fill_gradientn(colors = c("green", "darkgreen", "gold", "orange", "red"), name = "wOBA") +
    labs(title = title) +  # Set the title from the argument
    theme_minimal() +
    theme(axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold")) +
    geom_text(aes(label = wOBA), color = "black", size = 5) +  # Add wOBA values as text labels
    scale_x_discrete(labels = c("R" = "Right-Handed Pitcher", "L" = "Left-Handed Pitcher")) +  # Rename x-axis labels
    scale_y_discrete(labels = c("R" = "Right-Handed Batter", "L" = "Left-Handed Batter"))  # Rename y-axis labels
  
  return(plot)
}

# Define pitch types for each Punnett square
fastballs <- c("FF", "FC", "SI")
offspeed <- c("CH")
breakers <- c("KC", "CU", "SL")

# Create the plots with specific titles
fastball_plot <- create_punnett_square(data, fastballs, "Fastballs")
offspeed_plot <- create_punnett_square(data, offspeed, "Offspeed")
breakers_plot <- create_punnett_square(data, breakers, "Breaking Balls")

# Combine the plots
punnett <- grid.arrange(fastball_plot, offspeed_plot, breakers_plot, ncol = 1)

# Save the combined plot as a PNG file
ggsave("./Tigers/punnett_squares.png", plot = punnett, width = 8, height = 12, dpi = 300)
