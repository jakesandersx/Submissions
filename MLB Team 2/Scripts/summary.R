library(ggplot2)
library(tidyverse)
library(readr)
library(gridExtra)  # Make sure to load gridExtra for grid.arrange
library(grid)   
library(DT) # Load grid for tableGrob

# Load the data
data <- read_csv("AnalyticsQuestionnairePitchData.csv")

data <- data %>%
  filter(PitchType != "NULL")

format_stat <- function(value) {
  if (!is.na(value)) {
    return(sprintf(".%03d", round(value * 1000)))  # Format to .xxx
  }
  return(NA)
}

# Create a mapping for PitchType to Pitch Name
pitch_mapping <- c(
  CH = "Changeup",
  CU = "Curveball",
  FC = "Cutter",
  FF = "4-Seam",
  KC = "Knuckle-Curve",
  SI = "Sinker",
  SL = "Slider"
)

# Replace PitchType with Pitch Name
data$PitchType <- recode(data$PitchType, !!!pitch_mapping)

fullSummary <- data %>%
  # Calculate total batters faced (BF)
  summarize(
    BF = n_distinct(AtBatNumber),
    # Calculate Strike% accurately
    StrikePercent = round(sum(PitchCall != "ball" & PitchCall != "walk" & PitchCall != "blocked_ball") / n() * 100, 1),  # Correct calculation of Strike%
    H = statCalculator(data)$H,  # Sum of hits given up
    BB = sum(PitchCall == "walk"),
    K = statCalculator(data)$K,  # Sum of strikeouts
    BAA = statCalculator(data)$AVG,  # BAA
    wOBA = statCalculator(data)$wOBA   # wOBA
  )

# Create a summary table
summary_table <- data %>%
  group_by(PitchType) %>%
  summarize(
    Count = n(),
    PitchPercent = n() / nrow(data) * 100,
    Velo = mean(ReleaseSpeed, na.rm = TRUE),
    vertB = mean(TrajectoryVerticalBreak, na.rm = TRUE),
    horB = mean(TrajectoryHorizontalBreak, na.rm = TRUE),
    Spin = mean(ReleaseSpinRate, na.rm = TRUE)
  ) %>%
  ungroup()

# Define a function to calculate BAA and wOBA
calculate_stats <- function(pitch_name) {
  filtered_data <- data %>% filter(PitchType == pitch_name)
  h <- as.numeric(statCalculator(filtered_data)$AVG)
  avg <- as.numeric(statCalculator(filtered_data)$AVG)  # Ensure it's numeric
  woba <- as.numeric(statCalculator(filtered_data)$wOBA)  # Ensure it's numeric
  return(c(BAA = avg, wOBA = woba))
}

# Calculate BAA and wOBA for each Pitch Name
summary_table <- summary_table %>%
  rowwise() %>%
  mutate(
    Stats = list(calculate_stats(PitchType)),  # Call the stats calculation function
    BAA = Stats[1],  # Extract BAA
    wOBA = Stats[2]  # Extract wOBA
  ) %>%
  select(-Stats) %>%  # Remove the intermediate stats column
  ungroup()  # Ungroup after mutating

# Split into tables for Right-handed and Left-handed pitchers
format_stat <- function(value) {
  if (!is.na(value)) {
    return(sprintf(".%03d", round(value * 1000)))  # Format to .xxx
  }
  return(NA)
}

summary_R <- data %>%
  filter(PitcherHand == "R") %>%
  group_by(PitchType) %>%
  summarize(
    Count = n(),
    PitchPercent = round(n() / nrow(data[data$PitcherHand == "R", ]) * 100, 1),  # Round to 1 decimal
    Velo = round(mean(ReleaseSpeed, na.rm = TRUE), 1),  # Round to 1 decimal
    vertB = round(mean(TrajectoryVerticalBreak, na.rm = TRUE), 1),  # Round to 1 decimal
    horB = round(mean(TrajectoryHorizontalBreak, na.rm = TRUE), 1),  # Round to 1 decimal
    Spin = round(mean(ReleaseSpinRate, na.rm = TRUE), 0)  # Round to 0 decimal
  ) %>%
  rowwise() %>%
  mutate(
    Stats = list(calculate_stats(PitchType)),
    BAA = format_stat(Stats[1]),  # Format BAA
    wOBA = format_stat(Stats[2])   # Format wOBA
  ) %>%
  select(-Stats) %>%
  ungroup()

summary_L <- data %>%
  filter(PitcherHand == "L") %>%
  group_by(PitchType) %>%
  summarize(
    Count = n(),
    PitchPercent = round(n() / nrow(data[data$PitcherHand == "L", ]) * 100, 1),  # Round to 1 decimal
    Velo = round(mean(ReleaseSpeed, na.rm = TRUE), 1),  # Round to 1 decimal
    vertB = round(mean(TrajectoryVerticalBreak, na.rm = TRUE), 1),  # Round to 1 decimal
    horB = round(mean(TrajectoryHorizontalBreak, na.rm = TRUE), 1),  # Round to 1 decimal
    Spin = round(mean(ReleaseSpinRate, na.rm = TRUE), 0)  # Round to 0 decimal
  ) %>%
  rowwise() %>%
  mutate(
    Stats = list(calculate_stats(PitchType)),
    BAA = format_stat(Stats[1]),  # Format BAA
    wOBA = format_stat(Stats[2])   # Format wOBA
  ) %>%
  select(-Stats) %>%
  ungroup()

# Create a Total Summary without filtering by PitcherHand
total_summary <- data %>%
  group_by(PitchType) %>%
  summarize(
    Count = n(),
    PitchPercent = round(n() / nrow(data) * 100, 1),  # Round to 1 decimal
    Velo = round(mean(ReleaseSpeed, na.rm = TRUE), 1),  # Round to 1 decimal
    vertB = round(mean(TrajectoryVerticalBreak, na.rm = TRUE), 1),  # Round to 1 decimal
    horB = round(mean(TrajectoryHorizontalBreak, na.rm = TRUE), 1),  # Round to 1 decimal
    Spin = round(mean(ReleaseSpinRate, na.rm = TRUE), 0)  # Round to 0 decimal
  ) %>%
  rowwise() %>%
  mutate(
    Stats = list(calculate_stats(PitchType)),
    BAA = format_stat(Stats[1]),  # Format BAA
    wOBA = format_stat(Stats[2])   # Format wOBA
  ) %>%
  select(-Stats) %>%
  ungroup()

# Function to convert a data frame to a grob without row names
table_grob <- function(data, title) {
  title_grob <- textGrob(title, gp = gpar(fontsize = 16, fontface = "bold"))  # Title for the table
  table <- tableGrob(data, rows = NULL)  # Set rownames to FALSE
  combined_grob <- arrangeGrob(title_grob, table, ncol = 1)  # Arrange title and table
  return(combined_grob)
}

# Convert data frames to grobs with titles
grob_fullSummary <- table_grob(fullSummary, "Quick Hits")
grob_summary_L <- table_grob(summary_L, "Left-Handed Pitching")
grob_summary_R <- table_grob(summary_R, "Right-Handed Pitching")
grob_total_summary <- table_grob(total_summary, "Full Summary")

tables <- grid.arrange(
  grob_total_summary,
  arrangeGrob(grob_summary_L, grob_summary_R, ncol = 2),
  grob_fullSummary,   # Full Summary   # Quick Hits
  nrow = 3              # Update the number of rows to 4
)

# Save as PNG
ggsave("./Tigers/tables.png", tables, width = 16, height = 12, dpi = 300)

