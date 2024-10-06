library(ggplot2)
library(tidyverse)
library(readr)
library(gridExtra)  # For stacking plots vertically

# Load the data
data <- read_csv("AnalyticsQuestionnairePitchData.csv")

# Transform the data
data <- data %>%
  mutate(pfx_x = TrajectoryHorizontalBreak * 12 * -1,
         pfx_z = TrajectoryVerticalBreakInduced * 12) %>%
  filter(PitchType != "NULL")  # Remove rows with NULL values in PitchType

# Calculate average pfx values
data <- data %>%
  group_by(PitchType, PitcherHand) %>%
  summarize(avg_pfx_x = mean(pfx_x, na.rm = TRUE),
            avg_pfx_z = mean(pfx_z, na.rm = TRUE)) %>%
  ungroup()  # Ungroup to prevent unwanted grouping in ggplot

# Create separate plots for each PitcherHand
plot_R <- ggplot(data %>% filter(PitcherHand == "R"), aes(x = avg_pfx_x, y = avg_pfx_z, fill = PitchType)) +
  geom_point(shape = 21, size = 4, stroke = 1, color = "black") +  # Add border color
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  xlab("Horizontal Movement (Catcher POV)") + 
  ylab("Vertical Movement") +
  coord_fixed(ratio = 1) +
  ggtitle("Pitch Movement for Right-Handed Pitchers") +
  xlim(c(-20, 20)) +
  ylim(c(-20, 20)) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

plot_L <- ggplot(data %>% filter(PitcherHand == "L"), aes(x = avg_pfx_x, y = avg_pfx_z, fill = PitchType)) +
  geom_point(shape = 21, size = 4, stroke = 1, color = "black") +  # Add border color
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  
  
  
  xlab("Horizontal Movement (Catcher POV)") + 
  ylab("Vertical Movement") +
  coord_fixed(ratio = 1) +
  ggtitle("Pitch Movement for Left-Handed Pitchers") +
  xlim(c(-20, 20)) +
  ylim(c(-20, 20)) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

# Stack the plots vertically
movementPlots <- grid.arrange(plot_R, plot_L, ncol = 1)

# Save the combined plot as a PNG file
ggsave("./Tigers/movementPlot.png", plot = movementPlots, width = 8, height = 12, dpi = 300)
