library(ggplot2)
library(readr)
library(tidyverse)
library(patchwork)

# Load data
data <- read_csv("AnalyticsQuestionnairePitchData.csv")

# Filter out NULL PitchType
data <- data %>%
  filter(PitchType != "NULL")

home_plate <- data.frame(
  x = c(-0.83, 0.83, 0.83, 0, -0.83),
  y = c(0.5, 0.5, 0.25, 0, 0.25)
)

plots <- list()

# Create heatmap for each PitchType
for (pitch_type in unique(data$PitchType)) {
  filtered_data <- data %>%
    filter(PitchType == pitch_type)
  
  if (nrow(filtered_data) > 0) {
    plot <- ggplot(filtered_data, aes(x = TrajectoryLocationX, y = TrajectoryLocationZ)) +
      stat_density2d(aes(fill = after_stat(level)), geom = "polygon", contour = TRUE) +  # Create heatmap with density levels
      scale_fill_gradientn(colors = c("darkblue", "#0096FF", "salmon", "red", "white")) +  # Adjust colors
      annotate("rect", xmin = -0.83, xmax = 0.83, 
               ymin = mean(filtered_data$StrikeZoneBottom, na.rm = TRUE), 
               ymax = mean(filtered_data$StrikeZoneTop, na.rm = TRUE), 
               fill = NA, color = "black", size = 0.5) +
      coord_fixed(ratio = 1) +
      geom_polygon(data = home_plate, aes(x = x, y = y), fill = "white", color = "black", size = 0.5) +
      xlim(c(-5, 5)) +
      ylim(c(0, 5)) +
      theme_minimal() +
      ggtitle(paste("Pitch Type:", pitch_type))
    
    plots[[pitch_type]] <- plot  # Store the plot with pitch type as the key
  }
}

# Combine all plots into a single image
heatmaps <- wrap_plots(plots) + plot_layout(ncol = 3)

# Save the combined plot
ggsave("./Tigers/heatmaps.png", heatmaps, width = 12, height = 8)
