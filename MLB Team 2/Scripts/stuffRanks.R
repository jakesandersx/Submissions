library(dplyr)
library(readr)

# Read the CSV file
avgs <- read_csv("avgs.csv")

# Modify break_x columns: multiply positive values by -1
avgs <- avgs %>%
  mutate(across(contains("break_x"), ~ ifelse(. > 0, . * -1, .)))

# Calculate averages
avg_fb_velo <- mean(avgs$ff_avg_speed, na.rm = TRUE)
avg_fb_hB <- mean(avgs$ff_avg_break_x, na.rm = TRUE)
avg_fb_vB <- mean(avgs$ff_avg_break_z, na.rm = TRUE)

avg_ch_velo <- mean(avgs$ch_avg_speed, na.rm = TRUE)
avg_ch_hB <- mean(avgs$ch_avg_break_x, na.rm = TRUE)
avg_ch_vB <- mean(avgs$ch_avg_break_z, na.rm = TRUE)

avg_sl_velo <- mean(avgs$sl_avg_speed, na.rm = TRUE)
avg_sl_hB <- mean(avgs$sl_avg_break_x, na.rm = TRUE)
avg_sl_vB <- mean(avgs$sl_avg_break_z, na.rm = TRUE)

data <- data.frame(
  Type = c("FB", "SL", "CH"),
  Velo = c(92.7, 81.4, 84.5),
  Spin = c(2145, 2675, 1760),
  Direction = c(228, 84, 230),
  Efficiency = c(0.8, 0.38, 0.86),
  hB = c(8.5, -8.5, 13.3),
  vB = c(13.4, -1.6, 9.1)
)

ggplot(data, aes(x = hB, y = vB)) +
  # Add dashed lines for axes
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Add the main points
  geom_point(size = 7) +
  # Add points for each pitch type
  geom_point(aes(x = avg_fb_hB, y = avg_fb_vB), color = "red", size = 5, shape = 1, stroke = 1.5) +
  geom_point(aes(x = avg_ch_hB, y = avg_ch_vB), color = "green", size = 5, shape = 1, stroke = 1.5) +
  geom_point(aes(x = avg_sl_hB, y = avg_sl_vB), color = "blue", size = 5, shape = 1, stroke = 1.5) +
  # Adjust the coordinate ratio
  coord_fixed(ratio = 1) +
  # Use a minimal theme
  theme_minimal() + 
  # Set limits for the axes
  xlim(c(-40, 40)) +
  ylim(c(-40, 40)) +
  # Add a legend
  labs(title = "Pitch Breaks", 
       x = "Horizontal Break (hB)", 
       y = "Vertical Break (vB)") +
  # Customize the legend
  scale_color_manual(values = c("FF" = "red", "CH" = "green", "SL" = "blue")) +
  theme(legend.position = "top")
