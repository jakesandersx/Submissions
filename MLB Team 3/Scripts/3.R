library(dplyr)
library(ggplot2)

data <- read.csv("data_sample.csv")

#Remove weak contact < 85 EV
#Remove ground balls < 10 LA
#Remove pop-ups: > 50 LA

allData <- data %>%
  filter(hit_vertical_angle > 10 & hit_vertical_angle <= 50 & hit_exit_speed >= 85)

flyBalls <- data %>%
  filter(hit_vertical_angle >= 25 & hit_vertical_angle <= 50 & hit_exit_speed >= 85)

allBins <- allData %>%
  mutate(angle_bin = cut(hit_vertical_angle, 
                         breaks = seq(10, 50, by = 5), 
                         right = FALSE, 
                         include.lowest = TRUE)) %>%
  mutate(angle_bin = factor(angle_bin, 
                            levels = levels(angle_bin),
                            labels = paste(seq(10, 45, by = 5), seq(15, 50, by = 5), sep = "-")))

flyBins <- flyBalls %>%
  mutate(angle_bin = cut(hit_vertical_angle, 
                         breaks = seq(25, 50, by = 5), 
                         right = FALSE, 
                         include.lowest = TRUE)) %>%
  mutate(angle_bin = factor(angle_bin, 
                            levels = levels(angle_bin),
                            labels = paste(seq(25, 45, by = 5), seq(30, 50, by = 5), sep = "-")))


# Create the scatter plot
ggplot(allBins, aes(x = hit_exit_speed, y = hit_distance, color = angle_bin)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", aes(group = year, color = as.factor(year)), se = FALSE) +
  labs(title = "Hit Exit Speed vs. Hit Distance by Launch Angle Bins",
       x = "Hit Exit Speed",
       y = "Hit Distance",
       color = "Launch Angle Bin") +
  theme_minimal()

ggplot(flyBins, aes(x = hit_exit_speed, y = hit_distance, color = angle_bin)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", aes(group = year, color = as.factor(year)), se = FALSE) +
  labs(title = "Hit Exit Speed vs. Hit Distance by Launch Angle Bins",
       x = "Hit Exit Speed",
       y = "Hit Distance",
       color = "Launch Angle Bin") +
  theme_minimal()
