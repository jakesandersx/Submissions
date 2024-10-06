library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

data <- read_csv("data_sample.csv")

# Are balls travelling farther in a given year?
# Balls with equal launch_speed and launch_angle should travel the same distance (~1-2 feet for air density / resistance)
# Discard ground balls, weak contact
# Previous knowledge indicates Pulled Fly-Balls are most-likely to be home runs

data <- data %>%
  filter((hit_vertical_angle > 0) & (hit_exit_speed > 85) & (!is.na(hit_spinrate)))

#round data to integers
data <- data %>%
  mutate(across(where(is.numeric), ~ round(.)))


summary(data$hit_distance)
summary(data$hit_exit_speed)
summary(data$hit_spinrate)

print(sd(data$hit_distance))
print(sd(data$hit_exit_speed))
print(sd(data$hit_spinrate))

# Create the classification column
data <- data %>%
  mutate(classification = case_when(
    hit_bearing < -20 & hit_vertical_angle > 25 & bat_side == "R" ~ "Pulled FB",
    hit_bearing >= -20 & hit_bearing <= 20  & hit_vertical_angle > 25 & bat_side == "R" ~ "Centered FB",
    hit_bearing > 20 & hit_vertical_angle > 25 & bat_side == "R" ~ "Pushed FB",
    
    hit_bearing < -20 & hit_vertical_angle <= 25 & hit_vertical_angle >= 10 & bat_side == "R" ~ "Pulled LD",
    hit_bearing >= -20 & hit_bearing <= 20 & hit_vertical_angle <= 25 & hit_vertical_angle >= 10 & bat_side == "R" ~ "Centered LD",
    hit_bearing > 20 & hit_vertical_angle > 25& hit_vertical_angle >= 10 & bat_side == "R" ~ "Pushed LD",
    
    hit_bearing < -20 & hit_vertical_angle > 25 & bat_side == "L" ~ "Pushed FB",
    hit_bearing >= -20 & hit_bearing <= 20  & hit_vertical_angle > 25 & bat_side == "L" ~ "Centered FB",
    hit_bearing > 20 & hit_vertical_angle > 25 & bat_side == "L" ~ "Pulled FB",
    
    hit_bearing < -20 & hit_vertical_angle <= 25 & hit_vertical_angle >= 10 & bat_side == "L" ~ "Pushed LD",
    hit_bearing >= -20 & hit_bearing <= 20 & hit_vertical_angle <= 25 & hit_vertical_angle >= 10 & bat_side == "L" ~ "Centered LD",
    hit_bearing > 20 & hit_vertical_angle > 25& hit_vertical_angle >= 10 & bat_side == "L" ~ "Pulled LD",
    TRUE ~ NA_character_  # Handles cases that do not match any condition
  ))

# View the updated data frame
data <- data %>%
  filter(!is.na(classification))

percentage_results <- data %>%
  group_by(year, classification, event_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(year, classification) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  select(year, classification, event_result, percentage)

homers <- percentage_results %>%
  group_by(classification, year) %>%
  filter(event_result == "home_run" & classification == "Pulled FB")

p <- ggplot(homers, aes(x = classification, y = percentage, color = as.factor(year))) +
  geom_point() +
  labs(title = "Event Result Distribution by Classification and Year",
       x = "Classification",
       y = "Percentage",
       color = "Year") +
  theme_minimal()

# Convert to interactive plot using plotly
interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot

#Home run rates on pulled fly balls:
#2015: 47%
#2016: 50%
#2017: 55%
#2018: 61%
#2019: 36%
