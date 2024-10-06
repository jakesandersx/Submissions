library(xgboost)
library(caret)  # For train-test split
library(dplyr)  # For data manipulation
library(readr)
library(ggplot2)

# Load the data
data <- read_csv("data_sample.csv")

# Data cleaning and preparation
data <- data %>%
  filter(!is.na(hit_exit_speed) & !is.na(hit_distance)) %>%
  filter((hit_exit_speed > 90) & (hit_distance > 100)) %>%
  mutate(pitch_type = as.numeric(factor(pitch_type)),
         bat_side = as.numeric(factor(bat_side)),
         pitcher_throws = as.numeric(factor(pitcher_throws))) %>%
  select(-event_result)  # Remove columns not used for prediction

# Split data by year
years <- unique(data$year)
predictions_list <- list()
variance_by_year_list <- list()

for (year in years) {
  # Filter data for the current year
  year_data <- data %>% filter(year == !!year)
  
  # Train-test split
  set.seed(123)  # For reproducibility
  trainIndex <- createDataPartition(year_data$hit_distance, p = .7, list = FALSE, times = 1)
  trainData <- year_data[trainIndex, ]
  testData <- year_data[-trainIndex, ]
  
  # Prepare matrices for XGBoost
  train_matrix <- xgb.DMatrix(data = as.matrix(trainData %>% select(-hit_distance)),
                              label = trainData$hit_distance)
  test_matrix <- xgb.DMatrix(data = as.matrix(testData %>% select(-hit_distance)),
                             label = testData$hit_distance)
  
  # Set parameters for XGBoost
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 10,
    eta = 0.07,
    subsample = 0.75,
    colsample_bytree = 0.75
  )
  
  # Train the model
  xgb_model <- xgb.train(params = params,
                         data = train_matrix,
                         nrounds = 10000,
                         watchlist = list(train = train_matrix, test = test_matrix),
                         early_stopping_rounds = 10000)
  
  # Predict on the test set
  predictions <- predict(xgb_model, test_matrix)
  
  # Calculate RMSE
  rmse <- sqrt(mean((predictions - testData$hit_distance)^2))
  cat("Year:", year, "RMSE:", rmse, "\n")
  
  # Store results
  results <- data.frame(
    observed = testData$hit_distance,
    predicted = predictions,
    year = year
  )
  
  predictions_list[[as.character(year)]] <- results
  
  variance_by_year <- results %>%
    summarize(
      observed_variance = var(observed, na.rm = TRUE),
      predicted_variance = var(predicted, na.rm = TRUE),
      difference = var(observed - predicted)
    )
  
  variance_by_year$year <- year
  variance_by_year_list[[as.character(year)]] <- variance_by_year
}

# Combine results
all_predictions <- do.call(rbind, predictions_list)
all_variance_by_year <- do.call(rbind, variance_by_year_list)

# Plot observed vs predicted hit distance by year
ggplot(all_predictions, aes(x = observed, y = predicted)) +
  geom_point(color = "red", alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Observed vs. Predicted Hit Distance by Year",
       x = "Observed Hit Distance",
       y = "Predicted Hit Distance") +
  theme_minimal() +
  facet_wrap(~ year)

# Plot variance of hit distance (observed vs. predicted) by year
ggplot(all_variance_by_year, aes(x = year)) +
  geom_line(aes(y = observed_variance, color = "Observed"), size = 1) +
  geom_line(aes(y = predicted_variance, color = "Predicted"), size = 1, linetype = "dashed") +
  geom_line(aes(y = difference, color = "Difference"), size = 1, linetype = "dashed") +
  labs(title = "Variance of Hit Distance (Observed vs. Predicted) by Year",
       x = "Year",
       y = "Variance") +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red", "Difference" = "green")) +
  theme_minimal()

print(all_variance_by_year)
