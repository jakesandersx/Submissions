# All code was aimed to try and solve the given prompt:
### Given <code>data_sample.csv</code>, is there any specific year in the data that shows evidence of the ball being juiced?

## 1.R
Classifies the data into pull, center, push, as well as line-drive, fly-ball, pop-up, and gets the percentages of pulled fly-balls that result in Home Runs. These types of batted-balls are most common for home runs.

## 2.R
Generates an XGBoost model to predict distance based on exit_velocity and launch_angle

## 3.R
Sorts data into bins based on launch_angle per year. Generates a regression line for each year