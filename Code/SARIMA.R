library(tidymodels)
library(vroom)
library(modeltime)
library(timetk)
library(patchwork)

item_train <- vroom("train.csv")
item_test <- vroom("test.csv")

# Choose store item combination for train and test data
item1train <- item_train |> 
  filter(store == 10, item == 22)

item1test <- item_test |> 
  filter(store == 10, item == 22)

item2train <- item_train |> 
  filter(store == 3, item == 27)

item2test <- item_test |> 
  filter(store == 3, item == 27)

# Split training data for each item
cv_split1 <- time_series_split(item1train,
                              assess = "3 months",
                              cumulative = TRUE)

cv_split2 <- time_series_split(item2train,
                              assess = "3 months",
                              cumulative = TRUE)

# Create plots for splits
cv_split1 |> 
  tk_time_series_cv_plan() |> 
  plot_time_series_cv_plan(date, sales, .interactive = FALSE)

cv_split2 |> 
  tk_time_series_cv_plan() |> 
  plot_time_series_cv_plan(date, sales, .interactive = FALSE)

# Create recipe
arima_recipe <- recipe(sales ~ ., data = item1train) |> 
  step_date(date, features = "doy") |> 
  step_range(date_doy, min = 0, max = pi) |> 
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) |> 
  step_date(date, features = "month")
  
# Define the ARIMA model
arima_mod <- arima_reg(seasonal_period = "3 months",
                       non_seasonal_ar = 5,
                       non_seasonal_ma = 5,
                       seasonal_ar = 2,
                       seasonal_ma = 2,
                       non_seasonal_differences = 2,
                       seasonal_differences = 2) |> 
  set_engine("auto_arima")

# Merge into single workflow and fit to training data
arima_wf1 <- workflow() |> 
  add_recipe(arima_recipe) |> 
  add_model(arima_mod) |> 
  fit(data = training(cv_split1))

arima_wf2 <- workflow() |> 
  add_recipe(arima_recipe) |> 
  add_model(arima_mod) |> 
  fit(data = training(cv_split2))

# Calibrate models
cv_results1 <- modeltime_calibrate(arima_wf1,
                                   new_data = testing(cv_split1))

cv_results2 <- modeltime_calibrate(arima_wf2,
                                   new_data = testing(cv_split2))

# Visualize results
plot1 <- cv_results1 |> 
  modeltime_forecast(new_data = testing(cv_split1),
                     actual_data = training(cv_split1)) |> 
  plot_modeltime_forecast(.interactive = FALSE)

plot2 <- cv_results2 |> 
  modeltime_forecast(new_data = testing(cv_split2),
                     actual_data = training(cv_split2)) |> 
  plot_modeltime_forecast(.interactive = FALSE)

# Refit whole dataset
fullfit1 <- cv_results1 |> 
  modeltime_refit(data = item1train)

fullfit2 <- cv_results2 |> 
  modeltime_refit(data = item2train)

# Predict for all observations
plot3 <- fullfit1 |> 
  modeltime_forecast(new_data = item1test,
                     actual_data = item1train) |> 
  plot_modeltime_forecast(.interactive = FALSE)

plot4 <- fullfit2 |> 
  modeltime_forecast(new_data = item2test,
                     actual_data = item2train) |> 
  plot_modeltime_forecast(.interactive = FALSE)

# Create four-panel plot
((plot1|plot2)/
  (plot3|plot4))

