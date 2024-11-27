library(tidymodels)
library(vroom)
library(timetk)
library(modeltime)
library(patchwork)

item_train <- vroom("train.csv")
item_test <- vroom("test.csv")

# Choose store-item combination for train and test data
item1train <- item_train |> 
  filter(store == 5, item == 8)

item1test <- item_test |> 
  filter(store == 5, item == 8)

item2train <- item_train |> 
  filter(store == 3, item == 22)

item2test <- item_test |> 
  filter(store == 3, item == 22)

# Split training data for each item
cv_split1 <- time_series_split(item1train,
                               assess = "3 months",
                               cumulative = TRUE)

cv_split2 <- time_series_split(item2train,
                               assess = "3 months",
                               cumulative = TRUE)

# Create prophet model for both store-item combinations
prophet_mod1 <- prophet_reg() |> 
  set_engine(engine = "prophet") |> 
  fit(sales ~ date, data = training(cv_split1))

prophet_mod2 <- prophet_reg() |> 
  set_engine(engine = "prophet") |> 
  fit(sales ~ date, data = training(cv_split2))

# Calibrate workflows
cv_results1 <- modeltime_calibrate(prophet_mod1,
                                  new_data = testing(cv_split1))

cv_results2 <- modeltime_calibrate(prophet_mod2,
                                   new_data = testing(cv_split2))

# Visualize results
cv1_plot <- cv_results1 |> 
  modeltime_forecast(new_data = testing(cv_split1),
                     actual_data = training(cv_split1)) |> 
  plot_modeltime_forecast(.interactive=FALSE)

cv2_plot <- cv_results2 |> 
  modeltime_forecast(new_data = testing(cv_split2),
                     actual_data = training(cv_split2)) |> 
  plot_modeltime_forecast(.interactive=FALSE)

# Refit whole dataset for both observations
fullfit1 <- cv_results1 |> 
  modeltime_refit(data = item1train)

fullfit2 <- cv_results2 |> 
  modeltime_refit(data = item2train)

# Predict for all observations in dataset
fit_plot1 <- fullfit1 |> 
  modeltime_forecast(new_data = item1test,
                     actual_data = item1train) |> 
  plot_modeltime_forecast(.interactive = FALSE)

fit_plot2 <- fullfit2 |> 
  modeltime_forecast(new_data = item2test,
                     actual_data = item2train) |> 
  plot_modeltime_forecast(.interactive = FALSE)

# Create 4 panel plot
panel_plot <- ((cv1_plot|cv2_plot)/
                (fit_plot1|fit_plot2))

panel_plot
