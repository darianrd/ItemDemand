library(tidymodels)
library(vroom)
library(forecast)
library(patchwork)

# Read in data
item_test <- vroom("test.csv")
item_train <- vroom("train.csv")

# Filter down to just 1 store item for exploration and model building
store_item <- item_train |> 
  filter(store == 1, item == 1)

# Create time series plot
tsplot <- store_item |> 
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

# Create autocorrelation function plots
acf1mo <- store_item |> 
  pull(sales) |> 
  forecast::ggAcf()

acf2yr <- store_item |> 
  pull(sales) |> 
  forecast::ggAcf(lag.max = 2*365)

# Another store item
store_item2 <- item_train |> 
  filter(store == 5, item == 11)

tsplot2 <- store_item2 |> 
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

acf1mo2 <- store_item2 |> 
  pull(sales) |> 
  forecast::ggAcf()

acf2yr2 <- store_item2 |> 
  pull(sales) |> 
  forecast::ggAcf(lag.max = 2*365)

# Another store item
store_item3 <- item_train |> 
  filter(store == 6, item == 25)

tsplot3 <- store_item3 |> 
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

acf1mo3 <- store_item3 |> 
  pull(sales) |> 
  forecast::ggAcf()

acf2yr3 <- store_item3 |> 
  pull(sales) |> 
  forecast::ggAcf(lag.max = 2*365)

# Create 2x3 panel plot
panel_plot <- ((tsplot2|acf1mo2|acf2yr2)/
                (tsplot3|acf1mo3|acf2yr3))
