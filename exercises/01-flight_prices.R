# Exercise 01 - flight price data ----
# The data to this exercise is proprietary, hence available only to specific individuals.
# To the regular exercise, please refer to a differnt exercise script.

# Read flight data ----
library(tidyverse)
flights <- readr::read_csv("datasets/local/flight_prices.csv") # <- replace location with S3

# glimpse
glimpse(flights)

# Train/test split ----
set.seed(0)
flights <- flights %>% 
  mutate(is_train = runif(NROW(order_id)) < 0.8)

# This is an advance exercise, in the sense that you are free to "roam in the wild" as long as you are 
# handling the problems with the methods we have shown in class (or otherwise closely related methods).

# The exercise:
# We would like to predict the **order_total_spent** as best we can, using any of the tools we have discussed so far.
# That is, you can use feature selection (e.g., stepwise), feature importance, transformation of variables anyway you like,
# for example: factoring, log, sqrt, or any other which comes to mind.
# preprocessing anyway you like, outlier removal, quantile regression, etc.

# If you know pca (which we will talk about later) or regularization methods (lasso, ridge) you can also use them.
# If you have additional relevant tools from the current domain (linear regression and generalization), you are welcome to use them.

# Comparing results ----

# function to compute test error rates, 
# in RMSE (root mean squared error) and MAPE (mean absolute percentage error)

# example 
# model_accuracy(flights, 
#                original_price = order_total_spent, 
#                predicted_price = predicted_price)

model_accuracy <- function(tbl, original_price, predicted_price){
  if (!(has_name(tbl, "is_train"))){
    warning("No is_train variable detected, assuming that the data set is entirely the test set.")
    tbl <- tbl %>% 
      mutate(is_train = F)
  }
  
  tbl %>% 
    mutate(rmse = ({{original_price}} - {{predicted_price}})^2,
           mape = abs({{original_price}} - {{predicted_price}})/{{original_price}},
           mae = abs({{original_price}} - {{predicted_price}})) %>% 
    group_by(is_train) %>% 
    summarize(rmse = sqrt(mean(rmse)),
              mae = mean(mae),
              mape = mean(mape)) %>% 
    select(is_train, rmse, mape, mae)
}

# An example with a fairly simplistic linear model
flights_lm <- lm(data = flights %>% filter(is_train),
                 formula = order_total_spent ~ 
                   local_created_hour + local_created_dow + num_of_passengers + 
                   min_days_until_trip + avg_price_per_passenger + visit_length_hours + max_connection_cnt +
                   is_roundtrip + domestic_trip + number_of_flights)

summary(flights_lm)

flights %>% 
  mutate(predicted_price = predict(flights_lm, newdata = flights)) %>% 
  mutate(predicted_price = ifelse(is.na(predicted_price), 0, predicted_price)) %>% 
  model_accuracy(order_total_spent, predicted_price)
