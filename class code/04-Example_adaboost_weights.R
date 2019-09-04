# Example for how the weights change during adaboost.m1 boosting algorithm
# The algorithm is implemented in package adabag, but I want something very specific so implementing it manually.

library(tidyverse)

# read the telco data ----
set.seed(0)
telco_churn <- read_csv("datasets/telco_churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  select(-customerID) %>% 
  mutate(is_train = runif(NROW(gender)) <= 0.8) %>% 
  mutate_if(is.character, as.factor) # randomForest needs factors, doesn't know how to work with characters.

# Implementation using logistic regression models ----
B <- 50 # rounds for boosting

telco_work <- telco_churn %>% 
  filter(is_train) %>% 
  select(-is_train) %>% 
  na.omit() %>% 
  sample_n(1000) # making the data set a bit smaller for faster loop.

current_weights <- rep(1/NROW(telco_work), NROW(telco_work))
err <- NULL
alpha <- NULL
threshold <- 0.5

track_weights <- tibble(iteration1 = current_weights)

# The actual loop ----
pb <- progress_estimated(B)
for (m in 1:B) {
  pb$tick()$print()
  rescale_weights <- NROW(telco_work)/sum(current_weights) # needed for the glm (sum to sample size)
  new_classifier <- glm(formula = Churn ~ ., data = telco_work, family = "binomial", weights = current_weights*rescale_weights)
  new_predictions <- predict(new_classifier, type = "response") > threshold
  real_values <- (telco_work %>% pull(Churn)) == "Yes"
  err[m] <- sum(current_weights*(new_predictions != real_values))
  alpha[m] <- log((1-err[m])/err[m])
  current_weights <- current_weights*exp(alpha[m]*(new_predictions != real_values))
  
  # just to track the weights of the observations:
  track_weights <- track_weights %>% 
    mutate(curr_iteration = current_weights) %>% 
    rename_at(vars(curr_iteration), ~paste0("iteration", m+1))
}

# plot the weights changing:
weights_for_plot <- track_weights %>% 
  mutate(obs_num = seq_along(iteration1)) %>% 
  gather(iteration, weight, -obs_num) %>% 
  filter(obs_num %in% 1:10) %>% 
  mutate(iteration = as.numeric(str_replace(iteration, "iteration", "")))

ggplot(weights_for_plot, aes(x = iteration, y = weight, color = factor(obs_num))) + 
  geom_point() 
