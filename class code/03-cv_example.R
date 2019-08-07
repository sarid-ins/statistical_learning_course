# Live coding example for cross validation for tunning hyper-parameters

library(tidyverse)
library(e1071)

# We're going to do this example manually for educational purposes,
# But note that there are functions which can do this for you.
# tidyverse provides a lot of pre made tools within packages such as `parsnip` and `broom`

# Read the movies data ----

movies_raw <- read_csv("https://raw.githubusercontent.com/sarid-ins/statistical_learning_course/master/datasets/scraped_imdb/movie_db_clean.csv", col_types = cols())

movies <- movies_raw %>% 
  mutate(earn_ratio = gross/budget) %>% 
  mutate(success = earn_ratio >= 2.5) %>% 
  add_count(country, name = "country_count") %>% 
  add_count(language, name = "language_count") %>% 
  select(movie_title, title_year,
         duration, aspect_ratio, country_count, language_count,
         director_facebook_likes,
         actor_1_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes,
         Action:Western,
         gross, budget, earn_ratio, success) %>% 
  filter(!is.na(title_year))

movies_cls <- movies %>% 
  select(-movie_title, -gross, -earn_ratio) %>% 
  mutate(success = factor(success)) %>% 
  na.omit() %>% 
  select(-contains("Noir"), -contains("Game-Show"), -contains("Reality")) %>% 
  distinct()



# This is the hyper parameter we'll be tuning (svm's cost) ----

cost_cv <- 10^seq(-3,4, by = 0.33)
gamma_cv <- 10^seq(-4,3, by = 0.33)

# Define the function to split the data, build the model, and return the miss classification error
split_build_err <- function(data, k, class, ...){
  
  formula_svm <- as.formula(paste0(ensym(class), "~."))
  
  build_set <- data %>% 
    sample_n(size = NROW(data)*(1-1/k))
  validate_set <- data %>% 
    setdiff(build_set)
  
  svm_model <- svm(formula = formula_svm, data = build_set, ...) # IMPORTANT: See the "pass the dots" use here.
  
  svm_predict <- validate_set %>% 
    mutate(svm_pred = predict(svm_model, newdata = validate_set))
  
  svm_predict %>% 
    mutate(errors = {{class}} != svm_pred) %>% 
    count(errors) %>% 
    mutate(err_rate = n/sum(n)) %>% 
    filter(errors) %>% 
    pull(err_rate) # this is what the function will return (the error rate)
  
}

# Example uses for split_build_err:
split_build_err(movies_cls, k=10, class = success)
split_build_err(movies_cls, k=10, class = success, cost = 5)
split_build_err(movies_cls, k=10, class = success, kernel = "polynomial", degree = 3)

# Running the cv with functional programming ----

# Now for using functional programming which will iterate (10 times * ... cost/gamma options) = a lot of iterations
set.seed(0)
pb <- progress_estimated(n = 10*length(cost_cv))
error_results_cost <- crossing(iter = 1:10, cost = cost_cv) %>% 
  mutate(error_rates = map_dbl(iter, ~{
    pb$tick()$print()
    split_build_err(movies_cls, k=10, class = success, cost = cost)
    }))

pb <- progress_estimated(n = 10*length(gamma_cv))
error_results_gamma <- crossing(iter = 1:10, gamma = gamma_cv) %>% 
  mutate(error_rates = map_dbl(iter, ~{
    pb$tick()$print()
    split_build_err(movies_cls, k=10, class = success, gamma = gamma)
  }))

error_results <- bind_rows(error_results_cost,
                           error_results_gamma)

# This takes ~16 minutes to complete, so I prepared the results in advance
#write_csv(error_results, "class code/03-svm-cv-results.csv")
error_results <- read_csv("class code/03-svm-cv-results.csv")

# Not much of a discerning difference
ggplot(error_results %>% 
         arrange(cost) %>% 
         filter(!is.na(cost)) %>% 
         mutate(cost = fct_inorder(as.character(round(cost, 2)))), 
       aes(y = error_rates, x = cost)) + 
  geom_boxplot()

# Indicates a small advantage to gamma around 100
ggplot(error_results %>% 
         arrange(gamma) %>% 
         filter(!is.na(gamma)) %>% 
         mutate(gamma = fct_inorder(as.character(gamma))), 
       aes(y = error_rates, x = gamma)) + 
  geom_boxplot()

# As a line
ggplot(error_results %>%
         filter(cost <= 10) %>% 
         group_by(cost) %>% 
         summarize(mean_err = mean(error_rates)), 
       aes(y = mean_err, x = cost)) + 
  geom_point() + 
  geom_line() +
  scale_x_log10() + 
  theme_bw() +
  ggtitle("Quiz question - Error as a function of svm penalty cost")

ggplot(error_results %>% 
         group_by(cost) %>% 
         summarize(mean_err = mean(error_rates)), 
       aes(y = mean_err, x = cost)) + 
  geom_point() + 
  geom_line() +
  scale_x_log10() + 
  theme_bw() +
  ggtitle("Quiz question - Error as a function of svm penalty cost (2)")

ggplot(error_results %>% 
         group_by(gamma) %>% 
         summarize(mean_err = mean(error_rates)), 
       aes(y = mean_err, x = gamma)) + 
  geom_point() + 
  geom_line() +
  scale_x_log10() + 
  theme_bw()


# These two were not very beneficial, as there is no monotonicity that can be identified.
# Here is an example for a k-fold cv on a regression problem, to determine the penalty lambda of a lasso regression

library(glmnet)
set.seed(0)
test_lambda <- 10^(seq(-3, 4, by = 0.5))
pb <- progress_estimated(n = 10*length(test_lambda))

error_results_cost_boston <- crossing(iter = 1:10, lambda = test_lambda) %>% 
  mutate(error_rates = map_dbl(lambda, ~{
    pb$tick()$print()
    
    build_set <- MASS::Boston %>% 
      sample_n(size = NROW(MASS::Boston)*(1-1/k))
    validate_set <- MASS::Boston %>% 
      setdiff(build_set)
    
    glmnet_model <- glmnet(x = as.matrix(build_set %>% 
                                           select(-medv)),
                           y = as.matrix(build_set %>% select(medv)),
                           alpha = 1,
                           lambda = .)
    
    validate_set %>% 
      mutate(pred = as.numeric(predict(glmnet_model, newx = as.matrix(validate_set %>% select(-medv))))) %>% 
      mutate(rss = (pred-medv)^2) %>% 
      summarize(mse = mean(rss)) %>% 
      pull(mse) # this is what the function will return (the error rate)
    
  }))

error_results_cost_boston %>% 
  group_by(lambda) %>% 
  summarize(mean_err = mean(error_rates)) %>% 
  ggplot(aes(y = mean_err, x = lambda)) + 
  geom_smooth() + 
  geom_line() + 
  geom_point() +
  ylab("MSE") + 
  xlab("Lambda") +
  scale_x_log10() + 
  theme_bw()

error_results_cost_boston %>% 
  mutate(lambda = fct_inorder(as.character(round(log(lambda), 2)))) %>% 
  ggplot(aes(y = error_rates, x = lambda)) + 
  geom_boxplot() +
  ylab("MSE") + 
  xlab("Lambda") +
  theme_bw()


# As I said, this was for educational purposes, in most cases, the function will also provide a cv interface,
# as we've already seen in glmnet
# The implementation of build-in cv functions has potencial of being much quicker than something you build manually

boston_prices_cv <- cv.glmnet(MASS::Boston %>% select(-medv) %>% as.matrix(),
                              MASS::Boston %>% select(medv) %>% as.matrix(),
                              lambda = test_lambda,
                              alpha = 1)
plot(boston_prices_cv)
