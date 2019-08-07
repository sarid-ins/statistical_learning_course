# Live coding example for cross validation for tunning hyper-parameters

library(tidyverse)
library(e1701)

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
split_build_err <- function(k,...){
  build_set <- movies_cls %>% 
    sample_n(size = NROW(success)*(1-1/k))
  validate_set <- movies_cls %>% 
    setdiff(build_set)
  
  svm_model <- svm(formula = success ~ ., data = build_set, ...) # IMPORTANT: See the "pass the dots" use here.
  
  svm_predict <- validate_set %>% 
    mutate(svm_pred = predict(svm_model, newdata = validate_set))
  
  svm_predict %>% 
    mutate(errors = success != svm_pred) %>% 
    count(errors) %>% 
    mutate(err_rate = n/sum(n)) %>% 
    filter(errors) %>% 
    pull(err_rate) # this is what the function will return (the error rate)
  
}

# Example uses for split_build_err:
split_build_err(k=10)
split_build_err(k=10, cost = 5)
split_build_err(k=10, kernel = "polynomial", degree = 3)

# Running the cv with functional programming ----

# Now for using functional programming which will iterate (10 times * ... cost/gamma options) = a lot of iterations
set.seed(0)
pb <- progress_estimated(n = 10*length(cost_cv))
error_results_cost <- crossing(iter = 1:10, cost = cost_cv) %>% 
  mutate(error_rates = map_dbl(iter, ~{
    pb$tick()$print()
    split_build_err(k=10, cost = cost)
    }))

pb <- progress_estimated(n = 10*length(gamma_cv))
error_results_gamma <- crossing(iter = 1:10, gamma = gamma_cv) %>% 
  mutate(error_rates = map_dbl(iter, ~{
    pb$tick()$print()
    split_build_err(k=10, gamma = gamma)
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
         mutate(cost = fct_inorder(as.character(cost))), 
       aes(y = error_rates, group = cost)) + 
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
         group_by(cost) %>% 
         summarize(mean_err = mean(error_rates)), 
       aes(y = mean_err, x = cost)) + 
  geom_point() + 
  geom_line() +
  scale_x_log10()

ggplot(error_results %>% 
         group_by(gamma) %>% 
         summarize(mean_err = mean(error_rates)), 
       aes(y = mean_err, x = gamma)) + 
  geom_point() + 
  geom_line() +
  scale_x_log10()


# These two were not very beneficial, as there is no monotonicity that can be identified.
degree_cv <- 1:15
pb <- progress_estimated(n = 10*length(degree_cv))
error_results_degree <- crossing(iter = 1:10, degree = degree_cv) %>% 
  mutate(error_rates = map_dbl(iter, ~{
    pb$tick()$print()
    split_build_err(k=10, degree = degree, kernel = "polynomial")
  }))

error_results_degree <- read_csv("class code/03-svm-cv-degree-results.csv")

ggplot(error_results_degree, 
       aes(y = error_rates, group = degree)) + 
  geom_boxplot()

ggplot(error_results_degree %>% 
         group_by(degree) %>% 
         summarize(mean_err = mean(error_rates)), 
       aes(y = mean_err, x = degree)) + 
  geom_point() + 
  geom_line()
