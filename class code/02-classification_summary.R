library(tidyverse)

# What method would you use in each of the following scenarios? ----

# Identical covariance between the two sets. Means differ. ----
# (LDA)
set.seed(0)
dataset1 <- tibble(x1 = rnorm(100, mean = 5.5, sd = 1),
                   x2 = rnorm(100, mean = -3, sd = 1.6),
                   classification = T) %>% 
  bind_rows(tibble(x1 = rnorm(100, mean = 3, sd = 1),
                   x2 = rnorm(100, mean = -1.5, sd = 1.6),
                   classification = F))

ggplot(dataset1, aes(x = x1, y = x2, color = classification)) + 
  geom_point() + 
  theme_bw()

# Varying covariance between the two sets. Means differ. ----
# (QDA)
set.seed(0)
dataset2 <- tibble(x1 = rnorm(100, mean = 5.5, sd = 1),
                   x2 = rnorm(100, mean = -3, sd = 1.6),
                   classification = T) %>% 
  bind_rows(tibble(x1 = rnorm(100, mean = 4, sd = 0.3),
                   x2 = rnorm(100, mean = -1.5, sd = 3),
                   classification = F))

ggplot(dataset2, aes(x = x1, y = x2, color = classification)) + 
  geom_point() + 
  theme_bw()

# More overlapping data ("less" separable), distribution is not normal ----
# (Logistic regression)

regressor_cls <- function(data){
  data %>% 
    mutate(prob = exp(0.3 + 0.2*x1 + 0.2*x2)/(1 + exp(0.3 + 0.2*x1 + 0.2*x2)))
}

set.seed(0)

dataset3 <- tibble(x1 = rnorm(50, mean = 4, sd = 0.7),
                   x2 = -3.4 + runif(50, min = -0.5, max = 0.5)) %>% 
  bind_rows(tibble(x1 = rnorm(50, mean = 4, sd = 0.7),
                   x2 = -2.7 + runif(50, min = -0.5, max = 0.5))) %>% 
  bind_rows(tibble(x1 = rnorm(100, mean = 4, sd = 0.3),
                   x2 = rnorm(100, mean = -1.5, sd = 3))) %>% 
  regressor_cls() %>% 
  mutate(classification = prob >= 0.65-runif(200, 0, 0.1))

ggplot(dataset3, aes(x = x1, y = x2, color = classification)) + 
  geom_point() + 
  theme_bw()


# compare ROC of all methods in each of the data sets ----
generate_roc <- function(dataset){
  glm_fit <- glm(formula = classification ~ .,
                  data = dataset)
  lda_fit <- MASS::lda(formula = classification ~ .,
                        data = dataset)
  qda_fit <- MASS::qda(formula = classification ~ .,
                        data = dataset)
  
  roc_charts <- dataset %>%
    mutate(
      glm_pred = predict(glm_fit, type = "response"),
      lda_pred = predict(lda_fit)$posterior[, 2],
      qda_pred = predict(qda_fit)$posterior[, 2]
    ) %>%
    arrange(desc(glm_pred)) %>%
    mutate(
      TPR_glm = cumsum(classification) / sum(classification),
      FPR_glm = cumsum(!classification) / sum(!classification)
    ) %>%
    arrange(desc(lda_pred)) %>%
    mutate(
      TPR_lda = cumsum(classification) / sum(classification),
      FPR_lda = cumsum(!classification) / sum(!classification)
    ) %>%
    arrange(desc(qda_pred)) %>%
    mutate(
      TPR_qda = cumsum(classification) / sum(classification),
      FPR_qda = cumsum(!classification) / sum(!classification)
    ) %>%
    mutate(id = seq_along(x1)) %>%
    select(starts_with("TPR"), starts_with("FPR"), id) %>%
    gather(type_axis_method, value, -id) %>%
    separate(type_axis_method,
             into = c("axis", "method"),
             sep = "_") %>%
    spread(axis, value)
  
  ggplot(roc_charts, aes(x = FPR, y = TPR, color = method)) + 
    geom_step() + 
    theme_bw()
}

generate_roc(dataset1)
generate_roc(dataset2)
generate_roc(dataset3)
