# Demonstration of using svm for classification

library(tidyverse)

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

# A function to generate a confusion matrix ----
compute_confusion <- function(tbl, real_pr, predicted_pr, threshold, show_prop = F){
  abs_tbl <- tbl %>% 
    mutate(predicted_class = {{predicted_pr}} >= threshold) %>% 
    count({{real_pr}}, predicted_class) %>% 
    select({{real_pr}}, predicted_class, n)
  
  if (show_prop) {
    res_confuse <- abs_tbl %>% 
      group_by({{real_pr}}) %>% 
      mutate(prop = n/sum(n)) %>% 
      select(-n) %>% 
      spread({{real_pr}}, prop) %>% 
      rename(`predicted|true->` = predicted_class)
  } else {
    res_confuse <- abs_tbl %>% 
      spread({{real_pr}}, n) %>% 
      rename(`predicted|actual->` = predicted_class)
  }
  
  return(res_confuse)
  
}

# Run our reference logistic regression model ----
# We'll use this model later on to compare it to the svm.
set.seed(0)
movies_cls <- movies %>% 
  select(-movie_title, -gross, -earn_ratio) %>% 
  mutate(success = factor(success)) %>% 
  na.omit() %>% 
  mutate(is_train = runif(NROW(title_year)) < 0.8) %>% 
  select(-contains("Noir"), -contains("Game-Show"), -contains("Reality"))

# Note the exclusion of Film Noir, Game-Show and Reality-TV.
# They contain a single value and svm is not very robust, it simply fails if they are left as-is.

glm_fit <- glm(success ~ .,
               data = movies_cls %>% filter(is_train),
               family = binomial(link = "logit"))


# An svm model ----
# SVM's in R are implemented in package e1701
# (Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071))

#install.packages("e1071")
library(e1071)

svm_rad <- svm(formula = success ~ .,
               data = movies_cls %>% filter(is_train))
summary(svm_rad)

# This is an exremely flexible (and complex function).
# You can choose various kernel functions (default is radial)
# Each one comes with its own set of parameters
svm_pol <- svm(formula = success ~ .,
               data = movies_cls %>% filter(is_train),
               kernel = "polynomial", degree = 5)
summary(svm_pol)

# You can have it run its own cross validation ----
svm(formula = success ~ .,
    data = movies_cls %>% filter(is_train),
    kernel = "radial",
    gamma = 1/10,
    cross = 10) %>% summary()

svm(formula = success ~ .,
    data = movies_cls %>% filter(is_train),
    kernel = "polynomial", degree = 5,
    cross = 10) %>% 
  summary()

# You can control the bias-variance of the model using the cost parameter ----
# Just be sure you understand what is the cost (not the same as C in ISLR, but rather C in ESLII). 
# cost is a regularization parameter in the objective, meaning that higher C = less violations (lower bias, higher variance)
svm(formula = success ~ .,
    data = movies_cls %>% filter(is_train),
    kernel = "radial",
    gamma = 1/10,
    cost = 10,
    cross = 10) %>% 
  summary()

# Making predictions with svm ----
# Simply using "predict" will give you classes.
# To generate an ROC, use decision.values = TRUE

svm_rad_pred <- predict(svm_rad, newdata = movies_cls, decision.values = TRUE)
svm_pol_pred <- predict(svm_pol, newdata = movies_cls, decision.values = TRUE)

movies_pred <- movies_cls %>% 
  mutate(success = success == "TRUE") %>% 
  mutate(svm_rad_pred_dvals = attr(svm_rad_pred, "decision.values"),
         svm_rad_pred = svm_rad_pred == "TRUE") %>%
  mutate(svm_pol_pred_dvals = attr(svm_pol_pred, "decision.values")) %>% 
  mutate(glm_pred = predict(glm_fit, newdata = movies_cls))

svm_confusion_mat <- compute_confusion(movies_pred %>% filter(!is_train), success, svm_rad_pred, threshold = 0.5, show_prop = TRUE)
glm_confusion_mat <- compute_confusion(movies_pred %>% filter(!is_train), success, glm_pred, threshold = 0.5, show_prop = TRUE)

roc_chart <- movies_pred %>% 
  select(success, glm_pred, svm_rad_pred_dvals, svm_pol_pred_dvals, is_train) %>% 
  group_by(is_train) %>% 
  arrange(desc(glm_pred)) %>% 
  mutate(TPR_glm=cumsum(success)/sum(success),
         FPR_glm=cumsum(!success)/sum(!success)) %>% 
  arrange(as.numeric(svm_rad_pred_dvals)) %>% 
  mutate(TPR_svmrad=cumsum(success)/sum(success),
         FPR_svmrad=cumsum(!success)/sum(!success)) %>% 
  arrange(as.numeric(svm_pol_pred_dvals)) %>% 
  mutate(TPR_svmpol=cumsum(success)/sum(success),
         FPR_svmpol=cumsum(!success)/sum(!success)) %>% 
  mutate(key = seq_along(success)) %>% 
  select(key, starts_with("TPR"), starts_with("FPR"), is_train) %>% 
  gather(type, value, -key, -is_train) %>% 
  separate(type, into = c("type", "model"), sep = "_") %>% 
  spread(type, value) %>% 
  arrange(TPR, FPR)
  
# See how the ROC compares, facets by train/test sets ----

ggplot(roc_chart, aes(x = FPR, y = TPR, color = model)) + 
  geom_line() + 
  ylab("TPR (sensitivity)") + 
  xlab("FPR (1-specificity)") + 
  theme_bw() + 
  facet_wrap(~is_train)
