# A random forest example ----

library(tidyverse)
library(randomForest)
library(plotROC)

# In this exercise we are going to use the out of bag error and ROC/AUC measures to compare various approaches of random forests

set.seed(0)
telco_churn <- read_csv("datasets/telco_churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  select(-customerID) %>% 
  mutate(is_train = runif(NROW(gender)) <= 0.8) %>% 
  mutate_if(is.character, as.factor) %>% # randomForest needs factors, doesn't know how to work with characters.
  na.omit()

# Few trees ----
small_forest <- randomForest(
  formula = Churn ~ . - is_train,
  data = telco_churn %>% filter(is_train),
  na.action = na.omit,
  ntree = 10
)
# Many trees ----
large_forest <- randomForest(
  formula = Churn ~ . - is_train,
  data = telco_churn %>% filter(is_train),
  na.action = na.omit,
  ntree = 500
)

# Smaller trees ----
small_trees <- randomForest(
  formula = Churn ~ . - is_train,
  data = telco_churn %>% filter(is_train),
  na.action = na.omit,
  ntree = 100,
  maxnodes = 3
)
# Larger trees ----
large_trees <- randomForest(
  formula = Churn ~ . - is_train,
  data = telco_churn %>% filter(is_train),
  na.action = na.omit,
  ntree = 100,
  maxnodes = 10
)

# More variables considered ----
bigger_mtry <- randomForest(
  formula = Churn ~ . - is_train,
  data = telco_churn %>% filter(is_train),
  na.action = na.omit,
  ntree = 100,
  mtry = 15
)

# Examine the out of bag error and confusion matrices ----
small_forest
large_forest
small_trees
large_trees
bigger_mtry

# Examine the resulting ROC and AUC using the test set ----
churn_pred_forest <- telco_churn %>% 
  filter(!is_train) %>% 
  mutate(small_forest_p = predict(small_forest, newdata = ., type = "prob")[,2]) %>%
  mutate(large_forest_p = predict(large_forest, newdata = ., type = "prob")[,2]) %>%
  mutate(small_trees_p = predict(small_trees, newdata = ., type = "prob")[,2]) %>%
  mutate(large_trees_p = predict(large_trees, newdata = ., type = "prob")[,2]) %>%
  mutate(bigger_mtry_p = predict(bigger_mtry, newdata = ., type = "prob")[,2]) %>%
  select(Churn, ends_with("_p")) %>% 
  gather(model, prediction, -Churn) %>% 
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>% 
  group_by(model)

# Plot and comutations ----
forest_roc <- ggplot(churn_pred_forest, aes(m = prediction, d = Churn, color = model)) + 
  geom_roc(n.cuts = 0) + 
  theme_bw()

calc_auc(forest_roc)

forest_roc