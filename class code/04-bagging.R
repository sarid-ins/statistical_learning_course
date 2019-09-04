# Example for bagging
library(tidyverse)

# read the telco data ----
set.seed(0)
telco_churn <- read_csv("datasets/telco_churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  select(-customerID) %>% 
  mutate(is_train = runif(NROW(gender)) <= 0.8) %>% 
  mutate_if(is.character, as.factor) # randomForest needs factors, doesn't know how to work with characters.

# Split the data to two and see the different resulting trees ----
set.seed(0)
telco_sample1 <- telco_churn %>% 
  sample_frac(size = 0.5, replace = F)

telco_sample2 <- telco_churn %>% 
  setdiff(telco_sample1)
  
# make sure no overlap exists ----
intersect(telco_sample1, telco_sample2)

# build a tree for each sub sample ----
# same paramters, different sample. n>3500 for each sample.
tree1 <- rpart::rpart(Churn ~ ., telco_sample1)
tree2 <- rpart::rpart(Churn ~ ., telco_sample2)
rpart.plot::prp(tree1)
rpart.plot::prp(tree2)

# A bagging model (either via ipred or randomForest) ----
# Bagging can be accomplished via the randomForest package, by:
churn_bag <- randomForest::randomForest(formula = Churn ~ . -is_train,
                                        data = telco_churn %>% filter(is_train),
                                        na.action = na.omit,
                                        mtry = 19, # use all predictors, always.
                                        nodesize = 1, # no minimum on node size.
                                        ntree = 100, # keep number of trees small for short runtime.
                                        replace = TRUE, # replacement for proper bootstrap.
                                        importance = TRUE, # calculate variable importance.
                                        sampsize = sum(telco_churn$is_train)) # bootstrap sample size equal to size of train set.

# Analyze the importance of the variables ----
randomForest::importance(churn_bag)
randomForest::varImpPlot(churn_bag)

# For comparison, let's add a logistic regression model, a naive tree, and a random forest

# logistic regression model ----
glm_model <- glm(formula = Churn ~ . - is_train,
                 data = telco_churn %>% filter(is_train),
                 family = "binomial")

# classification tree (rpart) model ----
naive_tree <- rpart::rpart(formula = Churn ~ . - is_train,
                           data = telco_churn %>% filter(is_train), control = rpart::rpart.control(cp = 0.000001))
rpart::plotcp(naive_tree)

# randomForest ----
forest <- randomForest::randomForest(formula = Churn ~ . -is_train,
                                     data = telco_churn %>% filter(is_train),
                                     na.action = na.omit,
                                     ntree = 500)

# ROC and AUC ----
churn_pred_bag <- telco_churn %>% 
  filter(!is_train) %>% 
  mutate(yes_bag = predict(churn_bag, newdata = ., type = "prob")[,2]) %>%
  mutate(yes_logistic = predict(glm_model, newdata = ., type = "response")) %>% 
  mutate(yes_tree = predict(naive_tree, newdata = ., type = "prob")[,2]) %>% 
  mutate(yes_forest = predict(forest, newdata = ., type = "prob")[,2]) %>% 
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>% 
  select(Churn, starts_with("yes_")) %>% 
  gather(model, prediction, -Churn) %>% 
  mutate(model = str_replace(model, "yes_", "")) %>% 
  group_by(model)

# Let's make use of a ready-made package, for a change plotROC ----
library(plotROC)
# See this post for a review. I chose this package since it uses ggplot2
# https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# Any other package can also do the work. Also see plotROC::shiny_plotROC() for a nice interactive app

bagging_roc <- ggplot(churn_pred_bag, aes(m = prediction, d = Churn, color = model)) + 
  geom_roc(n.cuts = 0) + 
  theme_bw()

calc_auc(bagging_roc)

bagging_roc
