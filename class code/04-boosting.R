# Example for boosting
library(tidyverse)

# read and prepare the data ----
set.seed(0)
telco_churn <- read_csv("datasets/telco_churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  select(-customerID) %>% 
  mutate(is_train = runif(NROW(gender)) <= 0.8) %>% 
  mutate_if(is.character, as.factor) %>% 
  na.omit()

telco_churn_for_boost <- telco_churn %>% 
  mutate_at(vars(Partner, Dependents, PhoneService, MultipleLines, 
                 OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport,
                 StreamingTV, StreamingMovies, PaperlessBilling, Churn),
            ~ifelse(. == "Yes", 1, 0)) %>% 
  mutate(is_male = gender == "Male",
         internet_dsl = 1*(InternetService == "DSL"),
         internet_fiber = 1*(InternetService == "Fiber optic"),
         payment_auto = 1*str_detect(PaymentMethod, "automatic"),
         contract_monthly = 1*(Contract == "Month-to-month")) %>% 
  select_if(~!is.factor(.))

# Some prepping related to xgboost ----
library(xgboost)
churn_train <- xgb.DMatrix(telco_churn_for_boost %>% 
                             filter(is_train) %>% 
                             select(-is_train, -Churn) %>% 
                             as.matrix(),
                           label = telco_churn_for_boost %>% 
                             filter(is_train) %>% 
                             pull(Churn))
churn_test <- xgb.DMatrix(telco_churn_for_boost %>% 
                            filter(!is_train) %>% 
                            select(-is_train, -Churn) %>% 
                            as.matrix(),
                          label = telco_churn_for_boost %>% 
                            filter(!is_train) %>% 
                            pull(Churn))

# note that with large data sets it is better to work with a sparse matrix representation, i.e.:
# only values which are non-zero are retained in memory
Matrix::sparse.model.matrix(Churn ~ .,
                            data = telco_churn_for_boost %>%
                              filter(is_train) %>%
                              select(-is_train))

# Build the xgboost model ----

churn_boost <- xgboost(data = churn_train, nrounds = 15, objective = "binary:logistic")

# I'm adding these predictions to what we did in 04-bagging example
churn_boost_pred <- churn_pred_bag %>% 
  mutate(Churn = 1*(Churn == "Yes")) %>% 
  bind_rows(
    telco_churn_for_boost %>% 
      filter(!is_train) %>% 
      mutate(prediction = predict(churn_boost, newdata = churn_test),
             model = "boosting") %>% 
      select(Churn, model, prediction)
  )

# plot ROC and compute the AUC
boost_roc <- ggplot(churn_boost_pred, aes(m = prediction, d = Churn, color = model)) + 
  geom_roc(n.cuts = 0) + 
  theme_bw()

calc_auc(boost_roc)

boost_roc
