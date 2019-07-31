# Classification exercise ----

# This exercise is based on the files 
# basic_features_balanced_fraud.csv 
# basic_features_balanced_approved.csv

# Each of the files contains transactional data of fraud an of approved deals (50k+50k)
# Our goal is to maximize the AUC using one of the models we talked about in the last lesson.

# You shouldn't use the model_score and the mm_risk_score variables which are model outcome of Riskified's existing models.
# The classification to fraud/legit was performed via these scores.

library(tidyverse)


# Read the data, add the outcome, and split to train/test ----
set.seed(0)
transactions <- read_csv("datasets/local/basic_features_balanced_approved.csv") %>% 
  mutate(is_fraud = 0) %>% 
  select(-ship_zip_state_appearances_range) %>% 
  bind_rows(read_csv("datasets/local/basic_features_balanced_fraud.csv") %>% 
              mutate(is_fraud = 1) %>% 
              select(-ship_zip_state_appearances_range)) %>% 
  mutate(is_train = runif(NROW(.)) <= 0.8) %>% 
  select(-mm_risk_score)

# Remove variables with lots of NAs ----
transactions_dont_include <- transactions %>% 
  summarize_all(~sum(is.na(.))) %>% 
  gather(var, NAs_total) %>% 
  arrange(desc(NAs_total)) %>% 
  filter(NAs_total >= 500) %>% 
  pull(var)

trans_for_model <- transactions %>% 
  select(-one_of(transactions_dont_include))

# A reference glm ----
basic_glm <- glm(formula = is_fraud ~ .,
                 data = trans_for_model %>% 
                   select(-order_id, -model_score) %>% 
                   filter(is_train) %>% 
                   select(-is_train))

transactions_predicted <- trans_for_model %>% 
  mutate(glm_predict = predict(basic_glm, newdata = .))

summary(basic_glm)

# Plot the ROC of the original model score and of the glm ----
roc_chart <- transactions_predicted %>% 
  filter(!is_train) %>% 
  arrange(desc(glm_predict)) %>% 
  mutate(TPR_glm=cumsum(is_fraud)/sum(is_fraud),
         FPR_glm=cumsum(!is_fraud)/sum(!is_fraud)) %>% 
  arrange(model_score) %>% 
  mutate(TPR_riski = cumsum(is_fraud)/sum(is_fraud),
         FPR_riski = cumsum(!is_fraud)/sum(!is_fraud)) %>% 
  select(starts_with("TPR"), starts_with("FPR")) %>% 
  mutate(tmp_key = seq_along(TPR_riski)) %>% 
  gather(type, value, -tmp_key) %>% 
  separate(type, into = c("type", "model")) %>% 
  distinct() %>% 
  spread(type, value)

ggplot(roc_chart, aes(x = FPR, y = TPR, color = model)) + 
  geom_line(size = 1.5) + 
  ylab("TPR (sensitivity)") + 
  xlab("FPR (1-specificity)") + 
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

# Compute area under the curve, compare the Riskified score with the glm ----

AUC <- roc_chart %>% 
  arrange(FPR) %>% 
  group_by(model) %>% 
  mutate(ydx = (FPR - lag(FPR))*TPR) %>% 
  slice(-1) %>% 
  summarize(sum(ydx))

AUC


# The goal is the highest AUC you can. Good luck ----