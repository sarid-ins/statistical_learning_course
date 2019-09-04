# Example for solving an optimization problem (knapsack)

library(adagio)
library(tidyverse)

# Problem definition ----
knapsack_capacity <- 650 # Allotted up to knapsack_capacity miliseconds to complete query
feature_description <- tribble(
  ~time_to_query, ~relative_contribution, ~feature_name,
  300, 0.4, "country_score",
  150, 0.25, "past_history_on_paypal",
  50, 0.15, "past_history_on_amazon",
  500, 0.5, "credit_search",
  350, 0.42, "bank_account",
  550, 0.45, "own_database_query",
  25, 0.1, "ip_rank",
  30, 0.3, "past_history_on_ebay",
  630, 0.7, "email_spam_verification",
  75, 0.2, "zip_code_search"
) %>% 
  mutate(index = seq_along(feature_name))

knapsack_selection <- knapsack(w = feature_description$time_to_query,
                               p = feature_description$relative_contribution,
                               cap = knapsack_capacity)

# sensitivity to knapsack capacity via map ----
knapsack_results <- map_df(.x = seq(650, 1550, by = 100),
       .f = ~{
         knapsack_selection <- knapsack(w = feature_description$time_to_query,
                                        p = feature_description$relative_contribution,
                                        cap = .x)
         feature_description %>% 
           select(feature_name, index) %>% 
           mutate(selected = index %in% knapsack_selection$indices) %>% 
           mutate(used_capacity = knapsack_selection$capacity) %>%
           mutate(allotted_capacity = .x) %>% 
           mutate(profit = knapsack_selection$profit)
       }
)

# plot variables in the blend ----
knapsack_results %>% 
  mutate(facet_title = paste0("capacity (", used_capacity, "/", allotted_capacity,")\nprofit ", profit)) %>% 
  mutate(facet_title = fct_inorder(facet_title)) %>% 
  mutate(feature_name = str_replace_all(feature_name, "_", " ") %>% str_wrap(width = 10) %>% fct_inorder()) %>% 
  ggplot(aes(x = feature_name, y = selected*1)) + 
  geom_col(fill = "lightblue") + 
  facet_grid(rows = vars(facet_title)) + 
  theme_bw() + 
  theme(strip.text.y = element_text(angle = 360),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  scale_y_continuous(labels = NULL) + 
  ylab("") + xlab("Features")

# export table to latex ----
feature_description %>% select(feature_name, time_to_query, relative_contribution) %>% set_names(c("name", "time", "profit")) %>% knitr::kable(format = "latex")
