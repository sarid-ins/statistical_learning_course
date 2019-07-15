# How many outliers quiz ----
library(tidyverse)

set.seed(0)
outlier_tib <- tibble(x1 = runif(100, 1, 2), x2 = runif(100, 0, 1)) %>% 
  bind_rows(tibble(x1 = runif(100, 0, 1), x2 = runif(100, 1, 2))) %>% 
  bind_rows(tibble(x1 = c(0.5, 1.5), x2 = c(0.5, 1.5))) 
  

ggplot(outlier_tib %>% 
         gather(x, val), 
       aes(y = val, x = x)) + 
  geom_boxplot(fill = "lightblue") + theme_bw()


ggplot(outlier_tib, aes(x = x1, y = x2)) + 
  geom_point() + 
  theme_bw()
