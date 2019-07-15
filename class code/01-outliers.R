# How many outliers quiz ----
library(tidyverse)

# misleading boxplots ----

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


# regular outlier detectable via boxplot ----
ggplot(tibble(x = c(rnorm(30, 0, 1), 5)), aes(y = x, group = "")) + 
  geom_boxplot(fill = "lightblue") +
  theme_bw() + 
  theme(axis.text.y = element_blank()) + 
  coord_flip()


# How can linear regression go wrong with one single outlier ----

iris_setosa <- iris %>%
  filter(Species == "setosa") %>%
  mutate(Sepal.Length = jitter(Sepal.Length))

iris_setosa_outlier <- iris_setosa %>%
  add_row(Sepal.Length = 5.1, Sepal.Width = 35, Petal.Length = 1.4, Petal.Width = 0.2, Species = "setosa")

ggplot(iris_setosa_outlier, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = TRUE,
              linetype = "dashed") + 
  stat_smooth(inherit.aes = FALSE,
              method = "lm", 
              data = iris_setosa, aes(x = Sepal.Length, y = Sepal.Width), 
              se = TRUE) + 
  theme_bw() +
  coord_cartesian(ylim = c(2, 4.5))




# What would happens when we apply the quantile regression? (**QUANTILE REGRESSION EXAMPLE**) ----
#install.packages("quantreg")
library(quantreg)
quantreg_model <- rq(formula = Sepal.Width ~ Sepal.Length, data = iris_setosa_outlier)

summary(quantreg_model, se = "boot")

ggplot(iris_setosa_outlier, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE,
              linetype = "dashed", color = "darkred") + 
  stat_smooth(method = "rq", se = FALSE,
              linetype = "solid", color = "darkgreen") + 
  stat_smooth(inherit.aes = FALSE,
              method = "lm", 
              data = iris_setosa, aes(x = Sepal.Length, y = Sepal.Width), 
              se = FALSE) + 
  theme_bw() +
  coord_cartesian(ylim = c(2, 4.5)) 
