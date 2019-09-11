# Outlier detection with LOF and iForests

library(tidyverse)


# Sometimes a boxplot is just not enough ----------------------------------

example1 <- tibble(x = runif(100, 0, 1), y = runif(100, 1, 2)) %>% 
  bind_rows(tibble(x = runif(100, 1, 2), y = runif(100, 0, 1))) %>% 
  add_case(x = 0.5, y = 0.5) 

example_plot1 <- ggplot(example1, aes(x, y)) + 
  geom_point() + 
  theme_bw()

example_plot1 %>% ggExtra::ggMarginal(type = "boxplot", fill = "lightblue")


# Local Outlier Factor ----------------------------------------------------

library(dbscan)

example_lof <- lof(example1)

example2 <- example1 %>% 
  mutate(lof = example_lof)

ggplot(example2, aes(x, y)) + 
  geom_point(size = 1) +
  geom_point(data = filter(example2, lof > 1), inherit.aes = F,
             aes(x, y, size = lof), shape = 21, color = "red") + 
  theme_bw()

ggplot(example2, aes(lof)) + 
  geom_histogram(fill = "lightblue", color = "red", size = 0.5) + 
  theme_bw()


# Example of LOF on the alchohol consumption data -------------------------

alchohol <- read_csv("datasets/alchohol_consumption/alchohol_consumption_ready.csv") %>% 
  mutate(asia = ifelse(Continent_Name == "Asia", 1, 0),
         europe = ifelse(Continent_Name == "Europe", 1, 0),
         south_america = ifelse(Continent_Name == "South America", 1, 0),
         north_america = ifelse(Continent_Name == "North America", 1, 0),
         africa = ifelse(Continent_Name == "Africa", 1, 0)) %>% 
  select(ends_with("servings"), Country_Name) %>% 
  distinct(Country_Name, .keep_all = T) %>% 
  mutate(lof = lof(select(., ends_with("_servings")), k = 10)) %>% 
  arrange(desc(lof))

alchohol_plot <- ggplot(alchohol, aes(x = beer_servings, y = spirit_servings, color = wine_servings)) + 
  geom_point() + 
  theme_bw()

# detects some non-trivial outliers
alchohol_plot + 
  geom_point(data = alchohol %>% filter(lof > 1.5), 
             aes(size = lof), shape = 21, color = "red")

# in zoom-in for a better illustration
alchohol_plot + 
  geom_point(data = alchohol %>% filter(lof > 1.5), 
             aes(size = lof), shape = 21, color = "red") + 
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 25))


# Using rayshader to illustrate height (just for fun) ---------------------

library(rayshader)

plot_gg(alchohol_plot, height_aes = "color", raytrace = F, pointcontract = 1)

