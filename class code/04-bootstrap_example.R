# Usage of bootstrap - example for computing the distribution of a median

library(tidyverse)
library(rsample)

movie_data <- read_csv("datasets/scraped_imdb/movie_db_clean.csv")

movie_boot <- bootstraps(movie_data, times = 200)

boot_median <- movie_boot %>% 
  mutate(facebook_likes_median = 
           map_dbl(splits,
                   ~{
                     median(as.data.frame(.x)$movie_facebook_likes, na.rm = T)
                     }))

# Now we can extract and compare some statistics
median(movie_data$movie_facebook_likes, na.rm = T)
median(boot_median$facebook_likes_median)
mean(boot_median$facebook_likes_median)

# A confidence interval for the median is
ci_comp <- boot_median %>% 
  mutate(bottom2.5 = facebook_likes_median <= quantile(facebook_likes_median, 0.025),
         top2.5 = facebook_likes_median >= quantile(facebook_likes_median, 0.975)) %>% 
  filter(!bottom2.5 & !top2.5) %>% 
  summarize(lb = min(facebook_likes_median),
            ub = max(facebook_likes_median))

# The distribution of the median looks like this:
ggplot(boot_median, aes(x = facebook_likes_median, y = stat(density))) + 
  geom_histogram(bins = 10, fill = "lightblue") + 
  geom_density(size = 1.5) + 
  geom_vline(xintercept = ci_comp$lb, color = "red", size = 1) + 
  geom_vline(xintercept = ci_comp$ub, color = "red", size = 1)
