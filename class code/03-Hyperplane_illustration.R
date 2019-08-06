# This illustrates what the margin separator does

library(tidyverse)

our_tibble <- tibble(x1 = c(5, 2.5, 2.5), x2 = c(4, 4, 7))

ggplot(data = our_tibble) + 
  geom_point(aes(x1, x2), size = 3, color = "red") + 
  geom_abline(slope = 2, intercept = -1) + 
  coord_equal(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_bw()

# Note that our "linear separator" is the vector \beta = (-1, 2, -1), 
# i.e., -1 + 2x_1 -x_2 = 0
# In the normalized version that would be:
original_beta <- c(-1, 2, -1)
normalized_beta <- c(-1, 2, -1)/sqrt(sum(c(-1, 2, -1)^2))

# check norm=1
sum(normalized_beta^2)

# The projection's distance of our points to our hyperplan would be:
colSums(normalized_beta*t(cbind(1, our_tibble)))

# You can see that the first point (under our hyperplane) has a value of 2.04
# The second point (which was chosen on the hyperplane) has a value of 0 (up to percision error)
# The third point is slightly closer to the hyperplane but from the "other side" compared to the first point, 
#    accordingly it has a value of -1.2


# The plot with the projections
our_new_tibble <- our_tibble %>% 
  bind_rows(tibble(x1 = c(3, 3.7), x2 = c(5, 6.4)))

ggplot(data = our_new_tibble) + 
  geom_point(aes(x1, x2), size = 3, color = "red") + 
  geom_abline(slope = 2, intercept = -1) + 
  geom_abline(slope = -0.5, intercept = 6.5, color = "blue") +
  geom_abline(slope = -0.5, intercept = 8.25, color = "blue") +
  coord_equal(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_bw()

colSums(normalized_beta*t(cbind(1, our_new_tibble)))

# To find the projections use the perpendicular to the hyperplane (N=2i-j),
# Set (x_1-5)/2=t; (x_2-4)/(-1)=t
# Derives: x_1 = 2t+5; x_2 = 4-t
# Set the condition of being in the hyperplane: 4-t=2*(2t+5)-1
# t=-1; x_1=3, x_2=6.
# In a similar manner for the rest.
# To get the perpendicular line, we know the slope is -1/2, and set the point to get the intercept.