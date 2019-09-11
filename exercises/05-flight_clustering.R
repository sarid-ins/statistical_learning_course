# Flight clustering exercise

# The airline is making changes in its marketing department. 
# You are requested to suggest a clustering (to flight types) 
# according to your clustering, marketing sub-units will be established and put in charge of.

# You were instructed to just make sure that the clusters are more-or-less the same size.
# You can also suggest sub-units which will manage more than one cluster.

# The number of sub-units in the marketing departmenr should not increase 10.

# You can (and should) enginner the features however you think necessary.
# In your presentation, you must describe your clusters in a resonable way. 
# (verbally, and in a manner that makes sense to managers).

# In your answers, please relate to the following topics:
# 1. What feature engineering did you do? how did you decide?
# 2. What distance function(s) did you use? why?
# 3. If you used a link function, what kind and why?
# 4. How did you determine the number of clusters, k? Did you examine the sensitivity to your choice?
# 5. What algorithms did you use? in what packages? how did you choose them?
# 6. Find proper ways to visualize your methodolgy and your results. Explain them.


# Read flight data ----
library(tidyverse)
flights <- readr::read_csv("datasets/local/flight_prices.csv") # <- replace location with S3

# glimpse
glimpse(flights)
