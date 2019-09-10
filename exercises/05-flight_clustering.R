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


# Read flight data ----
library(tidyverse)
flights <- readr::read_csv("datasets/local/flight_prices.csv") # <- replace location with S3

# glimpse
glimpse(flights)
