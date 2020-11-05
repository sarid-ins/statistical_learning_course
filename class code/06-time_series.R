# Time series analysis

library(tidyverse)
library(tsibble)
library(fable)

# Example: creating a tsibble from structured data ------------------------

# Let's see how many movies are issued per month 
# We first read the movies imdb scraped data

movies <- read_csv("datasets/scraped_imdb/movie_db_clean.csv")

movies_ts <- movies %>% 
  filter(!is.na(title_year)) %>% 
  count(title_year) %>% 
  as_tsibble(key = NULL, index = title_year, regular = FALSE)

# When we use the regular = TRUE flag, tsibble "understands" that the data is given in yearly intervals

autoplot(movies_ts, .vars = n)

movies_ts

# Stop to think for a moment, what is the problem with this plot? 
# (think about the "edges" of the plot)

# Preprocessing series into a regular form with complete data  -------------

# Some times, you want to fill-in the gaps, to make sure you get an evenly spaced time series:

movies_ts_regular <- movies %>% 
  filter(!is.na(title_year)) %>% 
  count(title_year) %>% 
  as_tsibble(key = NULL, index = title_year, regular = T) %>% 
  fill_gaps(n = 0)


# Filter out the last year, which has missing data (partial year) ---------

movies_ts <- movies %>% 
  filter(!is.na(title_year)) %>%
  filter(title_year < 2016) %>% 
  count(title_year) %>% 
  as_tsibble(key = NULL, index = title_year, regular = TRUE)

autoplot(movies_ts, .vars = n)


# The Zombie/pool search example ------------------------------------------

# Now let's try to read a "more interesting" time series, one with more obvious patterns

search <- read_csv("datasets/time_series/zombieTimeline.csv", skip = 3, 
                   col_names = c("month", "google_search_volume")) %>% 
  mutate(term = "zombie") %>% 
  bind_rows(
    read_csv("datasets/time_series/swimmingpoolTimeline.csv", skip = 3, 
             col_names = c("month", "google_search_volume")) %>% 
      mutate(term = "pool")
  ) %>% 
  mutate(month = yearmonth(month)) %>% # vice verse using mutate(month = lubridate::ymd(paste0(month, "-01"))) but is suboptimal
  as_tsibble(key = term, index = month, regular = TRUE)

autoplot(search, .vars = google_search_volume, size = 1.5) + 
  facet_wrap(~term)

# The use of the yearmonth() function is important to get the right seasonality!
# We will see it come into affect later on.
# Other seasonality patterns can be captured with yearquarter(), yearweek()
# Usually the as_tibble is able to capture "standard" seasonality patterns.
# More complex seasonality patterns can be achieved with lubridate::period()

# Now that we have the two tsibbles (search; movies) we can learn about visualizations

# *** Go back to presentation, slide 9 ***

# Charting tools ----------------------------------------------------------
library(feasts)

# Seasonal plot
search %>% 
  gg_season(y = google_search_volume) + 
  ggtitle("Example for a season plot")

# Subseries plot
search %>% 
  gg_subseries(y = google_search_volume) + 
  ggtitle("Example for a subseries plot")

# Lag plots
search %>% 
  filter(term == "pool") %>% 
  gg_lag(geom = "point") + 
  ggtitle("Example for a lag-plot (a single series)")

# Autocorrelation function

search %>% 
  ACF(google_search_volume, type = "correlation") %>% 
  autoplot() + 
  ggtitle("ACF for google search data")

# *** This will fail because the series is not "filled-in": ***
movies_ts %>% 
  ACF(n)

# This will work properly

movies_ts_regular %>% 
  ACF(n) %>% 
  autoplot() +
  ggtitle("ACF for movies data")

# *** Go back to presentation, slide 15. ***


# Basic predictions: mean, naive, snaive, drift ---------------------------

# See the split to train/test using the filter_index function

baseline_models <- search %>% 
  filter_index("2004 Jan" ~ "2017 Nov") %>% 
  model(mean = MEAN(google_search_volume),
        naive = NAIVE(google_search_volume),
        s_naive = SNAIVE(google_search_volume),
        drift = RW(google_search_volume ~ drift()))

testset_models <- search %>% 
  filter_index("2017 Dec" ~ .)

baseline_models %>% 
  glance()

baseline_models %>% 
  select(2) %>% 
  filter(term == "zombie") %>% 
  report()

baseline_models %>%
  filter(term == "pool") %>%
  forecast(h = 12) %>% 
  autoplot(size = 1, linetype = "dashed") + 
  autolayer(search %>% 
              filter(term == "pool") %>% 
              filter_index("2015 Jan" ~ .),
            color = "black") + 
  ggtitle("Comparison of baseline prediction methods") + 
  guides(color = guide_legend("Method")) + 
  theme(legend.position = "bottom")


# Obtain confidence intervals ---------------------------------------------

baseline_models %>% 
  forecast(h = 12) %>% 
  hilo() 

# A plot with the c.i

baseline_models %>% 
  filter(term == "pool") %>% 
  select(1, 2) %>% 
  forecast(h = 12) %>% 
  autoplot(search %>% filter(term == "pool"))

# *** Go back to slide 19 ***

# Obtain the residuals from the model -------------------------------------

baseline_models %>% 
  augment()

baseline_models %>% 
  filter(term == "pool") %>% 
  augment() %>% 
  group_by(.model) %>% 
  slice(1:36) %>% 
  ggplot(aes(x = month, color = .model, y = .fitted)) + 
  geom_line(size = 1) + 
  geom_line(data = 
              baseline_models %>% 
              filter(term == "pool") %>% 
              augment() %>% 
              slice(1:36),
            inherit.aes = F,
            aes(x = month, y = google_search_volume),
            color = "black", size = 0.5) 

# plot the distribution of residuals:

baseline_models %>% 
  select(1,4) %>% 
  filter(term == "pool") %>% 
  gg_tsresiduals() + 
  ggtitle("Residuals analysis for the pool search terms predicted via SNAIVE")


# *** Go back to slide 22 ***

# Compute the accuracy of predictions -------------------------------------

# The google search example

baseline_models %>% 
  forecast(h=12) %>% 
  accuracy(testset_models)

# MASE is provided on the training set
baseline_models %>% 
  accuracy()

# The movies example

movies_ts_regular %>% 
  filter(title_year < 2002) %>% 
  model(naive = NAIVE(n),
        drift = RW(n ~ drift())) %>% 
  forecast(h = 4) %>% 
  accuracy(movies_ts_regular %>% filter(title_year >= 2002 & title_year <= 2005))

# MASE is provided on the training set:
movies_ts_regular %>% 
  filter(title_year < 2002) %>% 
  model(naive = NAIVE(n),
        drift = RW(n ~ drift()),
        mean = MEAN(n)) %>% 
  accuracy()

# Plotting what we are seeing re the movies example

movies_ts_regular %>% 
  filter(title_year < 2002) %>% 
  model(naive = NAIVE(n),
        drift = RW(n ~ drift()),
        mean = MEAN(n)) %>% 
  forecast(h = 4) %>% 
  autoplot(level = NULL, size = 3) + 
  autolayer(movies_ts_regular %>% filter(title_year <= 2005), color = "black") + 
  # coord_cartesian(xlim = c(1990, 2005)) + 
  ggtitle("Forecast on the number of titles per year (movies)")


# *** Go back to presentation, slide 25 ***


# Fitting an exponential smoothing model ----------------------------------

# for the movies_ts data

movies_exp_smooth <- movies_ts_regular %>% 
  filter(title_year < 2002) %>% 
  model(exp_smooth = ETS(n ~ error("A") + trend("N") + season("N"), opt_crit = "mse"))

movies_exp_fcst <- movies_exp_smooth %>% 
  forecast(h = 4) %>% 
  autoplot() + 
  geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(movies_exp_smooth)) +
  autolayer(movies_ts_regular %>% filter(title_year < 2006), color = "black") + 
  coord_cartesian(xlim = c(1980, 2006)) + 
  ggtitle("Example for simple exponential smoothing on the movies dataset")
  
movies_exp_fcst

movies_exp_smooth %>% 
  accuracy()

movies_exp_smooth %>% 
  gg_tsresiduals()


# The same goes for a seasonal model - can't be captured by SES

search %>% 
  filter(term == "pool") %>% 
  model(ses = ETS(google_search_volume ~ error("A") + trend("N") + season("N"))) %>% 
  gg_tsresiduals() + 
  ggtitle("Simple exponential smoothing on a seasonal time series\nUnable to capture seasonality")


# *** Go back to slide 31 ***

# Fitting an ETS model ----------------------------------------------------

optimal_ets_fit <- search %>% 
  filter(term == "pool") %>% 
  model(overall = ETS(google_search_volume),
        with_trend = ETS(google_search_volume ~ trend("A"))) 

report(optimal_ets_fit %>% select(term, with_trend))
augment(optimal_ets_fit)
accuracy(optimal_ets_fit)

optimal_ets_fit %>% 
  forecast(h=12) %>% 
  autoplot() +
  autolayer(search %>% filter(term == "pool")) + 
  geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(optimal_ets_fit))

optimal_ets_fit %>% components() %>% autoplot()

optimal_ets_fit %>% 
  gg_tsresiduals() 


# Comparison to a different optimization criteria
optimal_ets_fit2 <- search %>% filter(term == "pool") %>% model(ETS(google_search_volume, opt_crit = "sigma"))
report(optimal_ets_fit2)