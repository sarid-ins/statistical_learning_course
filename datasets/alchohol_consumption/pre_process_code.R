# Pre process code to generate a single tibble
library(tidyverse)



continents <- read_csv("https://github.com/sarid-ins/statistical_learning_course/raw/master/datasets/alchohol_consumption/country-and-continent-codes-list.csv")

country_gdp <- read_csv("https://github.com/sarid-ins/statistical_learning_course/raw/master/datasets/alchohol_consumption/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_41089.csv",
                        skip = 4) %>% 
  select(`Country Name`, `Country Code`, `2010`)

alchohol_consumption <- 
  read_csv("https://github.com/sarid-ins/statistical_learning_course/raw/master/datasets/alchohol_consumption/week13_alcohol_global.csv") %>% 
  mutate(country = ifelse(country == "USA", "United States", country)) %>% 
  left_join(country_gdp, by = c("country" = "Country Name")) %>% 
  filter(!is.na(`Country Code`)) %>% 
  left_join(continents, by = c("Country Code" = "Three_Letter_Country_Code")) %>% 
  rename(gdp2010 = `2010`) %>% 
  filter(!is.na(gdp2010))

write_csv(alchohol_consumption, "datasets/alchohol_consumption/alchohol_consumption_ready.csv")
