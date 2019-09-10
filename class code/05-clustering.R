# kmeans/kmedoids clustering example
library(tidyverse)

# Leverage wisdom of the crowds which are common?  ------------------------

#devtools::install_github("metacran/cranlogs")
clustering_packages <- read_csv("class code/05-clustering_packages_task_view.csv",
                                col_names = "package_name") %>% 
  filter(!str_detect(package_name, "(core)"))

download_stats <- cranlogs::cran_downloads(packages = clustering_packages$package_name,
                                           when = "last-month") %>% 
  group_by(package) %>% 
  summarize(total_downloads = sum(count))

download_stats %>% 
  top_n(10) %>% 
  arrange(desc(total_downloads))

# fpc is the most "popular" clustering package (e1071 is the svm package which also contains some clustering functions).
# We will use fpc to demonstrate kmeans.



# Cluster movies according to genre ---------------------------------------


# Cluster countries according to alchohol consumption ---------------------
