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

# We will be demonstrating cluster::pam, hclust, dendextend for comparing hclust results, and NbClust for selecting k.

# Cluster movies according to genre ---------------------------------------



# Cluster countries according to alchohol consumption ---------------------



# using: ------------------------------------------------------------------

# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
# https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html
