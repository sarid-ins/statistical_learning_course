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

movies_raw <- read_csv("datasets/scraped_imdb/movie_db_clean.csv", col_types = cols())

movies <- movies_raw %>% 
  select(Action:Western, movie_title) %>% 
  mutate_at(vars(-movie_title), as.numeric) %>% 
  select(-News, -`Reality-TV`, -Short, -`Film-Noir`, `Game-Show`) # removing genres with few observations, I checked real quick with colSums

# distrbution of number of categories per movie:
appear_genres <- movies %>% 
  gather(category, relevance, -movie_title) %>% 
  filter(relevance == 1) %>% 
  count(movie_title)

ggplot(appear_genres, aes(n)) + 
  stat_ecdf() + 
  theme_bw() + 
  ggtitle("Number of genres per movie (imdb scraped data)")


# Cluster movies by genres using binary distance --------------------------
# now to cluster the movies using cluster::pam (partitioning around medoids), binary distances

movie_dist <- dist(select(movies, -movie_title), method = "binary")
head(movie_dist, 10)

movie_clust <- cluster::pam(x = movie_dist, k = 5)

summary(movie_clust)

factoextra::fviz_silhouette(movie_clust)

# Cluster countries according to alchohol consumption ---------------------



# using: ------------------------------------------------------------------

# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
# https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html
