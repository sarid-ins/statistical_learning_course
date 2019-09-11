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


# Cluster movies by genres using binary distance k=5 ----------------------
# now to cluster the movies using cluster::pam (partitioning around medoids), binary distances

movie_dist <- dist(select(movies, -movie_title), method = "binary")
head(movie_dist, 10)

movie_clust <- cluster::pam(x = movie_dist, k = 5)

summary(movie_clust)

factoextra::fviz_silhouette(movie_clust)

# view movies per cluster
movie_clustered <- movies %>% 
  mutate(cluster = movie_clust$clustering) 

movie_clustered %>% 
  select(-movie_title) %>% 
  gather(genre, value, -cluster) %>% 
  group_by(cluster, genre) %>% 
  summarize(mean = mean(value)) %>% 
  filter(mean >= 0.1) %>% 
  ggplot(aes(y = mean, x = genre, fill = genre)) + 
  geom_col(color = "black") +
  facet_grid(rows = ~cluster) + 
  theme_bw() + 
  coord_flip() + 
  ggtitle("Clustering movie types", subtitle = "Occurrence < 10% removed")

# show 10 movies per cluster:
movie_clustered %>% 
  left_join(movies_raw %>% select(movie_title, num_voted_users)) %>% 
  distinct(movie_title, .keep_all = T) %>% 
  arrange(desc(num_voted_users)) %>% 
  group_by(cluster) %>% 
  top_n(10, wt = num_voted_users) %>% 
  select(movie_title, cluster, everything()) %>% 
  arrange(cluster) %>% 
  View

# Investigate the optimal k using "elbow", silhouette, and the gap --------
set.seed(0)
# do not run this, takes too long to run!
# examine_k <- NbClust::NbClust(data = select(movies, -movie_title) %>% 
#                                 sample_n(1000) %>% 
#                                 select_if(~{sum(.)>=150}),
#                               distance = "binary",
#                               min.nc = 1,
#                               max.nc = 5,
#                               index = "alllong",
#                               method = "single")

library(factoextra)

# Elbow method
# set.seed(0)
# elbow_movies <- fviz_nbclust(x = select(movies, -movie_title),
#                              cluster::pam, method = "wss",
#                              diss = movie_dist,
#                              k.max = 8, nboot = 100,
#                              verbose = TRUE) +
#   labs(subtitle = "Elbow method")
# saveRDS(elbow_movies, file = "rds_files/elbow_movies.rds")
elbow_movies <- readRDS("rds_files/elbow_movies.rds")

# Silhouette method
# silhouette_movies <- fviz_nbclust(x = select(movies, -movie_title),
#                                   cluster::pam, method = "silhouette",
#                                   diss = movie_dist,
#                                   k.max = 8, nboot = 100,
#                                   verbose = TRUE) +
#   labs(subtitle = "Silhouette method")
# saveRDS(silhouette_movies, "rds_files/silhouette_movies.rds")
silhouette_movies <- readRDS("rds_files/silhouette_movies.rds")

# Gap statistic
set.seed(0)
# gap_stat_movies <- fviz_nbclust(x = select(movies, -movie_title),
#                                 cluster::pam, method = "gap_stat",
#                                 diss = movie_dist,
#                                 k.max = 8, nboot = 100,
#                                 verbose = TRUE) +
#   labs(subtitle = "Gap statistic method")
# saveRDS(gap_stat_movies, "rds_files/gap_stat_movies.rds")
gap_stat_movies <- readRDS("rds_files/gap_stat_movies.rds")


# Hierarchical clustering for movies --------------------------------------

hclust_movies <- hclust(movie_dist, method = "average")

library(dendextend)

as.dendrogram(hclust_movies) %>% 
  set("labels", "") %>% 
  set("branches_k_color", k = 5) %>% 
  plot(main = "Clustering movies\naverage link function with k=5, binary distance")

movies_hclust_res <- movie_clustered %>% 
  mutate(hcluster = cutree(hclust_movies, k = 7))

clustering_comparison <- movies_hclust_res %>% 
  count(cluster, hcluster) %>% 
  arrange(desc(n))

#install.packages("ggalluvial")
library(ggalluvial)

ggplot(clustering_comparison, aes(y = n, axis1 = cluster, axis2 = hcluster)) + 
  geom_alluvium(aes(fill = factor(cluster))) + 
  geom_stratum(fill = "lightblue", width = 1/12) + 
  scale_x_discrete(limits = c("cluster::pam", "stats::hclust")) + 
  ggtitle("Comparison between clustering methods") + 
  theme_bw() + 
  guides(fill = guide_legend("cluster::pam"))



# See additional sources in the following links ---------------------------

# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
# https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html