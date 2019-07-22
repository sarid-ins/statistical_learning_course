library(tidyverse)

# Short code to tidy the genres a bit
movies_raw <- read_csv("https://raw.githubusercontent.com/sundeepblue/movie_rating_prediction/master/movie_metadata.csv") %>% 
  left_join(read_csv(""))

movie_genres <- movies_raw %>% 
  select(movie_title, genres) %>% 
  separate(genres, sep = ("\\|"), into = paste0("genre", 1:7)) %>% 
  gather(genre_num, value, -movie_title) %>% 
  filter(!is.na(value)) %>% 
  mutate(classified_genre = T) %>% 
  select(-genre_num) %>% 
  distinct() %>% 
  spread(value, classified_genre, fill = F) 

movies <- movies_raw %>% 
  left_join(movie_genres) %>% 
  select(-genres) %>% 
  add_count(director_name, name = "count_director")

write_csv(movies, "datasets/scraped_imdb/movie_db_clean.csv")
