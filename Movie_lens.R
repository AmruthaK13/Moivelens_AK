library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)
#first 6 rows with head function
head(edx)
#summary statistics
summary(edx)
n_distinct(edx$movieId)
n_distinct(edx$rating)
n_distinct(edx$userId)
n_distinct(edx$genres)
med_rating <- median(edx$rating)
med_rating
#Data visualization
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "blue") +
  xlab("Rating") +
  ylab("Number of") +
  ggtitle("Ratings Histogram") +
  theme(plot.title = element_text(hjust = 0.5))
#Top 10 popular genres
pop_genres <- edx %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number))
knitr::kable(head(pop_genres,10))
#Ratings vs Users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "blue", bin = 30) +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Users") +
  ggtitle("Users vs Ratings") +
  theme(plot.title = element_text(hjust = 0.5))
#Data analysis
mu <- mean(edx$rating)
mu
#basic mean analysis
rmse_basic <- RMSE(validation$rating, mu)
rmse_basic
#creating a table to display all the results of analysis
rmse_table = tibble(Analysis = "Basic Analysis", RMSE = rmse_basic)
rmse_table %>% knitr::kable()
#Introducing movie effect
movie_effect <- edx %>%
  group_by(movieId) %>%
  summarise(a = mean(rating - mu))
predict_ratings <- mu + validation %>%
  left_join(movie_effect, by = "movieId") %>%
  pull(a)
rmse_movie_effect <- RMSE(predict_ratings, validation$rating)
rmse_movie_effect
#Adding Movie effect result to the table
rmse_table <- bind_rows(rmse_table, tibble(Analysis = "Movie Effect Analysis", RMSE = rmse_movie_effect))
rmse_table %>% knitr::kable()
#Introducing user effect
user_effect <- edx %>%
  group_by(userId) %>%
  summarise(b = mean(rating - mu))
predict_ratings <- validation %>%
  left_join(user_effect, by = "userId") %>%
  pull(b)
rmse_user_effect <- RMSE(predict_ratings, validation$rating)
rmse_user_effect
#Adding the User effect result to the table
rmse_table <- bind_rows(rmse_table, tibble(Analysis = "User Effect Analysis", RMSE = rmse_user_effect))
rmse_table %>% knitr::kable()
#Introducing genre effect
genre_effect <- edx %>%
  group_by(genres) %>%
  summarise(c = mean(rating - mu))
predict_ratings <- validation %>%
  left_join(genre_effect, by = "genres") %>%
  pull(c)
rmse_genre_effect <- RMSE(predict_ratings, validation$rating)
rmse_genre_effect
#Adding Genre effect result to the table
rmse_table <- bind_rows(rmse_table, tibble(Analysis = "Genre Effect Analysis", RMSE = rmse_genre_effect))
rmse_table %>% knitr::kable()
#Introducing both movie and user effects
user_effect <- edx %>%
  left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>%
  summarise(a_u = mean(rating - mu - a))
predict_ratings <- validation %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  mutate(d = mu + a + a_u) %>%
  pull(d)
rmse_movie_user_effect <- RMSE(predict_ratings, validation$rating)
rmse_movie_user_effect
#Addind Movie-User Effect result to the table
rmse_table <- bind_rows(rmse_table, tibble(Analysis = "Movied & User Effect Analysis", RMSE = rmse_movie_user_effect))
rmse_table %>% knitr::kable()
#regularization
lambdas <- seq(0, 10, 20 , 1)
rmses <- sapply(lambdas, function(1) {
  mu <- mean(edx$rating)
  x <- edx %>%
    group_by(movieId) %>%
    summarise(x = sum(rating - mu) / (n()+1))
  y <- edx %>%
    left_join(x, by = "movieId") %>%
    group_by(userId) %>%
    summarise(y = sum(rating - x - mu) / (n()+1))
  
  predicted_ratings <- validation %>%
    left_join(x, by = "movieId") %>%
    left_join(y, by = "userId") %>%
    mutate(z = mu + x + y) %>%
    pull(z)
  return(RMSE(predicted_ratings, validation$rating))
})
rmse_regularization <- min(rmses)
rmse_regularization
#Adding the Regularization result to the table
rmse_table <- bind_rows(rmse_table, tibble(Analysis = "Regularized Movie and User Effect", RMSE = rmse_regularization))
rmse_table %>% knitr::kable()
























