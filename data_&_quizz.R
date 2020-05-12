# Intro

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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


# Quizz


# Q1
nrow(edx)
ncol(edx)


# Q2
sum(edx$rating == 0)
sum(edx$rating == 3)


# Q3
length(unique(edx$movieId))


# Q4
length(unique(edx$userId))


# Q5
sum(str_detect(edx$genres, "Drama"))
sum(str_detect(edx$genres, "Comedy"))
sum(str_detect(edx$genres, "Thriller"))
sum(str_detect(edx$genres, "Romance"))


# Q6
edx %>%
  group_by(title) %>%
  summarise(nb_ratings = n()) %>%
  arrange(desc(nb_ratings)) %>%
  head(.,10)


# Q7
edx %>%
  group_by(rating) %>%
  summarise(nb_ratings = n()) %>%
  arrange(desc(nb_ratings)) %>%
  head(.,5)


# Q8
edx %>%
  mutate(is_half_rating = str_detect(rating, ".5")) %>%
  summarise(prop_half_ratings = mean(is_half_rating))
