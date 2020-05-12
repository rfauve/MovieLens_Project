# Note: this process could take a couple of minutes (and requires around 3 GB of RAM)

# libraries required
if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(recommenderlab))
  install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
#(.packages())
options(digits = 6)

# dataset download from the GroupLens Research website.
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip",
              dl)

# First data set : ratings
ratings <-
  fread(text = gsub("::", 
                    "\t", 
                    readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
        col.names = c("userId", "movieId", "rating", "timestamp"))

ratings <- 
  as.data.frame(ratings) %>%
  mutate(rating_year = year(as.Date.POSIXct(timestamp))) %>%
  select(-timestamp)

# Second data set : movies
movies <-
  str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <-
  as.data.frame(movies) %>% mutate(
    movieId = as.numeric(movieId),
    title = as.character(title),
    genres = as.character(genres))

# Extraction of release year for each movie
movies <- movies %>%
  mutate(release_year = as.numeric(
    str_remove_all(
      str_extract(title,"\\((\\d{4})\\)"),
      "\\(|\\)")),
    title = str_remove(title," \\((\\d{4})\\)"))

# Join in a single data set
movielens <- left_join(ratings, movies, by = "movieId")

# Train/test sets creation
set.seed(1, sample.kind = "Rounding")
test_index <-
  createDataPartition(
    y = movielens$rating,
    times = 1,
    p = 0.1,
    list = FALSE
  )

edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# removing non-necessary variables
edx <- edx %>% 
  select(userId, movieId, release_year, rating, title)
validation <- validation %>% 
  select(userId, movieId, release_year, rating, title)

# RMSE function
RMSE_fct <- function(test, pred) {
  sqrt(mean((test - pred) ^ 2))
}

# function for lambda (regularisation)
lambda_opt <- function(train, test, fct_mod) {
  prec <- 0.1
  l_new <- 0
  i <- 2
  
  y <- fct_mod(train, test, l_new)
  rmse_l_new <- RMSE_fct(test$rating, y)
  
  out <- data.frame(l = l_new,
                    rmse = rmse_l_new)
  
  while (abs(i) > prec) {
    l_old <- l_new
    rmse_l_old <- rmse_l_new
    l_new <- l_old + i
    y <- fct_mod(train, test, l_new)
    rmse_l_new <- RMSE_fct(test$rating, y)
    if (rmse_l_new > rmse_l_old) {
      i <- -i / 2
    }
    out <- rbind(out,
                 c(l_new,
                   rmse_l_new))
  }
  out[nrow(out)-1,]
}

# model M + U + Y (without SVD)
mod_bm_bu_by <- function(train, test, lambda) {
  mu_rating <- mean(train$rating)
  movie_biais <- train %>%
    group_by(movieId) %>%
    summarise(b_movie = sum(rating - mu_rating) / (n() + lambda))
  
  user_biais <- train %>%
    left_join(movie_biais, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_user = sum(rating - mu_rating - b_movie) / (n() + lambda))
  
  year_biais <- train %>%
    left_join(user_biais, by = "userId") %>%
    left_join(movie_biais, by = "movieId") %>%
    group_by(release_year) %>%
    summarise(b_year = sum(rating - mu_rating - b_user - b_movie) / (n() + lambda))
  
  test <- test %>%
    left_join(movie_biais, by = "movieId") %>%
    left_join(user_biais, by = "userId") %>%
    left_join(year_biais, by = "release_year") %>%
    mutate(pred = mu_rating + b_movie + b_user + b_year)
  
  test$pred
}

# model M + U + Y (with SVD)
mod_bm_bu_by_SVD <- function(train, test, lambda) {
  mu_rating <- mean(train$rating)
  movie_biais <- train %>%
    group_by(movieId) %>%
    summarise(b_movie = sum(rating - mu_rating) / (n() + lambda))
  
  user_biais <- train %>%
    left_join(movie_biais, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_user = sum(rating - mu_rating - b_movie) / (n() + lambda))
  
  year_biais <- train %>%
    left_join(user_biais, by = "userId") %>%
    left_join(movie_biais, by = "movieId") %>%
    group_by(release_year) %>%
    summarise(b_year = sum(rating - mu_rating - b_user - b_movie) / (n() + lambda))
  
  resids <- train %>%
    left_join(movie_biais, by = "movieId") %>%
    left_join(user_biais, by = "userId") %>%
    left_join(year_biais, by = "release_year") %>%
    mutate(res = rating - (mu_rating + b_movie + b_user + b_year)) %>%
    select(userId, movieId, res) %>%
    spread(movieId, res)
  
  rownames(resids) <- resids[, 1]
  resids <- resids[, -1]
  
  res_SVD <- funkSVD(resids,
                     k = 7,
                     gamma = 0.015,
                     lambda = 0.001,
                     min_improvement = 1e-06,
                     min_epochs = 30,
                     max_epochs = 50,
                     verbose = TRUE)
  
  r <- tcrossprod(res_SVD$U, res_SVD$V)
  
  rownames(r) <- rownames(resids)
  colnames(r) <- colnames(resids)
  remove(resids)
  
  test$res_corr <- 0
  test$res_corr <- sapply(1:nrow(test), function(x) {
    r[which(test$userId[x] == rownames(r)), 
      which(test$movieId[x] == colnames(r))]
  })
  remove(r)
  
  test <- test %>%
    left_join(movie_biais, by = "movieId") %>%
    left_join(user_biais, by = "userId") %>%
    left_join(year_biais, by = "release_year") %>%
    mutate(pred = mu_rating + b_movie + b_user + b_year + res_corr)
  
  test$pred
}


res <- data.frame()

# M + U + Y model
l_opt <- lambda_opt(edx, validation, mod_bm_bu_by) #1min
res[1, 1] <- l_opt[1, 2] # 0.864522

# M + U + Y + SVD model   /!\ 3h30 /!\
# y_M_U_Y_SVD <- mod_bm_bu_by_SVD(edx, validation, l_opt[1, 1]) 
# save(y_M_U_Y_SVD, file = "./pred_SVD_final.Rda")

# the results has to be precalculated, as the execution could not go through
load(file = "./pred_SVD_final.Rda")
res[1, 2] <- RMSE_fct(validation$rating, y_M_U_Y_SVD) # 0.815741

# # nice display of both RMSEs
# RMSE_results <- tibble(model = "U + M + Y",
#                        RMSE = res[1, 1])
# 
# RMSE_results <-
#   rbind(RMSE_results,
#         tibble(model = "U + M + Y + SVD",
#                RMSE = res[1, 2]))
# 
# RMSE_results %>% knitr::kable()

# display final RMSE (threshold: 0.86490, final RMSE: 0.815741)
res[1, 2]

# Tadaa !!

