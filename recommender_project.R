#====================Packages====================

library (recommenderlab)
library (data.table)
library (dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library (gridExtra)
library (methods)
library(Matrix)

#====================Files====================

book_tags <- read.csv("files/book_tags.csv")
books <- read.csv("files/books.csv")
ratings <- read.csv("files/ratings.csv")
tags <- read.csv("files/tags.csv")

#====================Data Cleaning====================

#Finding duplicate ratings
ratings %>% group_by(user_id, book_id) %>% mutate(N = n()) -> ratings
table(ratings$N)
ratings %>% filter(N > 1) -> duplicate_ratings

#Removing duplicate ratings
ratings %>% filter(N==1) -> ratings

#Removing Users with less than 3 ratings given
ratings %>% group_by(user_id) %>% mutate(ratings_given = n()) -> ratings
ratings %>% filter(ratings_given > 2) -> ratings

#====================Sampling====================

set.seed(1)
user_fraction <- 0.02
users <- unique(ratings$user_id)
sample_users <- sample(users, round(user_fraction * length(users)))
nrow(ratings)
ratings %>% filter(user_id %in% sample_users) -> ratings
nrow(ratings)

#Distribution of Ratings

ratings %>% 
  ggplot(aes(x = rating, fill = factor(rating))) +
    geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = 'none')
  
#Number of ratings per book

ratings %>%
  group_by(book_id) %>%
    summarise(number_of_ratings_per_book = n()) %>%
      ggplot(aes(number_of_ratings_per_book)) +
        geom_bar(fill = "orange", color = "grey20", width = 1) + coord_cartesian(c(0, 20))

#====================Finding Genres====================

genres <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics", "Cookbooks", "crime", "Fantasy", "Gay and Lesbian", "Graphic Novels", "Historical Fiction", "History","Hоггог", "Humor and Comedy", "Manga", "Memoir", "Music", "Mystery", "Paranormal", "Philosophy", "Romance", "Science", "Poetry", "Psychology", "Religion", "Science Fiction", "Self Help", "Suspense", "Spirituality", "Sports", "Thriller", "Travel", "Young Adult"))

avaliable_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
avaliable_tags <- tags$tag_id[match(avaliable_genres, tags$tag_name)]

#Plotting Genre Percentage

tmp <- book_tags %>%
  filter(tag_id %in% avaliable_tags) %>%
    group_by(tag_id) %>%
      summarise(n = n()) %>%
        ungroup() %>%
          mutate(sumN = sum(n), percentage = (n / sumN)*100) %>%
            arrange(- percentage) %>%
              left_join(tags, by = "tag_id")

tmp %>%
  ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + geom_bar(stat = "identity") +
    coord_flip() + scale_fill_distiller(palette = "YlOrRd") + labs(y = 'Percentage', x = 'Genre')

#====================Top 10====================

#Top 10 Ratings
top10 <- books %>%
  arrange(-average_rating) %>%
    top_n(10, wt = average_rating) %>%
      select(title, ratings_count, average_rating)

#Top 10 Popularity
toppopular <- books %>%
  arrange(-ratings_count) %>%
    top_n(10, wt = ratings_count) %>%
      select(title, ratings_count, average_rating)

#====================Re-Structuring Data====================

dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(sort(unique(ratings$book_id)))) 
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>%
  select(-user_id)
ratingmat <- as.matrix(ratingmat)
ratingmat[1:5, 1:5]
ratingmat[,-1] -> ratingmat
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]
dim(ratingmat)

#Real Rating Matrix
ratingmat0 <- ratingmat
dim(ratingmat)
ratingmat[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

#====================Modeling====================

#Splitting Data
sample(x = c(T, F), size = nrow(real_ratings), replace = T, prob = c(0.8, 0.2)) -> split_book
real_ratings[split_book, ] -> recc_train
real_ratings[!split_book, ] -> recc_test

#UBCF model
Recommender(data = recc_train, method = "UBCF") -> recc_model_ubcf
n_recommend_ubcf <- 6

predict(object=recc_model_ubcf, newdata=recc_test, interval=n_recommended_ubcf) -> recc_predicted_ubcf
