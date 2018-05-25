# install.packages('recommenderlab')
# install.packages('data.table')
# install.packages('reshape2')
# install.packages('ggplot2')

library(recommenderlab)
library(data.table)
library(reshape2)
library(ggplot2)

# Read in the movies and ratings data sets
movies <- read.csv("data/movies.csv", stringsAsFactors = FALSE)
ratings <- read.csv("data/ratings.csv", stringsAsFactors = FALSE)

# Extract genre from movies data frame
genres <- as.data.frame(movies$genres, stringsAsFactors = FALSE)

genres <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert = TRUE), stringsAsFactors = FALSE)
colnames(genres) <- c(1:10)
genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") 

# Creates a matrix for the number of movies + 1 and the number of genres
genre_matrix <- matrix(0, 9126, 18)
# Set first row to genre list
genre_matrix[1,] <- genre_list
# Set column names to genre list
colnames(genre_matrix) <- genre_list

# Iterate through matrix
for (i in 1:nrow(genres)) {
  for (c in 1:ncol(genres)) {
    genmat_col = which(genre_matrix[1,] == genres[i,c])
    genre_matrix[i + 1,genmat_col] <- 1
  }
}

# Convert the matrix into a data frame
# Remove first row, which was the genre list
genre_matrix <- as.data.frame(genre_matrix[-1,], stringsAsFactors = FALSE) 
# Convert from characters to integers
for (c in 1:ncol(genre_matrix)) {
  genre_matrix[,c] <- as.integer(genre_matrix[,c])  
} 

# Create search matrix
search_matrix <- cbind(movies[,1:2], genre_matrix)

# Create ratings matrix. Rows = userId, Columns = movieId
# Create ratings matrix. Rows = userId, Columns = movieId
rating_matrix <- dcast(ratings, userId~movieId, value.var = "rating", na.rm = FALSE)
# Remove userId's
rating_matrix <- as.matrix(rating_matrix[,-1])
# Convert rating matrix into a recommenderlab sparse matrix
rating_matrix <- as(rating_matrix, "realRatingMatrix")

# Selecting relevant data
# Select minimum number of users per rated movie
# and the minimum views per user
ratings_movies <- rating_matrix[rowCounts(rating_matrix) > 50, colCounts(rating_matrix) > 50]

# Normalize the data
# Remove bias instances of data 
# Averages the rating for each user to 0
normalized_ratings_movies <- normalize(ratings_movies)
sum(rowMeans(normalized_ratings_movies) > 0.00001)


# Define training and set data




# Building the recommendation model



# Applying the recommender system on the original dataset

# No. of recommendations we want per user 



# Recommendations for the first user


# A matrix of recommendations for each user


