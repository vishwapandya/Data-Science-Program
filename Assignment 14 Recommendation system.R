library("recommenderlab")
library(caTools)


#metadata about the variable
str(books)


#rating distribution
hist(books$ratings)

#the datatype should be realRatingMatrix inorder to build recommendation engine
books_data_matrix <- as(books, 'realRatingMatrix')

#Popularity based 

books_recomm_model1 <- Recommender(books_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(books_recomm_model1, books_data_matrix[516:517], n=5)
as(recommended_items1, "list")


## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

books_recomm_model2 <- Recommender(books_data_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(books_recomm_model2, books_data_matrix[516:517], n=5)
as(recommended_items2, "list")
