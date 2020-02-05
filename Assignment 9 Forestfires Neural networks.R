#transfer the data as changes are going to be made
FF<- forestfires

#change characters into variables
FF$month<- as.factor(revalue(forestfires$month,c("jan"=1, "feb"=2, "mar"=3, "apr"=4, "may"=5, "jun"=6,"jul"=7,"aug"=8, "sep"=9, "oct"=10, "nov"= 11,"dec"= 12)))
FF$day <- as.factor(revalue(forestfires$day,c("mon"=1, "tue"=2, "wed"=3, "thu"=4, "fri"=5, "sat"=6, "sun"=7)))
FF$size_category <- as.factor(revalue(forestfires$size_category,c("small"=1, "large"=2)))

#change factors into numeric
FF$month<-unfactor(FF$month)
FF$day<-unfactor(FF$day)
FF$size_category<-unfactor(FF$size_category)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame

FF_norm <- as.data.frame(lapply(FF, normalize))

# create training and test data
intraininglocal<- createDataPartition(FF_norm$size_category,p=0.75,list=F)
FF_train <- FF_norm[intraininglocal, ]
FF_test <- FF_norm[-intraininglocal, ]

library(neuralnet)

# simple ANN with only a single hidden neuron

FF_model <- neuralnet(size_category~.,
                            data = FF_train)

# visualize 
windows()
plot(FF_model)

# obtain model results
model_results <- compute(FF_model, FF_test[1:31])
# obtain predicted size category values
predicted_size_category <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_size_category, FF_test$size_category)

# a more complex neural network topology with 5 hidden neurons
FF_model2 <- neuralnet(size_category ~. ,
                             data = FF_train, hidden =c(5,2))
# plot 
windows()
plot(FF_model2)

# evaluate the results as we did before
model_results2 <- compute(FF_model2, FF_test[1:31])
predicted_size_category2 <- model_results2$net.result
cor(predicted_size_category2, FF_test$size_category)
