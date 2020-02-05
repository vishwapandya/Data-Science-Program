#transfer the data because changes are going to be made
Startups<- `50_Startups(1)`

#change characters into variables
Startups$State <- as.factor(revalue(`50_Startups(1)`$State,c("New York"=0, "California"=1, "Florida"=2)))

#change factor into numeric 
Startups$State<-unfactor(Startups$State)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
Startups_norm <- as.data.frame(lapply(Startups, normalize))

# create training and test data
intraininglocal<- createDataPartition(Startups_norm$Profit,p=0.75,list=F)
Startups_train <- Startups_norm[intraininglocal, ]
Startups_test <- Startups_norm[-intraininglocal, ]

library(neuralnet)

# simple ANN with only a single hidden neuron

Startups_model <- neuralnet(Profit~.,
                            data = Startups_train)

# visualize 
windows()
plot(Startups_model)

# obtain model results
model_results <- compute(Startups_model, Startups_test[1:5])
# obtain predicted strength values
predicted_profit <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_profit, Startups_test$Profit)

# a more complex neural network topology with 5 hidden neurons
Startups_model2 <- neuralnet(Profit ~ .,
                             data = Startups_train, hidden =c(5,2))
# plot 
windows()
plot(Startups_model2)

# evaluate the results as we did before
model_results2 <- compute(Startups_model2, Startups_test[1:5])
predicted_profit2 <- model_results2$net.result
cor(predicted_profit2, Startups_test$Profit)
