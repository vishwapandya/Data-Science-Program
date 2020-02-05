
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame

concrete_norm <- as.data.frame(lapply(concrete, normalize))

# create training and test data
intraininglocal<- createDataPartition(concrete$strength,p=0.75,list=F)
concrete_train <- concrete_norm[intraininglocal, ]
concrete_test <- concrete_norm[-intraininglocal, ]

library(neuralnet)

# simple ANN with only a single hidden neuron

concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)

# visualize 
windows()
plot(concrete_model)

# obtain model results
model_results <- compute(concrete_model, concrete_test[1:8])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

# a more complex neural network topology with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden =c(5,2))
# plot 
windows()
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
