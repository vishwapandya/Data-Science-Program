#plot the data
corrplot(cor(glass))
         
#divide the glass data into test and train data
inTraininglocal<- createDataPartition(glass$Type,p=0.50,list=F)
glass_train <- glass[inTraininglocal,]
glass_test <- glass[-inTraininglocal,]

#give labels to test and train data 
glass_train_labels <- glass[1:106, 1]
glass_test_labels <- glass[107:214, 1]

#build the model
knn_glass<- knn(glass_train,glass_test,glass_train$Type,k=1)

#build the confusion matrix. levels are given so that error does not occur
confusionMatrix(
  factor(knn_glass, levels = 1:7),
  factor(glass_test$Type, levels = 1:7)
)

#the model shows that with 1 k value the model accuracy is 95%
