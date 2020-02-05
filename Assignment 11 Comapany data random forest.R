library("caret")
library("randomForest")

#change the values of sales into yes and no 
Sales<- ifelse(Company_Data$Sales<9, "No", "Yes")

#combine the yes and no sales value in a new dataframe with all other columns
CD<- data.frame(Company_Data[2:11], Sales)

#partition the data
intraininglocal<- createDataPartition(CD$Sales,p=0.75,list=F)
CD_train<- CD[intraininglocal,]
CD_test<- CD[-intraininglocal,]

#make the random forest model with training data
model<-randomForest(Sales~.,data=CD_train,ntree=500)
print(model)

#Imoporantce of the variable - Lower Gini
print(importance(model))

#predict the values using training data 
pred<- predict(model,CD_train)

#check the values predicted by the model with the actual data
head(pred)
head(CD_train$Sales)

#the confusion matrix will show the training model accuracy (in this case it is 100%)
confusionMatrix(pred, CD_train$Sales)

#now predict using the test data
pred1 <- predict(model, CD_test)

#the accuracy of test data is 89.9%
confusionMatrix(pred1, CD_test$Sales)

#once again check the predicted and actual values
head(pred1)
head(CD_test$Sales)
#the accuracy can change everytime because data is partitioned randomly