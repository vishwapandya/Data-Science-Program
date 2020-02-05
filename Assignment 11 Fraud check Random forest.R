#any taxable income less than or equal to 30000 should be named as risky others should be good
Risky <- NULL
Risky <- ifelse(Fraud_check$Taxable.Income<=30000,"Risky","Good")
Fraud_check[,"Risky"] <- Risky

#change characters into variables
Fraud_check$Undergrad <- as.factor(revalue(Fraud_check$Undergrad,c("YES"=1, "NO"=0)))
Fraud_check$Marital.Status <- as.factor(revalue(Fraud_check$Marital.Status,c("Divorced"=0, "Married"=1, "Single"= 2 )))
Fraud_check$Urban  <- as.factor(revalue(Fraud_check$Urban,c("YES"=1, "NO"=0)))
Fraud_check$Risky <- as.factor(revalue(Fraud_check$Risky,c("Risky"=1, "Good"=0)))

#partition the data
intraininglocal<- createDataPartition(Fraud_check$Risky,p=0.75,list=F)
FC_train<- Fraud_check[intraininglocal,]
FC_test<- Fraud_check[-intraininglocal,]

#make the random forest model with training data
model<- randomForest(Risky~.,data=FC_train, ntree=1000)
print(model)

#Imoporantce of the variable - Lower Gini
print(importance(model))

#predict the values using training data 
pred<- predict(model,FC_train)

#check the values predicted by the model with the actual data
head(pred)
head(FC_train$Risky)

#the confusion matrix will show the training model accuracy (in this case it is 100%)
confusionMatrix(pred, FC_train$Risky)

#now predict using the test data
pred1 <- predict(model, FC_test)

#the accuracy of test data is 100%
confusionMatrix(pred1, FC_test$Risky)

#once again check the predicted and actual values
head(pred1)
head(FC_test$Risky)
#the accuracy can change everytime because data is partitioned randomly