library(caret)
library(e1071)
#split the data 
intraininglocal<- createDataPartition(sms_raw_NB$type,p=0.50,list=F)
sms_train<- sms_raw_NB[intraininglocal,]
sms_test<- sms_raw_NB[-intraininglocal,]

#build the model
model_sms<-naiveBayes(type~.,data = sms_train)

#predict using the test data
pred_sms<-predict(model_sms,sms_test)

#check the first 5 values of predicted and original data 
head(pred_sms)
head(sms_test$type)

#check the accuracy of the model
confusionMatrix(pred_sms,sms_test$type)
#the accuracy of the model is 86.61%
#accuracy may change because data splits randomly