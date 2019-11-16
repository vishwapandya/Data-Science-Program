
data("iris")



 
  ##Data Partition
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list = F)
  training<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]
  #Model building 
  model <- C5.0(training$Species~., data=training,trials=10)#trials= boosting parameter
  summary(model)
  #Predicting
  pred<-predict.C5.0(model,testing[,-5])
  #summary(pred)
  a<-table(testing$Species,pred)
  


summary(model)
