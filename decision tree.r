data("iris")

#install.packages("caret")
#install.packages("C50")
library(caret)
library(C50)

inTraininglocal<-createDataPartition(iris$Species,p=.75,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]


#Model Building
model<-C5.0(training$Species~.,data = training) #Trials- Boosting parameter

#Generate the model summary
summary(model)
#Predict for test data set
pred<-predict.C5.0(model,testing[,-5])
a<-table(testing$Species,pred)
sum(diag(a))/sum(a)

plot(model)




