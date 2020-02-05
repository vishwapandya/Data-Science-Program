risky_good<- ifelse(Fraud_check$Taxable.Income<= 30000, "Risky", "Good")
df<- data.frame(Fraud_check,risky_good)
inTraininglocal<-createDataPartition(df$risky_good,p=.50,list = F)
training<-df[inTraininglocal,]
testing<-df[-inTraininglocal,]
model<-C5.0(training$risky_good~.,data = training) #Trials- Boosting parameter

#Generate the model summary
summary(model)
#Predict for test data set
pred<-predict.C5.0(model,testing[,-7])
a<-table(testing$risky_good,pred)
sum(diag(a))/sum(a)
print(a)


plot(model)












