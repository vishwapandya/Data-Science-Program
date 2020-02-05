#store the data in some other name because changes are going to be made
startups<- `50_Startups`
#change the characters into variables
startups$State <- as.factor(revalue(`50_Startups`$State,c("New York"=0, "California"=1, "Florida"=2)))
#split the data into test and training sets
intrainingLocal<- createDataPartition(startups$Profit,p=0.75,list=F)
startups_train<- startups[intrainingLocal,]
startups_test<- startups[-intrainingLocal,]
#make model ussing all the parameters and using the training test
model1<- lm(Profit~.,data=startups_train)
summary(model1)
#remove administration as it is not significant
model2<- lm(Profit~R.D.Spend+Marketing.Spend+State, data= startups_train) 
summary(model2)
#remove state as it is not significant
model3<- lm(Profit~R.D.Spend+Marketing.Spend, data= startups_train) 
summary(model3)
#finally make the model using the test set
model4<- lm(Profit~R.D.Spend+Marketing.Spend, data= startups_test) 
summary(model4)
#let the model4 predict the prices
model_predict<- predict(model4,data= startups_test)
model_predict
#plot the predictions
plot(startups_test$Profit, model_predict)
#make the table of the model numbers and the adjusted r squared values
A<- c(1,2,3,4)
B<- c(0.9541,0.9554,0.9574,0.9099)
table<- data.frame(modelnumber= A,Rsquaredvalue= B)
table
#the adjusted r squared value may be diffrent then mentioned by me that is because the data is partitioned randomnly everytime you partion it