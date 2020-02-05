SalaryTest<- `SalaryData_Test(1)`
SalaryTrain<- `SalaryData_Train(1)`

library(caret)
library(e1071)

#build the model
model_salary<-naiveBayes(Salary~.,data = SalaryTrain)

#predict using the test data
pred_salary<-predict(model_salary,SalaryTest)

#check the first 5 values of predicted and original data 
head(pred_salary)
head(SalaryTest$Salary)

#check the accuracy of the model
confusionMatrix(pred_salary,SalaryTest$Salary)
#the accuracy of the model is 81.93
