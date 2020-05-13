
SalaryTest<- `SalaryData_Test(1)`
SalaryTrain<- `SalaryData_Train(1)`

library(naivebayes)
library(ggplot2)
library(caret)
library(e1071)

#visualize the data
ggplot(data=SalaryTrain,aes(x=SalaryTrain$Salary, y = SalaryTrain$age, fill = SalaryTrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(SalaryTrain$workclass,SalaryTrain$Salary)
plot(SalaryTrain$education,SalaryTrain$Salary)
plot(SalaryTrain$educationno,SalaryTrain$Salary)
plot(SalaryTrain$maritalstatus,SalaryTrain$Salary)
plot(SalaryTrain$occupation,SalaryTrain$Salary)
plot(SalaryTrain$relationship,SalaryTrain$Salary)
plot(SalaryTrain$race,SalaryTrain$Salary)
plot(SalaryTrain$sex,SalaryTrain$Salary)

ggplot(data=SalaryTrain,aes(x=SalaryTrain$Salary, y = SalaryTrain$capitalgain, fill = SalaryTrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=SalaryTrain,aes(x=SalaryTrain$Salary, y = SalaryTrain$capitalloss, fill = SalaryTrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=SalaryTrain,aes(x=SalaryTrain$Salary, y = SalaryTrain$hoursperweek, fill = SalaryTrain$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(SalaryTrain$native,SalaryTrain$Salary)


#build the model
model_salary<-naiveBayes(Salary~.,data = SalaryTrain)
model_salary

#predict using the test data
pred_salary<-predict(model_salary,SalaryTest)

#check the first 5 values of predicted and original data 
head(pred_salary)
head(SalaryTest$Salary)

#check the accuracy of the model
confusionMatrix(pred_salary,SalaryTest$Salary)
#the accuracy of the model is 81.93
