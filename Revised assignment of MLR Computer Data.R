Computer_Data1<- Computer_Data[,-1] #save the data while deleting the first column as it is not needed and changes have to be done in the dataset

#change yes and no into 1 and 0
Computer_Data1$cd <- as.factor(revalue(Computer_Data$cd,c("yes"=1, "no"=0)))
Computer_Data1$multi <- as.factor(revalue(Computer_Data$multi,c("yes"=1, "no"=0)))
Computer_Data1$premium <- as.factor(revalue(Computer_Data$premium,c("yes"=1, "no"=0)))

#plot the data 
pairs(Computer_Data1)
plot(Computer_Data1$speed, Computer_Data1$price)
plot(Computer_Data1$hd, Computer_Data1$price)
plot(Computer_Data1$ram, Computer_Data1$price)
plot(Computer_Data1$screen, Computer_Data1$price)
plot(Computer_Data1$ads, Computer_Data1$price)
plot(Computer_Data1$trend, Computer_Data1$price)
plot(Computer_Data1$cd, Computer_Data1$price)
plot(Computer_Data1$multi, Computer_Data1$price)
plot(Computer_Data1$premium, Computer_Data1$price)

#create data partition
inTraininglocal<- createDataPartition(Computer_Data1$price,p=.85,list=F)
Computer_Data1_train <- Computer_Data1[inTraininglocal,]
Computer_Data1_test <-Computer_Data1[-inTraininglocal,]

#build and summarize model
model<- lm(price ~., data= Computer_Data1_train)
summary(model)

#now use the test data 
model1<- lm(price~., data = Computer_Data1_test)
summary(model1)

#diagnostic plots
plot(model1)

#residual vs regressors
residualPlots(model,tests=F)

#added variable plots
avPlots(model)

#qq plots of studentized residuals
qqPlot(model)

#deletion Diagnostics
influenceIndexPlot(model)

#removing 1441th observation
cd<-Computer_Data1[-1441,]
# like this we can keep deleting the observations that are irrelevant to the dataset to get an more accurate and proper model
