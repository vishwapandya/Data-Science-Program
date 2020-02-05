Computer_Data1<- Computer_Data[,-1] #save the data while deleting the first column as it is not needed and changes have to be done in the dataset
#change yes and no into 1 and 0
Computer_Data1$cd <- as.factor(revalue(Computer_Data$cd,c("yes"=1, "no"=0)))
Computer_Data1$multi <- as.factor(revalue(Computer_Data$multi,c("yes"=1, "no"=0)))
Computer_Data1$premium <- as.factor(revalue(Computer_Data$premium,c("yes"=1, "no"=0)))
#plot scatter plot
pairs(Computer_Data1)
#build and summarize model
model<- lm(price ~., data= Computer_Data1)
summary(model)
#diagnostic plots
plot(model)
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
