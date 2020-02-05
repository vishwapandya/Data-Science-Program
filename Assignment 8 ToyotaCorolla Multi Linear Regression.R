Corolla<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

#regression model and summary
model.car<-lm(Price~.,data=Corolla)
summary(model.car) #here after we get the summary we can delete the use of "cc" and "doors" column because they are not significant as compared to others.
#diagnostic plots
plot(model.car)
#residual vs regressors
residualPlots(model.car,tests=F)
#added variable plots
avPlots(model.car)
#qq plots of studentized residuals
qqPlot(model.car)
#deletion Diagnostics
influenceIndexPlot(model.car)
#removing 81 observation
tc<-ToyotaCorolla[-81,]
# like this we can keep deleting the observations that are irrelevant to the dataset to get an more accurate and proper model