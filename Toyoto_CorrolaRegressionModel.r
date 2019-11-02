#scatter plots
pairs(Toyoto_Corrola[,3:11])
#correlation equation
cor(Toyoto_Corrola[,3:11])
#regression model and summary
model.car<-lm(Price~Age_08_04+KM+HP+cc+Doors+Cylinders+Gears+Weight,data=Toyoto_Corrola)
summary(model.car)
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
tc<-Toyoto_Corrola[-81,]
