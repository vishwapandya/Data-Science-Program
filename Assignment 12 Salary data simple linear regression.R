#simple linear regression model 
model<- lm(Salary~YearsExperience, data=Salary_Data)
summary(model)
confint(model,level = 0.95) 
predict(model,interval="predict")
#above model's adjusted Rsquared value is 0.9554

#logarthmic transformation model
model_log<-lm(Salary~log(YearsExperience), data= Salary_Data)
summary(model_log)
confint(model_log,level=0.95)
predict(model_log,interval = "predict")
#above model's adjusted Rsquared value is 0.8487

#exponential model 
model_exp<-lm(log(Salary)~YearsExperience, data= Salary_Data) 
summary(model_exp)
confint(model_exp,level=0.95)
predict(model_exp,interval = "predict")
#above model's adjusted Rsquared value is 0.9295
#out of the above three model you can use the model which has the highest value of adjusted Rsquare
#changes for getting a more accurate model can be done by changing the confidence intervals