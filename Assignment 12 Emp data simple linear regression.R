#simple linear regression model 
model<- lm(Churn_out_rate~Salary_hike, data=emp_data)
summary(model)
confint(model,level = 0.95) 
predict(model,interval="predict")
#above model's adjusted Rsquared value is 0.8101

#logarthmic transformation model
model_log<-lm(Churn_out_rate~log(Salary_hike), data= emp_data)
summary(model_log)
confint(model_log,level=0.95)
predict(model_log,interval = "predict")
#above model's adjusted Rsquared value is 0.8297

#exponential model 
model_exp<-lm(log(Churn_out_rate)~Salary_hike, data= emp_data) 
summary(model_exp)
confint(model_exp,level=0.95)
predict(model_exp,interval = "predict")
#above model's adjusted Rsquared value is 0.8577
#out of the above three model you can use the model which has the highest value of adjusted Rsquare
#changes for getting a more accurate model can be done by changing the confidence intervals