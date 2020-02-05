#simple linear regression model 
model<- lm(Delivery.Time~Sorting.Time, data=delivery_time)
summary(model)
confint(model,level = 0.95) 
predict(model,interval="predict")
#above model's adjusted Rsquared value is 0.6655

#logarthmic transformation model
model_log<-lm(Delivery.Time~log(Sorting.Time), data= delivery_time)
summary(model_log)
confint(model_log,level=0.95)
predict(model_log,interval = "predict")
#above model's adjusted Rsquared value is 0.6794

#exponential model 
model_exp<-lm(log(Delivery.Time)~Sorting.Time, data= delivery_time) 
summary(model_exp)
confint(model_exp,level=0.95)
predict(model_exp,interval = "predict")
#above model's adjusted Rsquared value is 0.6957
#out of the above three model you can use the model which has the highest value of adjusted Rsquare
#changes for getting a more accurate model can be done by changing the confidence intervals