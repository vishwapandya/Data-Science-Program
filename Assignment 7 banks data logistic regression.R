library('AER')
library(plyr)


banks1 <- bank.full #transfer because changes are going to be made in the dataset
summary(banks1)

table(banks1$y)#shows how many values are there in how many rows eg: no has appeared 39922 times

 # change characters to numbers 
banks1$default <- as.factor(revalue(bank.full$default,c("yes"=1, "no"=0)))
banks1$housing <- as.factor(revalue(bank.full$housing,c("yes"=1, "no"=0)))
banks1$loan <- as.factor(revalue(bank.full$loan,c("yes"=1, "no"=0)))
banks1$y <- as.factor(revalue(bank.full$y,c("yes"=1, "no"=0)))


model <- glm(y ~ ., data = banks1,family = "binomial")

#Exponentiating back the coefficients
exp(coef(model))

prob <- predict(model,banks1,type="response")
summary(model)
confusion<-table(prob>0.5,banks1$y)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 90.18% is the accuracy of the model

# create new table to store the predicted values 
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
banks1[,"prob"] <- prob
banks1[,"pred_values"] <- pred_values
banks1[,"yes_no"] <- yes_no

View(banks1[,c(2,4,7,8,11,17:20)])

table(banks1$y,banks1$pred_values)
