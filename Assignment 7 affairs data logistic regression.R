library('AER')
library(plyr)


affairs1 <- affairs #transfer because changes are going to be made in the dataset
summary(affairs1)

table(affairs1$affairs)#shows how many values are there in how many rows eg: 0 has appeared 451 times

affairs1$ynaffairs[affairs1$affairs > 0] <- 1 # if values are greater that 0 assign 1 
affairs1$ynaffairs[affairs1$affairs == 0] <- 0 # if values are equal to 0 assign 0 

affairs1$gender <- as.factor(revalue(affairs$gender,c("male"=1, "female"=0))) # change characters to numbers 
affairs1$children <- as.factor(revalue(affairs$children,c("yes"=1, "no"=0)))

model <- glm(ynaffairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+
               education+occupation+rating, data = affairs1,family = "binomial")

#Exponentiating back the coefficients
exp(coef(model))

prob <- predict(model,affairs1,type="response")
summary(model)
confusion<-table(prob>0.5,affairs1$ynaffairs)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53% is the accuracy of the model

# create new table to store the predicted values 
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
affairs1[,"prob"] <- prob
affairs1[,"pred_values"] <- pred_values
affairs1[,"yes_no"] <- yes_no

View(affairs1[,c(1,9:11)])

table(affairs1$ynaffairs,affairs1$pred_values)
