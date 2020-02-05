library("caret")
library("randomForest")
data('iris')
model<-randomForest(iris$Species~.,data=iris,ntree=1000)
# View the forest results.
print(model)
#Imoporantce of the variable - Lower Gini
print(importance(model))
#Prediction
pred<- predict(model,iris[,-5])
print(pred)
table(pred,iris$Species)
#like this you can change the ntree value as per needed