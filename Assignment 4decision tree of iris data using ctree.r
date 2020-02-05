data("iris")
library(party)
inTraininglocal<-createDataPartition(iris$Species,p=.75,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]
model<- ctree(training$Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = iris)
summary(model)
plot(model)









