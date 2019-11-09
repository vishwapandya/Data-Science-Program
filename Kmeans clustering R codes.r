#install package "plyr"
library(plyr)
#generating 50 random numbers from uniform distribution (0,1)
x<- runif(50)
y<- runif(50)
#combine x and y using cbind
data<- cbind(x,y)
#kmeans clustering 4 clusters; k~sqrt(n/2)
km<- kmeans(data,4)
#install package "animation"
library(animation)
#run line 13 and 14 together to see the diagram
windows()
km<- kmeans.ani(data,4)
wss<- c()
#finds distance from every data point to each other
for(i in 2:15)wss[i]<- sum(kmeans(data,centers=i)$withinss)
#to create scree plot or elbow chart to find ideal number of clusters to be made
plot(1:15,wss,type="b", xlab="no. of clusters", ylab="avg distance")

