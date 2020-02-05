#first: do the pca process
pca<-princomp(wine[,2:14], cor = TRUE,scores = TRUE, covmat = NULL)
summary(pca)
pca$scores
pca$loadings #weights to multiply with each column to get pca score

#second: do hierarchical clustering
mydata<- scale(wine[,2:14])
#computing distance matrix
d<- dist(mydata,method="euclidean")
#building the algorithm using centroid linkage method
fit<- hclust(d,method="average")
#display dendogram
windows()
plot(fit)
#cut into 4 clusters
groups<- cutree(fit,k=4)
#draw dendogram with red border around five clusters
rect.hclust(fit,k=4,border="blue")
#attach the cluster numbers 
clusters=data.frame("type"=wine[,1],"cluster"=groups)

#third: do kmeans clustering
#kmeans clustering 4 clusters; k~sqrt(n/2)
km<- kmeans(wine,4)
#install package "animation"
library(animation)
#run line 25 and 26 together to see the diagram
windows()
km<- kmeans.ani(wine,4)
wss<- c()
#finds distance from every data point to each other
for(i in 2:15)wss[i]<- sum(kmeans(wine,centers=i)$withinss)
#to create scree plot or elbow chart to find ideal number of clusters to be made
plot(1:15,wss,type="b", xlab="no. of clusters", ylab="avg distance")

# as you can see from the elbow chart you can see that after 4 number of clusters it is not optimum to go any lower
#so 4 is the ideal number of clusters needed 