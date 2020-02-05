#normalization of data 
mydata<- scale(crime_data[,2:5])
#computing distance matrix
d<- dist(mydata,method="euclidean")
#building the algorithm using centroid linkage method
fit<- hclust(d,method="centroid")
#display dendogram
plot(fit)
#cut into 5 clusters
groups<- cutree(fit,k=5)
#draw dendogram with red border around five clusters
rect.hclust(fit,k=5,border="red")
#attach the cluster numbers into uni
clusters=data.frame("crime_data"=crime_data[,1],"cluster"=groups)
