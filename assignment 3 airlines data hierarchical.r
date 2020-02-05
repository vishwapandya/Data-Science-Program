df<-readxl::read_excel(file.choose("C:\\Users\\Vishwa\\Downloads\\EastWestAirlines.xlsx") , sheet='data') 
#normalization of data 
mydata<- scale(df[,2:12])
#computing distance matrix
d<- dist(mydata,method="euclidean")
#building the algorithm using centroid linkage method
fit<- hclust(d,method="average")
#display dendogram
plot(fit)
#cut into 2 clusters
groups<- cutree(fit,k=2)
#draw dendogram with red border around five clusters
rect.hclust(fit,k=2,border="blue")
#attach the cluster numbers into uni
clusters=data.frame("df"=df[,1],"cluster"=groups)
