
df<-readxl::read_excel(file.choose("C:\\Users\\Vishwa\\Downloads\\EastWestAirlines.xlsx") , sheet='data') 
#kmeans clustering 4 clusters; k~sqrt(n/2)
km<- kmeans(df,4)
#install package "animation"
library(animation)
#run line 8 and 9 together to see the diagram
windows()
km<- kmeans.ani(df,4)
wss<- c()
#finds distance from every data point to each other
for(i in 2:15)wss[i]<- sum(kmeans(df,centers=i)$withinss)
#to create scree plot or elbow chart to find ideal number of clusters to be made
plot(1:15,wss,type="b", xlab="no. of clusters", ylab="avg distance")











