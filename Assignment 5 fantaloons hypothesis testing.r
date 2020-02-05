fantaloons<-read.csv("https://excelr.s3.nl-ams.scw.cloud/DataScience-Assignments/Assignments/hypothesis%20Testing/Faltoons.csv")
x<- fantaloons$Weekdays
y<- fantaloons$Weekend

x<- as.numeric(fantaloons$Weekdays)
y<-as.numeric(fantaloons$Weekend)

t.test(x,y, alternative = 'two.sided', mu=0, var.equal = FALSE, conf.level = 0.95)
#here we see that the p value is less than 0.05 which means that we have to reject null hypothisis
#this means there is some significance between all.