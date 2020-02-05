labtat<-read.csv("https://excelr.s3.nl-ams.scw.cloud/DataScience-Assignments/Assignments/hypothesis%20Testing/LabTAT.csv")
a<-labtat$Laboratory.1
b<-labtat$Laboratory.2
c<-labtat$Laboratory.3
d<-labtat$Laboratory.4
t.test(a,alternative = "two.sided",mu=0,var.equal = FALSE,conf.level = 0.95)

t.test(b,alternative = "two.sided",mu=0,var.equal = FALSE,conf.level = 0.95)

t.test(c,alternative = "two.sided",mu=0,var.equal = FALSE,conf.level = 0.95)

t.test(d,alternative = "two.sided",mu=0,var.equal = FALSE,conf.level = 0.95)
#here all the labs have p value less tha 0.05 so we have to reject null hypothesis
#that means there is some significance between all.