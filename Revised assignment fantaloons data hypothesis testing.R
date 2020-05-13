table1<- table(Faltoons$Weekdays, Faltoons$Weekend)
table1
prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95,correct = FALSE, alternative = "two.sided")
#p value greater than 0.05 therefore fail to reject null hypothesis

prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95,correct = FALSE, alternative = "less")
#p value greater than 0.05 therefore fail to reject null hypothesis