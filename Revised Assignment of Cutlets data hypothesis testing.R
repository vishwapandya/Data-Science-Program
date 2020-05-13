#normality test -> if p value > 0.05 then it follows normal distribution
shapiro.test(Cutlets$Unit.A)
shapiro.test(Cutlets$Unit.B)

#variance test-> if p value > 0.05 then it has equal variances
var.test(Cutlets$Unit.A, Cutlets$Unit.B)

#two sample t test
t.test(Cutlets$Unit.A,Cutlets$Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)

# here the t value= 0.72287 p value = 0.4723
#this means that we fail to reject null hypothesis
# there is no significant difference in the diameter of the cutlets from 2 units. 




