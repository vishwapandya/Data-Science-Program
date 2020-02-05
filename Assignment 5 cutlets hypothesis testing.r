x<- Cutlets$Unit.A
y<- Cutlets$Unit.B
mean(x)
mean(y)
sd(x)
sd(y)
t.test(x,y,alternative = "two.sided",mu=0, var.equal = FALSE, conf.level = 0.95)
# here the t value= 0.72287 p value = 0.4723 
#this means that we fail to reject null hypothesis
# there is no significant difference in the diameter of the cutlets from 2 units. 
           