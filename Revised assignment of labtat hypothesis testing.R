#normality test -> if p value > 0.05 then data is normally distributed
shapiro.test(LabTAT$Laboratory.1)
shapiro.test(LabTAT$Laboratory.2)
shapiro.test(LabTAT$Laboratory.3)
shapiro.test(LabTAT$Laboratory.4)

#variance test -> if p value > 0.05 then data has equal variance
var.test(LabTAT$Laboratory.1,LabTAT$Laboratory.2)
var.test(LabTAT$Laboratory.2,LabTAT$Laboratory.3)
var.test(LabTAT$Laboratory.3,LabTAT$Laboratory.4)
var.test(LabTAT$Laboratory.1,LabTAT$Laboratory.4)

Stacked_data<- stack(LabTAT)
leveneTest(values~ ind, data = Stacked_data)
Anova_results <- aov(values~ind,data = Stacked_data)
Anova_results
1-pf(79979.17/14.98631,3,476) #by doing this we find the p value 
#hence there is some significance among all the labs 