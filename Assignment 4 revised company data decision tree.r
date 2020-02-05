Bad=ifelse(Company_Data$Sales>=9,"Yes","No")
Company_Data=data.frame(Company_Data, Bad)
Company_Data=Company_Data[,-1]
inTraininglocal<-createDataPartition(Company_Data$Bad,p=.50,list = F)
training<-Company_Data[inTraininglocal,]
testing<-Company_Data[-inTraininglocal,]


model=tree(Bad~.,training)


pred=predict(model, testing, type="class")
pruned_model=prune.misclass(model, best=9)
plot(pruned_model)
text(pruned_model, pretty=0)

