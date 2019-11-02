 pairs(Cigarttes[,2:8])
cor(Cigarttes[,2:8])
reg.model<-lm(Sales~Age+HS+Income+Black+Female+Price,data=Cigarttes)
summary(reg.model)
