
library("XML")
library("sqldf")
library("dplyr")
library("magrittr")
library("NLP")
library("tm")
library("tidyverse")
library("tidytext")
library("lubridate")

data3<-read.csv('E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/data4_WithNumbers.csv', stringsAsFactors = FALSE) 

summary(data3)

data3$unixTime<-as.Date(data3$unixTime , format = "%Y-%m-%d")
data3$questiontypeOE<-as.factor(data3$questiontypeOE)
data3$answerType<-as.factor(data3$answerType)
data3$asin<-as.factor(data3$asin)

## Diving the data into dataset without answertype=?
data220<-sqldf("select rownum, asin,unixTime,questiontypeOE,question,answer,questionWords,answerWords,QuestionLength, AnswerLength,
                       questionWCount,answerWCount,questionAve_Sentiment,answerAve_Sentiment,questionsd,answersd,
                       sentimentQ,sentimentA,answerType 
              from data3 where answerType<>'?'
               order by rownum")

## Diving the data into dataset with answertype=?
data2201<-sqldf("select rownum, asin,unixTime,questiontypeOE,question,answer,questionWords,answerWords,QuestionLength, AnswerLength,
                       questionWCount,answerWCount,questionAve_Sentiment,answerAve_Sentiment,questionsd,answersd,
                       sentimentQ,sentimentA,answerType 
              from data3 where answerType='?'
                order by rownum")

data220$answerType<-as.factor(as.character(data220$answerType))
data2201$answerType<-as.factor(as.character(data2201$answerType))

data221<-sqldf("select unixTime,questiontypeOE,QuestionLength, AnswerLength,
                       questionWCount,answerWCount,questionAve_Sentiment,answerAve_Sentiment,questionsd,answersd,
                      sentimentQ,sentimentA,answerType
               from data220")
summary(data221)
str(data221)

data221<-sqldf(c("update data221 set answerType='No' where answerType='N'","select * from data221"))
data221<-sqldf(c("update data221 set answerType='Yes' where answerType='Y'","select * from data221"))
data221<-sqldf(c("update data221 set answerType='NotApp' where answerType='NaN'","select * from data221"))

data221$answerType<-as.factor(as.character(data221$answerType))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data221$QuestionLength<-normalize(data221$QuestionLength)
data221$AnswerLength<-normalize(data221$AnswerLength)
data221$questionWCount<-normalize(data221$questionWCount)
data221$answerWCount<-normalize(data221$answerWCount)

data221$questionAve_Sentiment<-normalize(data221$questionAve_Sentiment)
data221$answerAve_Sentiment<-normalize(data221$answerAve_Sentiment)
data221$questionsd <-normalize(data221$questionsd )
data221$answersd<-normalize(data221$answersd)

data221$sentimentQ <-normalize(data221$sentimentQ )
data221$sentimentA<-normalize(data221$sentimentA)


summary(data221)
str(data221)

#Checking distibution in origanl data and partitioned data
prop.table(table(data220$answerType)) * 100



#############################################################################################################
##  Sampling
############################################################################################################
seed<-1321

library(caret)
set.seed(seed)

indexes <- createDataPartition(data221$answerType, times = 1,
                               p = 0.8, list = FALSE)

train <- data221[indexes,]
test <- data221[-indexes,]

str(train)
set.seed(seed)
ctrl <- trainControl(method = "cv",number = 5,
                     summaryFunction = multiClassSummary,
                     classProbs = TRUE
                     )

model_rf1 <- caret::train(answerType ~ ., 
                          data = train,
                          method = "rf",
                          metric="ROC",
                          trControl = ctrl
                          )



ctrl$sampling <- "down"
model_rf_down <- caret::train(answerType ~ ., 
                              data = train,
                              method = "rf",
                              metric="ROC",
                              trControl = ctrl)

ctrl$sampling <- "up"
model_rf_up <- caret::train(answerType ~ ., 
                            data = train,
                            method = "rf",
                            metric="ROC",
                            trControl = ctrl)

ctrl$sampling <- "smote"
model_rf_smote <- caret::train(answerType ~ ., 
                               data = train,
                               method = "rf",
                               metric="ROC",
                               trControl = ctrl)


caret::confusionMatrix(predict(model_rf1,test[1:(length(test)-1)]), test$answerType)

           caret::confusionMatrix(predict(model_rf_down,test[1:(length(test)-1)]),test$answerType)
           caret::confusionMatrix(predict(model_rf_up,test[1:(length(test)-1)]),test$answerType)
           caret::confusionMatrix(predict(model_rf_smote,test[1:(length(test)-1)]),test$answerType)
           
          

print(model_rf1)      
print(model_rf_down)  
print(model_rf_up)    
print(model_rf_smote) 



###########################################################################
## smote was selected

## Time to select which model is accurate for classification for answertype
############################################################################
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3,
                     summaryFunction = multiClassSummary,
                     classProbs=TRUE,
                     savePredictions = TRUE,
                     sampling <- "smote"
                    )



metric <- "accuracy"
seed<-1234


# GLMNET
set.seed(seed)
fit.glmnet <- caret::train(answerType~., data=train, method="glmnet", metric=metric, trControl=ctrl)
# SVM Radial
set.seed(seed)
fit.svmRadial <- caret::train(answerType~., data=train, method="svmRadial", metric=metric, trControl=ctrl, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- caret::train(answerType~., data=train, method="knn", metric=metric, trControl=ctrl)
# Naive Bayes
set.seed(seed)
fit.nb <- caret::train(answerType~., data=train, method="nb", metric=metric, trControl=ctrl)
# CART
set.seed(seed)
fit.cart <- caret::train(answerType~., data=train, method="rpart", metric=metric, trControl=ctrl)
# C5.0
set.seed(seed)
fit.c50 <- caret::train(answerType~., data=train, method="C5.0", metric=metric, trControl=ctrl)
# Bagged CART
set.seed(seed)
fit.treebag <- caret::train(answerType~., data=train, method="treebag", metric=metric, trControl=ctrl)
# Random Forest
set.seed(seed)
fit.rf <- caret::train(answerType~., data=train, method="rf", metric=metric, trControl=ctrl)


results <- resamples(list( glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

plot(varImp(fit.rf))

#########################################################################
## Final Model is RF as it has lowest logloss value
#########################################################################
train[, !(colnames(train) %in% c("sentimentQ","answerType"))]

## smote model was selected
ctrl <- caret::trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            classProbs=TRUE,
                            savePredictions = TRUE,
                            sampling <- "smote",
                            summaryFunction = multiClassSummary
)

set.seed(1322)
mtry <- 7
tunegrid <- expand.grid(.mtry=mtry)

model_rf2 <- caret::train(answerType ~ ., 
                          data = train[, !(colnames(train) %in% c("sentimentQ"))],
                          method = "rf",
                          metric="Accuracy",
                          tuneLength  = 15,
                          trControl = ctrl,
                          tuneGrid = tunegrid)

print(model_rf2)



## implementing Model in true test. Got Accuracy of 0.9611  

caret::confusionMatrix(predict(model_rf2, test[, !(colnames(test) %in% c("sentimentQ","answerType"))]),test$answerType)

data2202<-data2201

data2202$QuestionLength<-normalize(data2202$QuestionLength)
data2202$AnswerLength<-normalize(data2202$AnswerLength)
data2202$questionWCount<-normalize(data2202$questionWCount)
data2202$answerWCount<-normalize(data2202$answerWCount)

data2202$questionAve_Sentiment<-normalize(data2202$questionAve_Sentiment)
data2202$answerAve_Sentiment<-normalize(data2202$answerAve_Sentiment)
data2202$questionsd <-normalize(data2202$questionsd )
data2202$answersd<-normalize(data2202$answersd)

data2202$sentimentQ <-normalize(data2202$sentimentQ )
data2202$sentimentA<-normalize(data2202$sentimentA)



pred<-predict(model_rf2, data2202[, !(colnames(data2202) %in% c("sentimentQ","answerType"))])

data2201$answerType<-pred



FinalData1<- sqldf("select rownum, asin,unixTime,questiontypeOE,question,answer,questionWords,answerWords,QuestionLength, AnswerLength,
                       questionWCount,answerWCount,questionAve_Sentiment,answerAve_Sentiment,questionsd,answersd,
                       sentimentQ,sentimentA,answerType  
                  from data220
                  union 
                  select rownum, asin,unixTime,questiontypeOE,question,answer,questionWords,answerWords,QuestionLength, AnswerLength,
                       questionWCount,answerWCount,questionAve_Sentiment,answerAve_Sentiment,questionsd,answersd,
                       sentimentQ,sentimentA,answerType 
                  from data2201
                  ")
FinalData<-FinalData1
FinalData<-sqldf(c("update FinalData set answerType='N' where answerType='No'","select * from FinalData"))
FinalData<-sqldf(c("update FinalData set answerType='Y' where answerType='Yes'","select * from FinalData"))
FinalData<-sqldf(c("update FinalData set answerType='NaN' where answerType='NotApp'","select * from FinalData"))

summary(FinalData)
str(FinalData)


 # write.csv(FinalData,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/FinalData_WithNumbers.csv", row.names = FALSE)
 # write.csv(FinalData1,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/FinalData1_WithNumbers.csv", row.names = FALSE)

################################## END OF SCRIPT ###############################################################

