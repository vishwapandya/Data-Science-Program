library(caret)
library(e1071)
library(tm)

str(sms_raw_NB)
round(prop.table(table(sms_raw_NB$type))*100, digits = 1)
sms_raw_NB$type <- factor(sms_raw_NB$type)

#cleaning the data
sms_corpus <- VCorpus(VectorSource(sms_raw_NB$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus_clean[[1]])
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
as.character(sms_corpus_clean[1:3])

#prepare document term matrix
corpus_clean <- tm_map(sms_corpus_clean, PlainTextDocument) 
dtm <- DocumentTermMatrix(sms_corpus_clean)
str(dtm)

#split the raw data
sms_train <- sms_raw_NB[1:4200, ] 
sms_test  <- sms_raw_NB[4201:5559, ] 

#split the document term matrix
dtm_train <- dtm[1:4200, ]
dtm_test  <- dtm[4201:5559, ]

#split the corpus
corpus_train <- sms_corpus_clean[1:4200]
corpus_test  <- sms_corpus_clean[4201:5559]

#check whether there are same amount of ham and spam in both training and test sets:
round(prop.table(table(sms_train$type))*100)
round(prop.table(table(sms_test$type))*100)

#remove the frequently occuring words
freq_terms <- findFreqTerms(dtm_train, 5)
reduced_dtm_train <- DocumentTermMatrix(corpus_train, list(dictionary=freq_terms))
reduced_dtm_test <- DocumentTermMatrix(corpus_test, list(dictionary=freq_terms))

#check whether we have the same amount of observations in both test and training dataset
ncol(reduced_dtm_train)
ncol(reduced_dtm_test)

#change factors into numerics
convert_counts <- function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels=c("No", "Yes"))
  return (x)
}

reduced_dtm_train = apply(reduced_dtm_train, MARGIN=2, convert_counts)
reduced_dtm_test  = apply(reduced_dtm_test, MARGIN=2, convert_counts)

#building the model
model_sms <- naiveBayes(reduced_dtm_train, sms_train$type)

#predict using the model
pred_sms <- predict(model_sms,reduced_dtm_test)

#check the first 5 values of predicted and original data 
head(pred_sms)
head(sms_test$type)
                             
#check the accuracy of the model
confusionMatrix(pred_sms,sms_test$type)
#the accuracy of the model is 97.42%
#accuracy may change because data splits randomly