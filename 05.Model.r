



library("XML")
library("sqldf")
library("dplyr")
library("tm")
library("tidyverse")
library("tidytext")
library("hunspell")

FinalData=read.csv('E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/FinalData_WithNumbers.csv', stringsAsFactors = FALSE) 

str(FinalData)
summary(FinalData)


FinalData$unixTime<-as.Date(FinalData$unixTime , format = "%Y-%m-%d")

FinalData <- FinalData %>% 
  mutate(rowIndex=as.numeric(row.names(.)))

str(FinalData)

## Loading stopwords to correct user input
stopwd <- read.table('E:/Data Science/00.Excelr/PROJECT/sCRIPT/stop.txt')
stopwd <-as.character(stopwd$V1)

stopwd<-gsub('no','  ',stopwd)
stopwd<-gsub('yes','  ',stopwd)


## Loading correct spell check for user input
correct3=read.csv('E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/correct3_WithNumbers.csv', stringsAsFactors = FALSE) 



## user input




#### Start of the function ###
QuestionList <- as.list(FinalData$questionWords)
 QuestionLength <- length(QuestionList)


 QuestionSearch <- function(UserQuestion) {

   UserQuestion<-CleanText_Analysis(UserQuestion,correct3)

  TotalQuestions <- VectorSource(c(QuestionList, UserQuestion))
  
  
  QuestionCorpus <- VCorpus(TotalQuestions) %>% 
    tm_map(stemDocument) %>%
    tm_map(stripWhitespace)
  
  
  term.doc.matrix.stm <- TermDocumentMatrix(QuestionCorpus,
                                            control=list(
                                              weighting=function(x) weightSMART(x,spec="ltc"),
                                              wordLengths=c(1,Inf)))
  
  
  # QuestionSearch <- function(UserQuestion) {
  term.doc.matrix <- tidy(term.doc.matrix.stm) %>% 
    group_by(document) %>% 
    mutate(vtrLen=sqrt(sum(count^2))) %>% 
    mutate(count=count/vtrLen) %>% 
    ungroup() %>% 
    select(term:count)
  
  docMatrix <- term.doc.matrix %>% 
    mutate(document=as.numeric(document)) %>% 
    filter(document<QuestionLength+1)
  QuestionMatrix <- term.doc.matrix %>% 
    mutate(document=as.numeric(document)) %>% 
    filter(document>=QuestionLength+1)
  
  

  
  Wordsearch <- docMatrix %>% 
    inner_join(QuestionMatrix,by=c("term"="term"),
               suffix=c(".doc",".query")) %>% 
    mutate(termScore=round(count.doc*count.query,4)) %>% 
    group_by(document.query,document.doc) %>% 
    summarise(Score=sum(termScore)) %>% 
    filter(row_number(desc(Score))<=5) %>% 
    arrange(desc(Score))%>% 
    left_join(FinalData,by=c("document.doc"="rowIndex")) %>% 
    ungroup() %>% 
    select(question,answer) %>% 
    data.frame()
  
 
  return(Wordsearch)
  
 }
