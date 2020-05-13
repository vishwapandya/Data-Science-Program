
library(wordcloud)
library("XML")
library("sqldf")
library("dplyr")
library("magrittr")
library("NLP")
library("tm")
library("tidyverse")
library("tidytext")
library("lubridate")
library("ggplot2")
FinalData<-read.csv('C:/Users/PREM/Desktop/Assignments/Project/FinalData_WithNumbers.csv', stringsAsFactors = FALSE)

str(FinalData)
summary(FinalData)

FinalData$rownum<-as.numeric(FinalData$rownum)
FinalData$answerType<-as.factor(FinalData$answerType)

FinalData$unixTime<-as.Date(FinalData$unixTime , format = "%Y-%m-%d")


#Visualization
# Multiple line plot

library('ggplot2')
library('reshape2')

df <- melt(FinalData, id.vars = c('asin', 'answerType'))  # melt data
df <- df[!is.na(FinalData$value), ]                  # remove NA
df <- with(df, aggregate(df, by = list(c(answerType,asin)), FUN = count )) # compute length by grouping variable

df<-sqldf("select asin,answerType,count(answerType) icount from FinalData group by asin,answerType")

ggplot(df, aes( x = answerType, y = icount, fill = answerType)) + 
  geom_bar(stat="identity")

df1<-sqldf("select asin,questionTypeOE,count(questionTypeOE) icount from FinalData group by asin,questionTypeOE")

ggplot(df1, aes( x = questiontypeOE, y = icount, fill = questiontypeOE)) + 
  geom_bar(stat="identity")

FinalData%>% 
  group_by(asin,answerType ) %>% 
  mutate(answerTCount=n()) %>% 
  top_n(15) %>% 
  ungroup() %>%
ggplot(aes(x = unixTime, y = answerTCount)) + 
  geom_line(aes(color = asin), size = 1) +
  facet_wrap(~asin, ncol = 2, scales = "free") +
  coord_flip()
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
  
}

# tokenize
questionWords_tokens <- FinalData %>% 
  unnest_tokens(word, questionWords) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup()

# define a nice color palette
pal <- brewer.pal(8,"Dark2")

# plot the 200 most common words
questionWords_tokens %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 200, colors=pal))

# plot the 200 most common words without work
questionWords_tokens %>% filter(word!='work') %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 200, colors=pal))


## Creating bigram
questionWords_bigrams <- FinalData %>%
  unnest_tokens(bigram, questionWords, token = "ngrams", n = 2)
view(questionWords_bigrams)
questionWords_bigrams %>% 
  count(bigram, sort = TRUE) %>%
    filter(bigram !='NA')

questionWords_bigrams %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip()

bigram_tf_idf <- questionWords_bigrams %>%
  count(asin, bigram) %>%
  bind_tf_idf(bigram, asin, n) %>%
  arrange(desc(tf_idf))


## bigram plot
Plot_bigram_Question<-bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(asin) %>% 
  top_n(15) %>% 
  ungroup() %>%head()%>%
  ggplot(aes(word, tf_idf, fill = asin)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~asin, ncol = 2, scales = "free") +
  coord_flip()


##############################################################################################3
memory.limit(size = 250000000000000000000000)
memory.limit(999999999999999999999)

QuestionWords <- Corpus(VectorSource(FinalData$questionWords)) 

QuestionWords_tdm <- TermDocumentMatrix(QuestionWords)
QuestionWords_tdm_matrix<-as.matrix(QuestionWords_tdm)

words_bar_plot(QuestionWords_tdm_matrix)

library(wordcloud)
 wordcloud(QuestionWords_tdm
           , scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

 # define a nice color palette
 pal <- brewer.pal(8,"Dark2")
 
 # plot the 50 most common words
 tokens_clean %>% 
   with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal)) 

 
 
 library(ggraph)
 library(ggforce)
 library(igraph)
 library(tidytext)
 
 
 
 final_data=read.csv('C:/Users/PREM/Desktop/Assignments/Project/FinalData_WithNumbers.csv', stringsAsFactors = FALSE)
 attach(final_data)
 qplot(final_data$sentimentA, geom="histogram", color=cyl)
 qplot(final_data$sentimentQ, geom="histogram")
 qplot(final_data$questionAve_Sentiment, geom="histogram")
 qplot(final_data$answerAve_Sentiment, geom="histogram")
 
 d <- final_data[,c(12,13,16,17)]
 res <- cor(d)
 
 library(corrplot)
 corrplot(res, type = "upper", order = "hclust", 
          tl.col = "rainbow", tl.srt = 45)
 
 library("PerformanceAnalytics")
 
 chart.Correlation(d, histogram=TRUE, pch=19)
 
 datetxt <- as.Date(final_data$unixTime)
 df <- data.frame(date = datetxt,
                  year = as.numeric(format(datetxt, format = "%Y")))
 view(df)
