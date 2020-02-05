library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(dplyr)

aurl <- "https://www.amazon.com/Bluetooth-Headphones-Waterproof-Sweatproof-Cancelling/product-reviews/B01G8JO5F2/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"

amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)

write.table(amazon_reviews,"amazon_reviews.txt",row.names = F)

amazon_reviews <- read.delim('amazon_reviews.txt')
str(amazon_reviews)

corpus <- amazon_reviews[-1,]
head(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
corpus_clean<-tm_map(corpus,stripWhitespace)
cleanset<-tm_map(corpus,removeWords, stopwords('english'))

inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

cleanset<-tm_map(cleanset,removeWords, c('can','earphones'))
cleanset<-tm_map(cleanset,removeWords, c('price','headphones'))
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)

tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
