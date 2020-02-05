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

aurl <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}

length(IMDB_reviews)

write.table(IMDB_reviews,"Aquaman.txt",row.names = F)

Aquaman <- read.delim('Aquaman.txt')
str(Aquaman)

corpus <- Aquaman[-1,]
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

cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
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


