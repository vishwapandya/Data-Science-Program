

library("XML")
library("sqldf")
library("dplyr")
library("tm")
library("tidyverse")
library("tidytext")

library("textclean")
library("qdapRegex")

library("hunspell")
###############################################################################################
## Data Loading
##############################################################################################

data=read.csv('E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/data_file.csv', stringsAsFactors = FALSE) 
str(data)
summary(data)

data$unixTime <- as.POSIXct(as.numeric(data$unixTime), origin = '1970-01-01', tz = 'GMT')
data$questionType<-as.factor(data$questionType)
data$asin <-as.factor(data$asin )
data$answerType<-as.factor(data$answerType)


###############################################################################################
## Performing EDA
##############################################################################################
data1<- data[, !(colnames(data) %in% c("answerTime"))]


## One hot Encoding with QuestionType
data1<-sqldf(c("alter table data1 add column questiontypeOE bit","select * from data1"))
data1<- sqldf(c("update data1 set questiontypeOE=1 where questiontype='open-ended'","select * from data1"))
data1<- sqldf(c("update data1 set questiontypeOE=0 where questiontype<>'open-ended'","select * from data1"))

## Removed answertime and Questiontype
data1<- data1[, !(colnames(data1) %in% c("questionType"))]


## Imputing null with Mean of unix time

data1$unixTime<-as.Date(data1$unixTime , format = "%m/%d/%y") #mdy(data4$unixTime)


data2<-sqldf("select asin,median(unixTime) MedianDate  from data1 group by asin")
data2$MedianDate<- as.Date(data2$MedianDate, origin='1970-01-01')


data1<- merge(data1,data2,by="asin")

## these asin have 2 records with median date as null. Will consider today's date a median date 
## and subtract with answertime to get a date
sqldf("select * from data1 where asin in ('B0007XD4LC','B000VNJD1S')")

sqldf("select median(unixTime) from data1")  #16126
as.Date(16126, origin='1970-01-01') #"2014-02-25"

data1<-sqldf(c("update data1 set MedianDate ='2014-02-25' where asin in ('B0007XD4LC','B000VNJD1S') and MedianDate  is null","select * from data1"))

data1<-sqldf(c("update data1 set unixTime=MedianDate where unixTime is null","select * from data1"))

data1<-data1[, !(colnames(data1) %in% c("MedianDate"))]


data1$rownum<-row.names(data1)

data1<-sqldf("select rownum, asin,unixTime,questiontypeOE,question,answerType,answer from data1")
data1$rownum<-as.integer(data1$rownum)
data1$questiontypeOE<-as.integer(data1$questiontypeOE)

summary(data1)
str(data1)

## removing extra variable 
rm(data2)

#################################################################################################
## Performing text analysis
#################################################################################################

#removing stopwords im dataframe
stopwd <- read.table('E:/Data Science/00.Excelr/PROJECT/sCRIPT/stop.txt')
stopwd <-as.character(stopwd$V1)

#stopwd<-as.character(stopwd$stopwords())

stopwd<-gsub('no','  ',stopwd)
stopwd<-gsub('yes','  ',stopwd)
#####################################################################################################
data1$questionWords<-data1$question
data1$answerWords<-data1$answer

data1$questionWords<-tolower(data1$questionWords)   
data1$answerWords<-tolower(data1$answerWords) 

## removing the Urls
data1$questionWords<-replace_url(data1$questionWords)
data1$answerWords<-replace_url(data1$answerWords)

data1$questionWords<-gsub("\\s*[^[:space:]/]+/[^[:space:]/]+","", data1$questionWords)
data1$answerWords<-gsub("\\s*[^[:space:]/]+/[^[:space:]/]+","", data1$answerWords)

##removing email
data1$questionWords<-gsub('\\S+@\\S+', '  ', data1$questionWords)
data1$answerWords<-gsub('\\S+@\\S+', '  ', data1$answerWords)

##removing words starting with @
data1$questionWords<-gsub('@\\S+', '  ', data1$questionWords)
data1$answerWords<-gsub('@\\S+', '  ', data1$answerWords)

##removing words containing .com in between and does not contain http or fpt
data1$questionWords<-gsub('\\S+.com+\\s', '  ', data1$questionWords)
data1$questionWords<-gsub('\\S+.org+\\s', '  ', data1$questionWords)
data1$questionWords<-gsub('\\S+.eu+\\s', '  ', data1$questionWords)

data1$answerWords<-gsub('\\S+.com+\\s', '  ', data1$answerWords)
data1$answerWords<-gsub('\\S+.org+\\s', '  ', data1$answerWords)
data1$answerWords<-gsub('\\S+.eu+\\s', '  ', data1$answerWords)

##removing words ending with .com and does not contain http or fpt
data1$questionWords<-gsub('\\S+\\.com', '  ', data1$questionWords)
data1$questionWords<-gsub('\\S+\\.org', '  ', data1$questionWords)
data1$questionWords<-gsub('\\S+\\.eu', '  ', data1$questionWords)

data1$answerWords<-gsub('\\S+\\.com', '  ', data1$answerWords)
data1$answerWords<-gsub('\\S+\\.org', '  ', data1$answerWords)
data1$answerWords<-gsub('\\S+\\.eu', '  ', data1$answerWords)

##removing word .com and does not contain http or fpt
data1$questionWords<-gsub('\\S+\\s.com', '  ',data1$questionWords)
data1$questionWords<-gsub('\\S+\\s.org', '  ',data1$questionWords)
data1$questionWords<-gsub('\\S+\\s.eu', '  ',data1$questionWords)

data1$answerWords<-gsub('\\S+\\s.com', '  ',data1$answerWords)
data1$answerWords<-gsub('\\S+\\s.org', '  ',data1$answerWords)
data1$answerWords<-gsub('\\S+\\s.eu', '  ',data1$answerWords)


##removing word/ and does not contain http or fpt
data1$questionWords<-gsub('\\S+/', '  ', data1$questionWords)
data1$answerWords<-gsub('\\S+/', '  ', data1$answerWords)

##removing /word and does not contain http or fpt
data1$questionWords<-gsub('/\\S+', '  ', data1$questionWords)
data1$answerWords<-gsub('/\\S+', '  ', data1$answerWords)

##removing word&amp;word nd does not contain http or fpta
data1$questionWords<-gsub('\\S+&amp;\\S+', '  ', data1$questionWords)
data1$answerWords<-gsub('\\S+&amp;\\S+', '  ', data1$answerWords)

#replace_html(data1$questionWords,FALSE)

##removing word&amp; and does not contain http or fpt
data1$questionWords<-str_replace_all(data1$questionWords, "&amp;", " ")
data1$answerWords<-str_replace_all(data1$answerWords, "&amp;", " ")

##removing word&word
data1$questionWords<-gsub('\\S+&\\S+', '  ', data1$questionWords)
data1$answerWords<-gsub('\\S+&\\S+', '  ', data1$answerWords)

##removing &word; 
data1$questionWords<-gsub('&\\S+;', '  ', data1$questionWords)
data1$answerWords<-gsub('&\\S+;', '  ', data1$answerWords)



##removing combination of character and number as a word. It is genearlly serial number or model name.
data1$questionWords<-gsub('[a-z]+[0-9]+\\S+', '  ', data1$questionWords)
data1$answerWords<-gsub('[a-z]+[0-9]+\\S+', '  ', data1$answerWords)

##removing word-number
data1$questionWords<-gsub('\\S+\\-+[0-9]+', '  ', data1$questionWords)
data1$answerWords<-gsub('\\S+\\-+[0-9]+', '  ', data1$answerWords)

##removing number-word
data1$questionWords<-gsub('[0-9]+-\\S+', '  ', data1$questionWords)
data1$answerWords<-gsub('[0-9]+-\\S+', '  ', data1$answerWords)

##removing word#
data1$questionWords<-gsub('([a-z]+#)', '  ', data1$questionWords)
data1$answerWords<-gsub('([a-z]+#)', '  ', data1$answerWords)


## Removing Data frame 
data1$questionWords <- removeWords(data1$questionWords, stopwd)
data1$answerWords <- removeWords(data1$answerWords, stopwd)

#Removing numbers
#data1$questionWords <- removeNumbers(data1$questionWords)
#data1$answerWords <- removeNumbers(data1$answerWords)

## Removing punctuation mark 
#data1$questionWords<-removePunctuation(data1$questionWords)
#data1$answerWords<-removePunctuation(data1$answerWords)

data1$questionWords<-gsub('[[:punct:]]+','  ',data1$questionWords)
data1$answerWords<-gsub('[[:punct:]]+','  ',data1$answerWords)

#remove single letter words
data1$questionWords <- rm_nchar_words(data1$questionWords, "1,1")
data1$answerWords <- rm_nchar_words(data1$answerWords, "1,1")

#removing whitespace 
data1$questionWords <- stripWhitespace(data1$questionWords)
data1$answerWords <- stripWhitespace(data1$answerWords)

# replacing certain words
data1$questionWords<-str_replace_all(data1$questionWords, " wi fi ", " wifi ")
data1$questionWords<-str_replace_all(data1$questionWords, " aca ", " ")
data1$questionWords<-str_replace_all(data1$questionWords, " & ", "and")
data1$questionWords<-str_replace_all(data1$questionWords, " aaaaarge ", " ")

data1$answerWords<-str_replace_all(data1$answerWords, " wi fi ", " wifi ")
data1$answerWords<-str_replace_all(data1$answerWords, " aca ", " ")
data1$answerWords<-str_replace_all(data1$answerWords, " & ", "and")
data1$answerWords<-str_replace_all(data1$answerWords, " aaaaarge ", " ")

## word stemming
library(textstem)
data1$questionWords<-textstem::lemmatize_strings(data1$questionWords)
data1$answerWords<-textstem::lemmatize_strings(data1$answerWords)

str(data1)

## sqldf("select questionWords from data1 where questionWords like '%iphone 4s%' ")

## correcting misspelled word and replacing with correct one.

# vector of words to replace
wrong <- unlist(hunspell(data1$questionWords))

# vector of the first suggested words
correct <- data.frame(wrong,sapply(wrong, function(x) hunspell_suggest(x)[[1]][1]))
colnames(correct)<-c("wrong","correct")


# vector of words to replace
wrongA <- unlist(hunspell(data1$answerWords))
# vector of the first suggested words
correctA <- data.frame(wrongA,sapply(wrongA, function(x) hunspell_suggest(x)[[1]][1]))
colnames(correctA)<-c("wrong","correct")

correct2<-sqldf("select distinct wrong,correct from correct where wrong<>lower(correct)
                and length(lower(correct))>3
                and wrong not in ('korea','vesa','tv','lcd','visio','tvs','ereader','gmail','froyo','bootable','roms','hotspots','ereaders','decrypt','calibri','nikons','quora','sony','peru','photoplaceonlinecom','iphones','jitter','httplandelcom','teleconverter','macbooks','airbooks','ipads','innerfidelitycom','flickr','modi','phillips','mids','wlens','stationsis','lowepro','unshielded','wi','eq','fi','talkabout','talkabouts','hulu','ebooks','ebook','epub','hd','vhs','ethernet','mh','youtube','xbox','motorola','asus','acer','ipad','xp','cd','pc','usb','vizio','wifi','gxt','magnavox','vx','macbook','airbook','gb','aaabu','aac','aas','Aastra','aaxa','abel','abesofmaine','abf','abit','absorbers','abu')

                 union
                select distinct wrong,correct from correctA where wrong<>lower(correct)
                and length(lower(correct))>3
                and wrong not in ('korea','vesa','tv','lcd','visio','tvs','ereader','gmail','froyo','bootable','roms','hotspots','ereaders','decrypt','calibri','nikons','quora','sony','peru','photoplaceonlinecom','iphones','jitter','httplandelcom','teleconverter','macbooks','airbooks','ipads','innerfidelitycom','flickr','modi','phillips','mids','wlens','stationsis','lowepro','unshielded','wi','eq','fi','talkabout','talkabouts','hulu','ebooks','ebook','epub','hd','vhs','ethernet','mh','youtube','xbox','motorola','asus','acer','ipad','xp','cd','pc','usb','vizio','wifi','gxt','magnavox','vx','macbook','airbook','gb','aaabu','aac','aas','Aastra','aaxa','abel','abesofmaine','abf','abit','absorbers','abu')
        
                ")

#install.packages("RecordLinkage")
library(RecordLinkage)

correct2<-mutate(correct2, weight = jarowinkler(correct,wrong))

summary(correct2)



correct2<-sqldf(c("update correct2 set correct='hewlett packward' where wrong='hewlettpackward'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='its a wrap' where wrong='itsawrap'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='abc cbs' where wrong='abccbs'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='blue ray disc' where wrong='blueraydisc'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='bose enjoy' where wrong='boseenjoy'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='bsd vms' where wrong='bsdvms'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='day sony' where wrong='daysony'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='dlp link' where wrong='dlplink'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='samsung' where wrong='samsund'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='san disk extreme' where wrong='sandiskextreme'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='samsung' where wrong='samsungs'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='sony' where wrong='soni'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='netflix' where wrong='netfilx'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='of the' where wrong='ofnthe'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='cosine' where wrong='cosina'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct=' ' where wrong='arrrrgh'","select * from correct2"))


correct2<-sqldf(c("update correct2 set correct=' ' where wrong='aaaaarge'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='aaaaarge'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='arrrrgh'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='nivida'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='nvida'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='nvidea'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='nvidida'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='nvida'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='ofnthe'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='hewlettpackward'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='itsawrap'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='abccbs'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='blueraydisc'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='boseenjoy'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='bsdvms'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='daysony'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='cosina'","select * from correct2"))

correct2<-sqldf(c("update correct2 set weight=91 where wrong='dlplink'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='samsund'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='samsungs'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='soni'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='netfilx'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='arrrrgh'","select * from correct2"))



correct2<-sqldf(c("update correct2 set correct ='aaa batteries' where wrong='aaabatteries'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='aaa' where wrong='aaas'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='aa battery' where wrong='aabattery'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='' where wrong='aaclc'","select * from correct2"))

correct2<-sqldf(c("update correct2 set weight=91 where wrong='aaabatteries'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='aaas'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='aabattery'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='aaclc'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='ablumsmus'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='abled'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='ablet'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct ='ablums music' where wrong='ablumsmus'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='able' where wrong='abled'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='tablet' where wrong='ablet'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='about' where wrong='abot'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='about' where wrong='abt'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='about' where wrong='abu'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='abuse' where wrong='abus'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='a cabinet' where wrong='acabinet'","select * from correct2"))
correct<-sqldf(c("update correct2 set correct ='black' where wrong='blak'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='pause' where wrong='paus'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct ='foldable' where wrong='foldabl'","select * from correct2"))

correct2<-sqldf(c("update correct2 set weight=91 where wrong='abot'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='abt'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='abu'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='abus'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='acabinet'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='blak'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='paus'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='foldabl'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='i am' where wrong='im'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='i have' where wrong='ive'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='this' where wrong='ths'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='there' where wrong='ther'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='behind the head' where wrong='behindthehead'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='iphone but' where wrong='iphonebut'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='kodak' where wrong='kadak'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='multi coating please' where wrong='multicoatingpls'","select * from correct2"))

correct2<-sqldf(c("update correct2 set weight=91 where wrong='im'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='ive'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='ths'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='ther'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='behindthehead'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='iphonebut'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='kadak'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='multicoatingpls'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='shoul'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='should' where wrong='shoul'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='because' where wrong='cuz'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='high definition' where wrong='hidef'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='strong' where wrong='stong'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='wifi' where wrong='wifes'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='sony' where wrong='sonys'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='black on white' where wrong='blackonwhite'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='bought' where wrong='baught'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='putting them' where wrong='puttingbthem'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='is not' where wrong='isnt'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='are not' where wrong='arent'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='xda developers' where wrong='xdadevelopers'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='was not' where wrong='wasnt'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='low end' where wrong='lowend'","select * from correct2"))

correct2<-sqldf(c("update correct2 set weight=91 where wrong='cuz'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='hidef'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='stong'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='wifes'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='sonys'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='blackonwhite'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='baught'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='puttingbthem'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='isnt'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='arent'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='xdadevelopers'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='wasnt'","select * from correct2"))
correct2<-sqldf(c("update correct2 set weight=91 where wrong='lowend'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='reconnection' where wrong='reconnation'","select * from correct2"))  
correct2<-sqldf(c("update correct2 set correct='documentation' where wrong='documention'","select * from correct2")) 
correct2<-sqldf(c("update correct2 set correct='aberrations' where wrong='abberations'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='transmitter' where wrong='transmitor'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='distributor' where wrong='distributer'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='compartible' where wrong='compartable'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='vignetting' where wrong='vigneting'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='silverado' where wrong='sliverado'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='routable' where wrong='routeable'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='programmed' where wrong='programme'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='pre select' where wrong='preselect'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='practically' where wrong='practicaly'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='journal' where wrong='journaled'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='hard drives' where wrong='hardrives'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='exactly' where wrong='exactally'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='damage done' where wrong='damagedon'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='cover' where wrong='coverable'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='conformity' where wrong='conformit'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='cigarette' where wrong='cigarrate'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='bluetooth' where wrong='blurtooth'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='acessable' where wrong='acessable'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='upconvert' where wrong='upconverting'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='reassign' where wrong='reassignable'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='dimmention' where wrong='dimmention'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='widestit' where wrong='widestit'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='under seat' where wrong='underseat'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='transparent' where wrong='transpar'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='theatre' where wrong='threatre'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='stacks up' where wrong='stacksup'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='signal to' where wrong='signalto'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='should have' where wrong='shouldve'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='should not' where wrong='shouldnt'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='repost' where wrong='reposting'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='perform' where wrong='performce'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='pass through' where wrong='passthru'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='parthner' where wrong='parthners'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='novastorage' where wrong='novastor'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='multiple' where wrong='multipal'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='logitech' where wrong='logetech'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='leakage is' where wrong='leakageis'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='latest' where wrong='latelest'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='interface' where wrong='interace'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='enclosures' where wrong='incluser'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='heat sink' where wrong='heatsunk'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='heaven' where wrong='heareven'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='hard drive' where wrong='hardrive'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='hack' where wrong='hackable'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct=' ' where wrong='guruthat'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='converter' where wrong='covertor'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='bluetooth' where wrong='bluetoot'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='bluetooth' where wrong='bluetooh'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='biostar' where wrong='biosstar'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='bezeled' where wrong='bezelled'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='batteries' where wrong='batterier'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='aperture' where wrong='aperature'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='netflix' where wrong='netflicks'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='degradation' where wrong='degration'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='dissipates' where wrong='disipates'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='cancation' where wrong='cancation'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='main boards' where wrong='mainboards'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='independent' where wrong='independed'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='free format' where wrong='frreformat'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='caretake' where wrong='caretaking'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='would not' where wrong='wouldnt'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='would not' where wrong='woulden'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='reference' where wrong='referen'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='motorola' where wrong='motorla'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='logitech' where wrong='logetic'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='length' where wrong='lentnth'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='insetter' where wrong='inseter'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='freaking' where wrong='freekin'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='even though' where wrong='eventho'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='equipment' where wrong='equipen'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='distributor' where wrong='distrib'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='dimension' where wrong='dinsion'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='connector' where wrong='conetor'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='coaxial' where wrong='coaxell'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='cinemine' where wrong='cinemin'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='chrome os' where wrong='chromeos'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='built' where wrong='builted'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='assonance' where wrong='assocan'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='alloted' where wrong='allotrd'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='pre configured' where wrong='preconfigured'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='usable' where wrong='usesable'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='remplazar' where wrong='remplaza'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='portfolio' where wrong='profolio'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='panasonic' where wrong='pansonic'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='reading' where wrong='iireading'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='willing' where wrong='willjg'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='were not' where wrong='werent'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='they have' where wrong='theyve'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='stephen' where wrong='stepeh'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='samerth' where wrong='samert'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='european' where wrong='ropean'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='refills' where wrong='refils'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='properly' where wrong='propely'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='product' where wrong='produc'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='portal' where wrong='portta'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='panels' where wrong='planels'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='must have' where wrong='mustve'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='pinterest' where wrong='pintrest'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='micro sd' where wrong='microsd'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='knowbility' where wrong='knowbi'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='does not' where wrong='doesnt'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='directly' where wrong='direcly'","select * from correct2"))

correct2<-sqldf(c("update correct2 set correct='driver' where wrong='deriver'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='delilah' where wrong='delila'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='datebook' where wrong='datebk'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='control' where wrong='contral'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='comments' where wrong='coments'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='cellular' where wrong='celluar'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='battery' where wrong='bettery'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='battery' where wrong='batteri'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='actuary' where wrong='actuay'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='windows' where wrong='windoze'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='waste' where wrong='waisting'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='surround' where wrong='sourand'","select * from correct2"))
correct2<-sqldf(c("update correct2 set correct='iphone' where wrong='siphone'","select * from correct2"))

correct2<-sqldf(c("update correct2 set weight=91 where wrong='siphone'","select * from correct2"))




correct2<-sqldf("select * from correct2 where wrong not in ('reflectant','stokesdale','ultrasones','unpackaged','updateable','withington',
                                                              'reinstallation','remanufacturer','reprogrammable','compensator','installable',
                                                              'overnighter','overnighted','btci','synergistically','transmissive','removalable',
                                                               'microphonics','upgradeable','scrapbooker','dimensionally','directionally','panoramically',
                                                              'apgl','khzstep','novoflex','waterfi','acli','huluplus','booksamillion','chirstmas',
                                                              'adjustability','reefscapes','recordable','realplayer','chromecast','assessable',
                                                              'recalibration','remanufacture','unfixable','unmanaged','upscalers','technotom','superflex',
                                                              'superacct','silverado','silverwing','sidewards','rockfords','ridgeline','razorfish',
                                                              'pluggable','phonelynx','honestech','grommeted','globalsat','fanboyism','downscaler',
                                                              'dismantler','delidding','crossfirex','contraire','clamshell','bloomberg','beaverton',
                                                              'avermedia','aurasound','alienware','upconversion','redownloaded','reconnection','pixelation',
                                                              'altazimuth','wolfdale','yikesboy','trayless','traysoft','trendnet','trimline',
                                                              'tripmate','toshlink','timelinex','swapable','superlux','staticky','starflex',
                                                              'starmate','stagepas','springer','springer','spinrite','speakeas','sightsee','routable','shoretel',
                                                              'retropie','relinking','regrette','recertified','recalibrate','rebuffering','presonus','',
                                                              'plugable','pluglink','chemex','peelable','pearland','neighbour','millbrae','maxiaids',
                                                              'lavalier','justrite','jumpered','infotech','inductor','greenlee','gasketed','filmless','excelvan'
                                                              ,'excelsus','delidded','dearborn','dashcams','comptech','cardioid','burgmans','anisotropic',
                                                              'analytics','pixelated','daylilies','chimaster','arresters','upsampling','upgradable','upconverts',
                                                              'rebranding','redownload','replugging','rescanning','resettable','respotting','retransmit','rewatching',
                                                              'rewritable','innumerous','westell','undidad','softail','slidden','satechi','reposted','novatel',
                                                              'listado','insteon','imposter','eruptor','epicture','enquire','clemente','ceruelan','burgman',
                                                              'brannon','berthor','basterds','attacher','ableton','ablenet','youtubes','eveready','comcasts',
                                                              'retighten','mainboard','preformatted','preassembled','tintin','saleem','plentum','pentex',
                                                              'peltor','nailah','muslins','millan','malelo','loompa','kinect','jointer','haveta','diddley',
                                                              'detent','bytecc','appletv','amperex','activex','accesos','zencast','zenbook','yealink','youview',
                                                              'winbook','warhawk','voltech','uniloop','unibody','tomtoms','tiltall','tenemos','telstar','telgrad',
                                                              'telespy','sunbird','samsung','adorams','aircard')")


## final table of wrong and correct words
correct3<-sqldf("select * from correct2 where weight>=0.971428571428571")

correct3$correct<-tolower(correct3$correct)

sqldf("select * from correct3 where wrong like '%sony%'")

str(data1)

sqldf("select questionWords from data1 where question like '%samsung%'")


# write.csv(correct2,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/correct2.csv", row.names = FALSE)

# write.csv(correct3,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/correct3.csv", row.names = FALSE)

# write.csv(data1,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/Data/Data/data1.csv", row.names = FALSE)



library(DataCombine)
## Correcting the incorrect word
data1<-FindReplace(data1, Var="questionWords", correct3, from = "wrong", to = "correct" ,
                   exact = FALSE, vector = FALSE)


data1<-FindReplace(data1, Var="answerWords", correct3, from = "wrong", to = "correct" ,
                   exact = FALSE, vector = FALSE)


sqldf("select question  from data1 where questionWords =''")


data2<-sqldf(c("update data1 set questionWords=question where questionWords =''","select * from data1"))
#data2$questionWords <- removeWords(data2$questionWords, stopwd)


data2$questionWords<-tolower(data2$questionWords)   


## removing the Urls
data2$questionWords<-replace_url(data2$questionWords)


#data2$questionWords<-gsub("\\s*[^[:space:]/]+/[^[:space:]/]+","", data2$questionWords)


##removing email
data2$questionWords<-gsub('\\S+@\\S+', '  ', data2$questionWords)


##removing words starting with @
data2$questionWords<-gsub('@\\S+', '  ', data2$questionWords)

##removing words containing .com in between and does not contain http or fpt
data2$questionWords<-gsub('\\S+.com+\\s', '  ', data2$questionWords)
data2$questionWords<-gsub('\\S+.org+\\s', '  ', data2$questionWords)
data2$questionWords<-gsub('\\S+.eu+\\s', '  ', data2$questionWords)



##removing words ending with .com and does not contain http or fpt
data2$questionWords<-gsub('\\S+\\.com', '  ', data2$questionWords)
data2$questionWords<-gsub('\\S+\\.org', '  ', data2$questionWords)
data2$questionWords<-gsub('\\S+\\.eu', '  ', data2$questionWords)


##removing word .com and does not contain http or fpt
data2$questionWords<-gsub('\\S+\\s.com', '  ',data2$questionWords)
data2$questionWords<-gsub('\\S+\\s.org', '  ',data2$questionWords)
data2$questionWords<-gsub('\\S+\\s.eu', '  ',data2$questionWords)




##removing word/ and does not contain http or fpt
data2$questionWords<-gsub('\\S+/', '  ', data2$questionWords)


##removing /word and does not contain http or fpt
data2$questionWords<-gsub('/\\S+', '  ', data2$questionWords)


##removing word&amp;word nd does not contain http or fpta
data2$questionWords<-gsub('\\S+&amp;\\S+', '  ', data2$questionWords)


#replace_html(data1$questionWords,FALSE)

##removing word&amp; and does not contain http or fpt
data2$questionWords<-str_replace_all(data2$questionWords, "&amp;", " ")


##removing word&word
data2$questionWords<-gsub('\\S+&\\S+', '  ', data2$questionWords)


##removing &word; 
data2$questionWords<-gsub('&\\S+;', '  ', data2$questionWords)


##removing word#
data2$questionWords<-gsub('([a-z]+#)', '  ', data2$questionWords)


data2$questionWords<-gsub('[[:punct:]]+','  ',data2$questionWords)

#remove single letter words
#data2$questionWords <- rm_nchar_words(data2$questionWords, "1,1")


#removing whitespace 
data2$questionWords <- stripWhitespace(data2$questionWords)



data2$QuestionLength<-nchar(data2$question)
data2$AnswerLength<-nchar(data2$answer)
 
 library(stringr)
data2$questionWCount<-str_count(data2$question,'\\w+')
data2$answerWCount<-str_count(data2$answer,'\\w+')

library(sentimentr)
summary(data2)



questionSentiment<-sentiment_by(data2$question, by = NULL)
answerSentiment<-sentiment_by(data2$answer, by = NULL)

data2$questionAve_Sentiment<-questionSentiment$ave_sentiment
data2$answerAve_Sentiment<-answerSentiment$ave_sentiment

data2$questionsd<-questionSentiment$sd
data2$answersd<-answerSentiment$sd


sqldf("select median(questionsd) from data2") #0.1020621
sqldf("select median(answersd) from data2") #0.1833333

#median of questionsd is 0.1020621. Imputing this with na values. 
data2<-sqldf(c("update data2 set questionsd = 0.1020621  where questionsd is null","select * from data2"))

#median of answersd is 0.1833333. Imputing this with na values. 
data2<-sqldf(c("update data2 set answersd = 0.1833333  where answersd is null","select * from data2"))


# write.csv(data1,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/data1.csv", row.names = FALSE)

# write.csv(data2,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/data2.csv", row.names = FALSE)


data3<-data2 %>% unnest_tokens(word, question) %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(rownum,sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words
colnames(data3)<-c("rownum","negativeQ","positiveQ","sentimentQ")


data4<-merge(x = data2, y = data3, by = "rownum", all.x = TRUE)

data4$negativeQ[is.na(data4$negativeQ)] <- 0
data4$positiveQ[is.na(data4$positiveQ)] <- 0
data4$sentimentQ[is.na(data4$sentimentQ)] <- 0

data3<-data2 %>% unnest_tokens(word, answer) %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(rownum,sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words
colnames(data3)<-c("rownum","negativeA","positiveA","sentimentA")


data4<-merge(x = data4, y = data3, by = "rownum", all.x = TRUE)

data4$negativeA[is.na(data4$negativeA)] <- 0
data4$positiveA[is.na(data4$positiveA)] <- 0
data4$sentimentA[is.na(data4$sentimentA)] <- 0

summary(data4)

data4<- data4[, !(colnames(data4) %in% c("negativeQ","positiveQ","negativeA","positiveA"))]

# write.csv(correct2,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/correct2_WithNumbers.csv", row.names = FALSE)

# write.csv(correct3,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/correct3_WithNumbers.csv", row.names = FALSE)

# write.csv(data1,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/data1_WithNumbers.csv", row.names = FALSE)

# write.csv(data2,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/data2_WithNumbers.csv", row.names = FALSE)

# write.csv(data4,"E:/Data Science/00.Excelr/PROJECT/Final Data with Text mining in r/00. Latest Final Script/Data/data4_WithNumbers.csv", row.names = FALSE)

# rm(data4)
################################## END OF SCRIPT ###############################################################

