
## Function for User input Analysis
CleanText_Analysis <- function(textInput,correctSpelling_df) {
  
  ## textInput <- User Input
  ## correctSpelling_df <- Data Frame containing wrong words and its corresponding correct words
  
  
  textInput<-tolower(textInput)   
  
  
  ## removing the Urls
  textInput<-replace_url(textInput)
  
  
  textInput<-gsub("\\s*[^[:space:]/]+/[^[:space:]/]+","", textInput)
  
  
  ##removing email
  textInput<-gsub('\\S+@\\S+', '  ', textInput)
  
  
  ##removing words starting with @
  textInput<-gsub('@\\S+', '  ', textInput)
  
  
  ##removing words containing .com in between and does not contain http or fpt
  textInput<-gsub('\\S+.com+\\s', '  ', textInput)
  textInput<-gsub('\\S+.org+\\s', '  ', textInput)
  textInput<-gsub('\\S+.eu+\\s', '  ', textInput)
  
  
  ##removing words ending with .com and does not contain http or fpt
  textInput<-gsub('\\S+\\.com', '  ', textInput)
  textInput<-gsub('\\S+\\.org', '  ', textInput)
  textInput<-gsub('\\S+\\.eu', '  ', textInput)
  
  
  ##removing word .com and does not contain http or fpt
  textInput<-gsub('\\S+\\s.com', '  ',textInput)
  textInput<-gsub('\\S+\\s.org', '  ',textInput)
  textInput<-gsub('\\S+\\s.eu', '  ',textInput)
  
  
  
  ##removing word/ and does not contain http or fpt
  textInput<-gsub('\\S+/', '  ', textInput)
  
  
  ##removing /word and does not contain http or fpt
  textInput<-gsub('/\\S+', '  ', textInput)
  
  ##removing word&amp;word nd does not contain http or fpta
  textInput<-gsub('\\S+&amp;\\S+', '  ', textInput)
  
  #replace_html(textInput,FALSE)
  
  ##removing word&amp; and does not contain http or fpt
  textInput<-str_replace_all(textInput, "&amp;", " ")
  
  ##removing word&word
  textInput<-gsub('\\S+&\\S+', '  ', textInput)
  
  ##removing &word; 
  textInput<-gsub('&\\S+;', '  ', textInput)
  
  
  ##removing combination of character and number as a word. It is genearlly serial number or model name.
  textInput<-gsub('[a-z]+[0-9]+\\S+', '  ', textInput)
  
  
  ##removing word-number
  textInput<-gsub('\\S+\\-+[0-9]+', '  ', textInput)
  
  
  ##removing number-word
  textInput<-gsub('[0-9]+-\\S+', '  ', textInput)
  
  
  ##removing word#
  textInput<-gsub('([a-z]+#)', '  ', textInput)
  
  
  
  ## Removing Data frame 
  textInput <- removeWords(textInput, stopwd)
  
  
  
  
  textInput<-gsub('[[:punct:]]+','  ',textInput)
  
  
  #remove single letter words
  textInput <- rm_nchar_words(textInput, "1,1")
  
  
  #removing whitespace 
  textInput <- stripWhitespace(textInput)
  
  
  # replacing certain words
  textInput<-str_replace_all(textInput, " wi fi ", " wifi ")
  textInput<-str_replace_all(textInput, " aca ", " ")
  textInput<-str_replace_all(textInput, " & ", "and")
  textInput<-str_replace_all(textInput, " aaaaarge ", " ")
  
  
  
  ## word stemming
  library(textstem)
  textInput<-textstem::lemmatize_strings(textInput)
  

  a<-unlist(hunspell(textInput))



  
  if(identical(a, character(0))==FALSE){
  for (i in 1:length(a)){
    for (j in 1:nrow(correct3)){
       #print(a[i])
      
      if (a[i] == correct3[j,1]) {
         #print(correct3[j,2])
        # if (a[i]==correct3[j,1])
        textInput<-str_replace_all(textInput, a[i], correct3[j,2])
      }
      j=j+1
    }
   
    i=i+1
  }
  }

  return(textInput)
}
