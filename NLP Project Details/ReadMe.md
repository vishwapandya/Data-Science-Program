NLP Based Question & Answers for E-commerce


Project was done in 
•	Operating System: - Windows10
•	Processor: - Intel core i7
•	RAM: -16 GB 
•	System Type: -64-bit operating System, x64-based processor
•	Software Used:- R Version 3.6.1 (architecture x86_64).

Please install the required library if not already installed in R as shown below
install.packages(c("XML", "sqldf", "dplyr", "tm", "tidyverse", "tidytext", "textclean", "qdapRegex", "hunspell", "textstem", "DataCombine", "stringr", "sentimentr", "magrittr", "NLP", "lubridate", "caret", "rlist", "shiny", "DT", "ggplot2", "wordcloud", "reshape2", "reshape2", "ggraph", "ggforce", "igraph", "corrplot", "PerformanceAnalytics"))

Sequence of Code file to run
•	01.EDA.R
•	02.EDA.R
•	03.Visualization.R
•	04.Function for Text Analysis.R
•	05.Model.R
•	06.Shiny.R

Please change file path present in starting line of code to the path where you would be storing our folders. Please modify only highlighted path shown below only on files 01.EDA.r, 02.EDA.r, 05.Model.r:
 



1.	Business Objective
In any website or in ecommerce business there are very few FAQ or limited user review. All the questions cannot be shown in website as website will look clumsy. Due to limited exposure of the questions, there is a possibility that user might not find question and the answer He/ She is searching for or the question is not present or never have been asked.
Considering different scenarios, we are proposing a model which will take user question and will bring out relevant top five question and answer from Question Bank.  
Our model ensures that 
i.	User will get relevant answer for the question he/ she is searching for 
ii.	Website will not be clumsy
iii.	Since all the process is happening at server side and only top 5 result is achieved, website will not be clumsy.

2.	Project Architecture
2.1	Input Layer
User Question and Question bank both are considered as input. 
•	User question/Keywords is the text or question that user will provide as an input. 
•	Question bank means the existing question and answer set that is already present. Later User question will be compared with this question bank to find relevant Question and answer.

2.2	 Preprocessing 
Before considering to take text as input, text needs to be cleaned. Process of cleaning text includes:
•	Removal of Stop words (common words occur in a sentence)
•	Elimination of URLs (with https or ftp [absolute URLs] and broken URLs (without https or ftp]).
•	Exclusion of emails.
•	Deletion unwanted words
•	Word spell check and correction
•	Word stemming

2.3	Model
Model is a function “QuestionSearch” which computes cosine similarity between user Questions/Keywords and each Question bank to find the similar questions. 

2.4	Final Output
Final Output is the relevant question and answer shown based on the user input/keywords. Top five results will be shown with decreasing order of cosine similarity. 


