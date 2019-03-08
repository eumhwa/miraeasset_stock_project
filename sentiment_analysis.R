rm(list=ls())
#####################################################################

# Title : Sentiment Analysis
# Author : EH

#####################################################################
#Set Working Directory ----
setwd("C://Users//PC2//Desktop//미래에셋")

#library ----
#devtools::install_github("ropensci/googleLanguageR")
library(tidyverse)
library(tidytext) #textmining
library(stringr)
library(rJava)
library(googleLanguageR) # gcp api 
library(KoNLP)
library(dplyr)

#####################################################################

news <- read.csv("naver_news13.csv", header=T)

dt <- news %>% 
  group_by(day) %>%
  summarise(article_day=paste0(article, collapse=""))


#####################################################################
#function of preprocessing
preproc <- function(data){
  
  keywords <- c('[~!/@▲▼[]◇#↑$%&*()_+=?<>]','\\(이모티콘\\)',"\\d+","\\.","\\,","한국경제","머니","투데이","YTN","뉴시스","이데일리","뉴스","
                경향신문","매일경제","MBN","파이낸셜","서울경제","올해","종목","들이","오전","주요","마감","가운데","대비","종목","이후","증시",
                "투자","하루", "금융","코스","가격","경기","경제","시장","업계","아경","특파원","실적","기고","거래","아시", "bp", ".bp", "fnnews", 
                "com","fn", "ahnman", "pja", "co", "kr", "www", "http", "mk", "ceo", " [A-Za-z0-9_]","asiae","tv", "sms", "on", "SMS", "ism",
                "ON", "ARS", "HSBC", "오승주기자", "GO", "PMI", "VN", "nut", "\\[☞", "CEO", "[a-z]", "[A-Z]", "RBS", "KT", "했다")
  
  data_preproc <- data
  
  for(i in 1:length(keywords)){
    data_preproc <- gsub(keywords[i], "", data_preproc)
  }
  
  data_preproc <- Filter(function(x){nchar(x)>=2 && nchar(x)<6}, data_preproc)
  
  return(data_preproc)
}
preproc_en <- function(data){
  
  keywords <- c("a ", "sharp", "and ", "employment", "growth", "profit", "export", "in ", "sales", "demand"
                ,"tax", "sudden", "trend" )
  
  data_preproc <- data
  
  for(i in 1:length(keywords)){
    data_preproc <- gsub(keywords[i], "", data_preproc, fixed = T)
  }
  
  return(data_preproc)
}


#google translate api 
GL_AUTH <- "C:/Users/PC2/Desktop/API_KEY"
gl_auth("C:/Users/PC2/Desktop/API_KEY")









#####################################################################
#sentiment analysis

for(i in 1:nrow(dt)){
  
  useNIADic()
  useSejongDic()
  
  #extracting korean nouns
  noun <- unlist(sapply(dt$article_day[i], extractNoun, USE.NAMES = F))
  noun <- preproc(noun)
  
  text_df <- data_frame(noun)
  text_df <- text_df %>%
    mutate(line=1:length(noun)) %>%
    unnest_tokens(noun_ko, noun)
  
  #translating via gcp api
  noun_en <- gl_translate(text_df$noun_ko, target = "en")$translatedText  
  noun_en <- data_frame(noun_en=tolower(noun_en))
  text_df <- bind_cols(text_df,noun_en)
  
  word <- as.data.frame(noun_en)[,1]
  word <- data_frame(word=trimws(preproc_en(word)))
  text_df <- bind_cols(text_df, word)
  
  #loadiing lexicons
  nrcs <- get_sentiments("nrc")
  bings <- get_sentiments("bing")
  
  
  #merging data
  merge_df <- left_join(text_df, nrcs)
  colnames(merge_df) <- c("line", "noun_ko", "noun_en", "word","sentiment_nrc")
  
  merge_df <- left_join(merge_df, bings)
  colnames(merge_df) <- c("line", "noun_ko",  "noun_en", "word", "sentiment_nrc", "sentiment_bing")
  
  merge_df_nrc <- merge_df %>% 
    count(noun_ko, noun_en, sentiment_nrc) %>%
    spread(sentiment_nrc, n, fill=0)
  
  merge_df_bing <- merge_df %>% 
    count(noun_ko, noun_en,word, sentiment_bing) %>%
    spread(sentiment_bing, n, fill=0)
  colnames(merge_df_bing) <- c("noun_ko", "noun_en", "word", "negative_bing", "positive_bing", "NA_bing")
  
  
  export_df <- left_join(merge_df_bing, merge_df_nrc)
  
  f_name<-paste0("C://Users//PC2//Desktop//미래에셋//senti_dt//", as.character(dt$day[i]), ".csv")
  write.csv(export_df, f_name)
  
}




