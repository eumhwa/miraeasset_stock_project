rm(list=ls())
#####################################################################

# Title : Naver News Crawler
# Source: finance.naver.com
# Author : EH

#####################################################################
#library ----
library(RSelenium)
library(rvest)
library(XML)

#cmd ----
#java-Dwebdriver.gecko.driver="geckodriver.exe"-jarselenium-server-standalone-3.13.0.jar -port 4445


#####################################################################

#Driver ����
remDr <- remoteDriver(remoteServerAddr='localhost',
                      port=4445L, 
                      browserName="chrome")


#####################################################################
remDr$open()
url = "https://finance.naver.com/news/mainnews.nhn?date="

days <- seq(as.Date("2016/10/23"), as.Date("2018/6/29"), "days")
output <- data.frame(day=as.Date("1444/1/1"), article="aa",  stringsAsFactors = F)



#��л翡 ���� ������ ���-alert â ���������� �Լ�
alert <- function(){
  alt <- remDr$getAlertText()
  if(nchar(alt)>0) remDr$acceptAlert()
}


#ũ�Ѹ� �Լ�
crawl <- function(j){
  
  val <- paste0('.block1:nth-child(',j,') a')
  ele <- remDr$findElement(using = "css selector", value=val)
  
  if(nchar(unlist(ele$getElementText()))==0){
    val <- paste0('.block1:nth-child(',j,') img')
    ele <- remDr$findElement(using = "css selector", value=val)
  }
  
  ele$clickElement() # ��� Ŭ��
  
  #alert������� �Լ� ���� �����
  chk <- try(alert(), silent = T)
  if(class(chk)!="try-error") stop("alertŻ��!!")
  
  #��� ����� ���� ���� 
  title <- remDr$findElement(using="css selector", value=".article_info h3")
  title <- unlist(title$getElementText())
  
  article <- remDr$findElement(using="css selector", value="#content")
  article <- unlist(article$getElementText())
  
  return(list(title=title, article=article))
}


idx <- 1
for(i in 1:length(days)){
  
  day <- days[i]
  URL <- paste0(url, day)
  remDr$navigate(URL)
  
  Sys.sleep(1.5)
  
  #no. of articles
  cnt <- remDr$findElements(using="class", value="block1")
  cnt <- length(cnt)
  
  if(cnt!=0){
    for(j in 1:cnt){
      
      chk <- try(crawl(j), silent=T)
      Sys.sleep(0.5)
      
      #alert���� ��� j-for�� ����������
      if(class(chk)=="try-error") next()
      
      output[idx, 1] <- day
      output[idx, 2] <- paste(chk$title, chk$article) 
      
      idx <- idx+1
      
      Sys.sleep(1.5)
      
      remDr$goBack()
      
    }  
  }
  else if(cnt==0) next()
  
  
}


dim(output)
tail(output,2)[,1]
summary(output$day)

write.csv(output, "C://Users//PC2//Desktop//�̷�����//naver_news12.csv")