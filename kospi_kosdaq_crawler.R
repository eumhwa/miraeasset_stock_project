rm(list=ls())
#####################################################################

# Title : Kospi , Kosdaq Crawler
# Source: finance.naver.com
# Author : EH

#####################################################################

#Set working directory ----
setwd("C://Users//PC2//Desktop//미래에셋")

#library ----
library(RSelenium) 
library(rvest)
library(XML)

#cmd 
#java-Dwebdriver.gecko.driver="geckodriver.exe"-jarselenium-server-standalone-3.13.0.jar -port 4445


#Driver 설정
remDr <- remoteDriver(remoteServerAddr='localhost',
                      port=4445L, 
                      browserName="chrome")


#####################################################################
#kospi page iteration

output <- c("start")
remDr$open()
url <- "https://finance.naver.com/sise/sise_index_day.nhn?code=KOSPI&page="

for(i in 1:352){
  
  URL <- paste0(url, i)
  remDr$navigate(URL)
  Sys.sleep(2)
  
  ele <- remDr$findElement(using = "class", value='box_type_m')
  
  ele_txt <- unlist(ele$getElementText())
  tmp <- unlist(strsplit(ele_txt, " "))
  tmp <- unlist(strsplit(tmp, "\n"))[9:44]
  
  output <- append(output, tmp)
  Sys.sleep(2)
}

output_ <- output[-1]
output_mat <- matrix(output_, 6*352, 6, byrow = T)
write.csv(output_mat, "C://Users//PC2//Desktop//미래에셋//kospi.csv")



#####################################################################
#kosdaq iteration
output <- c("start")
remDr$open()
url <- "https://finance.naver.com/sise/sise_index_day.nhn?code=KOSDAQ&page="

for(i in 1:352){
  
  URL <- paste0(url, i)
  remDr$navigate(URL)
  Sys.sleep(2)
  
  ele <- remDr$findElement(using = "class", value='box_type_m')
  
  ele_txt <- unlist(ele$getElementText())
  tmp <- unlist(strsplit(ele_txt, " "))
  tmp <- unlist(strsplit(tmp, "\n"))[9:44]
  
  output <- append(output, tmp)
  Sys.sleep(2)
}
output_ <- output
out <- append(output_, output[-1])

out_ <- out[-1]
output_mat <- matrix(out_, 6*352, 6, byrow = T)
write.csv(output_mat, "C://Users//PC2//Desktop//미래에셋//kosdaq.csv")
