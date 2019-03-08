#####################################################################

# Title : Deep Neural Network
# Author : EH

#####################################################################

setwd("C://Users//PC2//Desktop//미래에셋//data")


library(h2o)
library(ggplot2)
h2o.init(nthreads = -1)


#####################################################################
dt <- list()
dt[[1]] <- read.csv("Mirae.csv", header=T)
dt[[2]] <- read.csv("NAVER.csv", header=T)

#1:미래에셋, 2:네이버 
train1 <- dt[[1]][dt[[1]]$DATE<20180101,]
test1 <-   dt[[1]][dt[[1]]$DATE>=20180101,]

train2 <- dt[[2]][dt[[2]]$DATE<20180101,]
test2 <-   dt[[2]][dt[[2]]$DATE>=20180101,]

names(train1)
#h2o타입변환
train_h2o1 <- as.h2o(train1)[,-c(1,2,7,8,9,10)] #date, item, volume, ta, updown, pr 제거
test_h2o1 <- as.h2o(test1)[,-c(1,2,7,8,9,10)]

train_h2o2 <- as.h2o(train2)[,-c(1,2,7,8,9,10)]
test_h2o2 <- as.h2o(test2)[,-c(1,2,7,8,9,10)]






#####################################################################
##NETWORK---

#1. 미래에셋대우
mirae.fit <- list()
for(i in 1:3){
  
  y_idx <- c(4,2,3) #종가, 고가, 저가 순서
  y_idx <- y_idx[i]
  mirae.fit[[i]] <- h2o.deeplearning(x = c(1, 5:ncol(train_h2o1)), #column index
                                     y = y_idx,
                                     training_frame = train_h2o1,
                                     distribution = "AUTO",
                                     activation = "Rectifier",
                                     hidden = c(ncol(train_h2o1)*2, ncol(train_h2o1)*2, ncol(train_h2o1)*2),
                                     epochs = 300,
                                     loss = "Quadratic",
                                     mini_batch_size = round(nrow(train_h2o1)/100),
                                     max_w2 = 20,
                                     l1 = 1e-3
  )
}



#2. Naver
naver.fit <- list()
for(i in 1:3){
  
  y_idx <- c(4,2,3) #종가, 고가, 저가 순서
  y_idx <- y_idx[i]
  naver.fit[[i]] <- h2o.deeplearning(x = c(1, 5:ncol(train_h2o2)), #column index
                                     y = y_idx,
                                     training_frame = train_h2o2,
                                     distribution = "AUTO",
                                     activation = "Rectifier",
                                     hidden = c(ncol(train_h2o2)*6, ncol(train_h2o2)*8, ncol(train_h2o2)*2),
                                     epochs = 300,
                                     loss = "Quadratic",
                                     mini_batch_size = round(nrow(train_h2o2)/100),
                                     max_w2 = 20,
                                     l1 = 1e-3
  )
}



#####################################################################
#PREDICTION ---

prd_to_rmse <- function(model, test, test_h2o, idx){
  
  p <- h2o.predict(model, newdata=test_h2o[,-c(2,3,4)], type="response")
  p <- as.data.frame(p)
  
  #1:CP, 2:HP, 3:LP
  if(idx==1){
    cp <- test$CP
    df <- cbind(test$DATE, cp, p)
    rms <- sqrt(mean((cp - p$predict)^2))
  }
  else if(idx==2){
    hp <- test$HP
    df <- cbind(test$DATE, hp, p)
    rms <- sqrt(mean((hp - p$predict)^2))
  }
  else if(idx==3){
    lp <- test$LP
    df <- cbind(test$DATE, lp, p)
    rms <- sqrt(mean((lp - p$predict)^2))
  }
  else{cat("invalid argument!!")}
  
  return(list(RMSE=rms, DF=df))
  
}

M_RMSE_CP <- prd_to_rmse(mirae.fit[[1]], test1, test_h2o1, 1)
M_RMSE_HP <- prd_to_rmse(mirae.fit[[2]], test1, test_h2o1, 2)
M_RMSE_LP <- prd_to_rmse(mirae.fit[[3]], test1, test_h2o1, 3)

N_RMSE_CP <- prd_to_rmse(naver.fit[[1]], test2, test_h2o2, 1)
N_RMSE_HP <- prd_to_rmse(naver.fit[[2]], test2, test_h2o2, 2)
N_RMSE_LP <- prd_to_rmse(naver.fit[[3]], test2, test_h2o2, 3)

M_RMSE_CP[[1]]
M_RMSE_HP[[1]]
M_RMSE_LP[[1]]

N_RMSE_CP[[1]]
N_RMSE_HP[[1]]
N_RMSE_LP[[1]]


#####################################################################
#PLOTTING ---

ts_plots <- function(df, test_y, idx){
  
  date <- as.Date(as.character(test_y$DATE), format="%Y%m%d")
  predict <- df$predict
  
  if(idx==1) true_y <- test_y$CP
  else if(idx==2) true_y <- test_y$HP
  else if (idx==3) true_y <- test_y$LP
  
  g <- ggplot(data=df, aes(x=date, y=true_y, col='True')) + 
    geom_line(lwd=1.3) + 
    geom_point(size=2) +
    geom_line(aes(x=date,y=predict,col="Prediction"),lwd=1.3) + 
    geom_point(aes(x=date,y=predict, col="Prediction"),size=2) +
    scale_x_date(breaks=date_breaks("1 day"),labels=date_format("%Y%m%d")) + 
    theme(axis.text.x=element_text(angle=90)) + 
    theme(text = element_text(size=15)) +
    labs(x="Date", y="Stock Price (Won)") +
    scale_color_manual(values = (c(Prediction = 'tomato1',True = 'black')))
  
  print(g)
}


M_PLOT_CP <- ts_plots(M_RMSE_CP[[2]], test1, 1)
M_PLOT_HP <- ts_plots(M_RMSE_HP[[2]], test1, 2)
M_PLOT_LP <- ts_plots(M_RMSE_LP[[2]], test1, 3)

N_PLOT_CP <- ts_plots(N_RMSE_CP[[2]], test2, 1)
N_PLOT_HP <- ts_plots(N_RMSE_HP[[2]], test2, 2)
N_PLOT_LP <- ts_plots(N_RMSE_LP[[2]], test2, 3)

