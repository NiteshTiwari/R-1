# script for FX data using quantmod
# ARMA Backtesting script
# latest change: 06. June 2016

library(Quandl)
library(quantmod)
standalone <- 0

if (standalone==1){
  rm(list=ls())
  # this.dir <- dirname(parent.frame(2)$ofile)
  this.dir <- ('C:/SRDEV/R_R/R')
  setwd(this.dir)
  intraday <- 0
  oanda <- 0
  google <- 0
  quandl <- 0
  api <- 1
  print(paste("Start: ", Sys.time()))          
  asset <- "USD/NOK"
  i <- 1
  pdq=c(0,0,1)
  del.sat <- 1
  add.data <- 0
  print("ARMA_V2: standalone = TRUE")
  pt <- 0
  source('Data_Retrieve.R')
  } else{
  print("ARMA_V2: standalone = FALSE")}

actual.diff <- diff(sc.data)
np <- 0
n <- 0

for(jc in 200:(nrow(sc.data)-1)){
  data <- sc.data[(jc-199):jc,]
  data_fd <- diff(data)
  
  # Overview
#   if (pt==1){
#     par(mfrow=c(3,3))
#     plot(data, main = paste(asset))
#     plot(data_fd, main = paste(asset," First Differences"))
#     hist(data_fd, main = "histogramm of fd")
#     acf(data, main = "ACF data")
#     acf(data_fd, main = "ACF fd", na.action=na.omit)
#     pacf(data_fd, main = "PACF fd",  na.action=na.omit)
#   }
  
  data.fit <- arima(data_fd, order=pdq, include.mean=FALSE)
  # optional: seasonal=list(order=c(1,1,0), period=12), include.mean=FALSE)
  # include.mean=TRUE -> assume constant trend in time series
  
  if (i==1){
    print(data.fit)
  }
  
  data.pred <-predict(data.fit, n.ahead=1)
  data_fd <- ts(data_fd)
  
#   if (pt==1){
#     plot(tail(data_fd,5))
#     lines(data.pred$pred, col="blue")
#     # see how well model predicts
#     lines(data.pred$pred + 2*data.pred$se, col="red")
#     lines(data.pred$pred - 2*data.pred$se, col="red")
#   }
  
  if (i==1){
    # print(data.pred$pred)
  }
  
  # Calculation of Hit Rate          
  if (sign(coredata(actual.diff[(jc+1),]))==coredata(sign(data.pred$pred))){
    if(i==1){
#       print(actual.diff[(jc+1),])
#       print(data.pred$pred)
    }
    np=np+1
  }
  n=n+1
}


HitRate <- np/n
print(paste("Hit Rate ", asset, ": ",round(100*HitRate,1),"%"))
print(paste("End: ", Sys.time()))


