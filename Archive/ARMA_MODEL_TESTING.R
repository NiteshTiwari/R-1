# script for FX data using quantmod
# ARMA Backtesting script

library(Quandl)
library(quantmod)
standalone <- 0

if (standalone==1){
  rm(list=ls())
  this.dir <- dirname(parent.frame(2)$ofile)
  setwd(this.dir)
  intraday <- 0
  oanda <- 1
  google <- 0
  quandl <- 0
  print(paste("Start: ", Sys.time()))          
  asset <- "EUR/GBP"
  i <- 1
  pdq=c(1,0,1)
  del.sat <- 1
  add.data <- 0
  print("ARMA_MODEL_TESTING: standalone = TRUE")
  pt <- 0
  source('Data_Retrieve.R')
  } else{
  print("ARMA_MODEL_TESTING: standalone = FALSE")}

actual.diff <- diff(sc.data)
np <- 0
n <- 0

for(jc in 100:(nrow(sc.data)-1)){
  data <- sc.data[(jc-99):jc,]
  data_fd <- diff(data)
  
  # Overview
  if (pt==1){
    par(mfrow=c(3,3))
    plot(data, main = paste(asset))
    plot(data_fd, main = paste(asset," First Differences"))
    hist(data_fd, main = "histogramm of fd")
    acf(data, main = "ACF data")
    acf(data_fd, main = "ACF fd", na.action=na.omit)
    pacf(data_fd, main = "PACF fd",  na.action=na.omit)
  }
  
  data.fit <- arima(data_fd, order=pdq, include.mean=FALSE)
  # optional: seasonal=list(order=c(1,1,0), period=12), include.mean=FALSE)
  # include.mean=TRUE -> assume constant trend in time series
  
  if (i==1){
    print(data.fit)
  }
  
  data.pred <-predict(data.fit, n.ahead=1)
  data_fd <- ts(data_fd)
  
  if (pt==1){
    plot(tail(data_fd,5))
    lines(data.pred$pred, col="blue")
    # see how well model predicts
    lines(data.pred$pred + 2*data.pred$se, col="red")
    lines(data.pred$pred - 2*data.pred$se, col="red")
  }
  
  if (i==1){
    print(data.pred$pred)
  }
  
  # Calculation of Hit Rate          
  if (sign(coredata(actual.diff[(jc+1),]))==coredata(sign(data.pred$pred))){
    if(i==1){
      print(actual.diff[(jc+1),])
      print(data.pred$pred)
    }
    np=np+1
  }
  n=n+1
}


HitRate <- np/n
print(HitRate)
print(paste("End: ", Sys.time()))


