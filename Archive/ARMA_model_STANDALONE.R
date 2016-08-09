
# ARMA model forecasting
# script for FX data using quantmod / google finance / yahoo / quandl
# mkil
# Source for SIT https://github.com/systematicinvestor/SIT/raw/master/sit.gz

  # dev.off(dev.list()["RStudioGD"])
  library(quantmod)
  library(Quandl)
  library(astsa)
  
  standalone <- 1
  
  if (standalone==1) {
    rm(list=ls())
    print('ARMA_model: standalone = TRUE')
    asset <- "AUD/CHF"
    oanda <- 1
    google <- 0
    quandl <- 0
    i <- 1
    AR <- 1
    MA <- 0
    ARMA <- 0
    del.sat <- 1
    add.data <- 0
    intraday <- 0
  }
  
  if (ARMA==1) {
    pdq=c(1,0,1)
  }
  if (AR==1) {
    pdq=c(4,0,0)
  }
  if (MA==1) {
    pdq=c(0,0,1)
  }

  source('Data_Retrieve.R')

  data <- sc.data

# plot switch (i=1 -> plotting / i = 0 -> no plotting)
  i <- 1
  data_fd <- diff(data)
  
# Hit Rate Calculation a
  n <- nrow(sc.data)
  
# Overview
  if (i==1){
    par(mfrow=c(3,3))
    plot(data, main = paste(asset))
    plot(data_fd, main = paste(asset," First Differences"))
    hist(data_fd, breaks=20, main = "histogramm of fd",prob=TRUE)
    lines(density(na.omit(data_fd)),col="red")
    lines(density(na.omit(data_fd), adjust=2), lty="dotted") 
    acf(data, main = "ACF data", lag.max = 10)
    acf(data_fd, main = "ACF fd", na.action=na.omit, lag.max=10)
    pacf(data_fd, main = "PACF fd",  na.action=na.omit, lag.max=10)
    qqnorm(na.omit(data_fd))
    qqline(na.omit(data_fd))
  }
  
  data.fit <- arima(data_fd, order=pdq, include.mean=FALSE)
    # optional: seasonal=list(order=c(1,1,0), period=12), 2include.mean=FALSE)
    # include.mean=TRUE -> assume constant trend in time series
  
  if (i==1){
    print(data.fit)
  }
  
  data.pred <-predict(data.fit, n.ahead=1)
  data_fd <- ts(data_fd)
  
  if (i==1){
    plot(data_fd)
    lines(data.pred$pred, col="blue")
    # see how well model predicts
    lines(data.pred$pred + 2*data.pred$se, col="red")
    lines(data.pred$pred - 2*data.pred$se, col="red")
    # abline(h=0.002, col="red")
    # dev.new()
    lag1.plot(na.omit(data_fd),9)
  }
  
  print(paste(asset, " 1d ARMA forecast (", pdq[1], ",", pdq[2],",",pdq[3],"): ", round(data.pred$pred,4), sep=""))
  
  
  