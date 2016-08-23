
# ARMA model forecasting
# script for FX data using quantmod / google finance / yahoo / quandl
# mkil
# Source for SIT https://github.com/systematicinvestor/SIT/raw/master/sit.gz

  # dev.off(dev.list()["RStudioGD"])
  library(quantmod)
  library(Quandl)
  library(astsa)
  library(PerformanceAnalytics)
  
  standalone <- 0
  
  if (standalone==1) {
    rm(list=ls())
    print('ARMA_model: standalone = TRUE')
    asset <- "USD/JPY"
    oanda <- 0
    google <- 0
    quandl <- 0
    api <- 1
    i <- 1
    AR <- 0
    MA <- 0
    ARMA <- 1
    del.sat <- 1
    add.data <- 0
    intraday <- 0
    setwd('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/')
    this.dir <- ('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/')
    # setwd('C:/SRDEV/R_R/R')
    # this.dir <- ('C:/SRDEV/R_R/R')
  }
  
  if (ARMA==1) {
    pdq=c(1,0,1)
    m.ARMA <- "ARMA"
  }
  if (AR==1) {
    pdq=c(4,0,0)
    m.ARMA <- "AR(4)"
  }
  if (MA==1) {
    pdq=c(0,0,1)
    m.ARMA <- "MA(1)"
  }

  source('Data_Retrieve.R')

  data <- sc.data

# plot switch (i=1 -> plotting / i = 0 -> no plotting)
  i <- 1
  data_fd <- diff(data)
  
# Hit Rate Calculation a
  n <- nrow(sc.data)
  
# Overview
  if (i==1 && AR==1){
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
  
  if (i==1 && AR==1){
    plot(data_fd)
    lines(data.pred$pred, col="blue")
    # see how well model predicts
    lines(data.pred$pred + 2*data.pred$se, col="red")
    lines(data.pred$pred - 2*data.pred$se, col="red")
    # abline(h=0.002, col="red")
    # dev.new()
    lag1.plot(na.omit(data_fd),9)
  }
  
  # calculate average daily move
  d.mean <- round(mean(abs(na.omit(data_fd))),4) # mean absolute deviation
  d.sd <- round(sd(data_fd, na.rm=TRUE),4) # standard deviation
  # Store Sign
  s.f <- sign(round(data.pred$pred,4))
  
  strSign="NEUTRAL"
  if (s.f==-1){
    strSign="DOWN"
  } else{strSign="UP"}
  
  # VaR calculations
  if (AR==1){
    qnt.a <- round(quantile(abs(data_fd),probs=c(0.25,0.5,0.75,0.8,0.9,0.95,0.99,1),
      na.rm=TRUE,names=TRUE,type=9),4)
  }
  
  print(paste(asset, " 1d ARMA forecast (", pdq[1], ",", pdq[2],",",pdq[3],"): ", round(data.pred$pred,4), sep=""))
  
  
  
  