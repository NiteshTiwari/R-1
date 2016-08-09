# AR 1 model
# Michael Kilchenmann
# script for FX data using quantmod

  library(quantmod)
  srcDrc <- getSrcDirectory(function(x) {x})
  sc.data <- getFX("EUR/USD",
                from = Sys.Date() - 360,
                tp = Sys.Date(),
                env = parent.frame(),
                verbose = FALSE,
                warning = TRUE,
                auto.assign = FALSE)
  
  data <- sc.data

# data <- head(data,-3)
  
# plot switch (i=1 -> plotting / i = 0 -> no plotting)
  i <- 1
  data_fd <- diff(data)
  
# Hit Rate Calculation a
  n <- nrow(sc.data)
  
  
# Overview
  if (i==1){
    par(mfrow=c(3,3))
    plot(data, main="data 500 days rolling")
    plot(data_fd, main = "first differences 500 days rolling")
    hist(data_fd, main = "histogramm of fd")
    acf(data, main = "ACF data")
    acf(data_fd, main = "ACF fd", na.action=na.omit)
    pacf(data_fd, main = "PACF fd",  na.action=na.omit)
  }
  
  data.fit <- arima(data_fd, order=c(1,0,0), include.mean=FALSE)
    # optional: seasonal=list(order=c(1,1,0), period=12), include.mean=FALSE)
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
    # abline(h=0.002, col="red)
  }
  
  print(data.pred$pred)