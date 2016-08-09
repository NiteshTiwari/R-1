# AR 1 model
# Michael Kilchenmann
# script for FX data using quantmod

library(quantmod)
srcDrc <- getSrcDirectory(function(x) {x})
eurusd <- getFX("EUR/USD",
                from = Sys.Date() - 360,
                tp = Sys.Date(),
                env = parent.frame(),
                verbose = FALSE,
                warning = TRUE,
                auto.assign = FALSE)

# plot switch (i=1 -> plotting / i = 0 -> no plotting)
  i <- 1
  eurusd_fd <- diff(eurusd)

# Overview
  if (i==1){
    par(mfrow=c(3,3))
    plot(eurusd, main="eurusd 500 days rolling")
    plot(eurusd_fd, main = "first differences 500 days rolling")
    hist(eurusd_fd, main = "histogramm of fd")
    acf(eurusd, main = "ACF eurusd")
    acf(eurusd_fd, main = "ACF fd", na.action=na.omit)
    pacf(eurusd_fd, main = "PACF fd",  na.action=na.omit)
  }
  
  eurusd.fit <- arima(eurusd_fd, order=c(1,0,0), include.mean=FALSE)
    # optional: seasonal=list(order=c(1,1,0), period=12), include.mean=FALSE)
    # include.mean=TRUE -> assume constant trend in time series
  
  if (i==1){
    print(eurusd.fit)
  }
  
  eurusd.pred <-predict(eurusd.fit, n.ahead=50)
  eurusd_fd <- ts(eurusd_fd)
  
  if (i==1){
    plot(eurusd_fd)
    lines(eurusd.pred$pred, col="blue")
    # see how well model predicts
    lines(eurusd.pred$pred + 2*eurusd.pred$se, col="red")
    lines(eurusd.pred$pred - 2*eurusd.pred$se, col="red")
    # abline(h=0.002, col="red)
  }
  
  

                