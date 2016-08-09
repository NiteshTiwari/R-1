# AR 1 model
# Michael Kilchenmann
# script for FX data using quantmod
  
  print(paste("Start: ", Sys.time()))
  
  library(quantmod)
  srcDrc <- getSrcDirectory(function(x) {x})
  sc.eurusd <- getFX("EUR/USD",
                from = Sys.Date() - 360,
                tp = Sys.Date(),
                env = parent.frame(),
                verbose = FALSE,
                warning = TRUE,
                auto.assign = FALSE)
    
  actual.diff <- diff(sc.eurusd)
  np <- 0
  n <- 0
  
  for(j in 100:358){
      
      eurusd <- sc.eurusd[(j-99):j,]
      
      # eurusd <- head(eurusd,-3)
        
      # plot switch (i=1 -> plotting / i = 0 -> no plotting)
        i <- 0
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
        
        eurusd.fit <- arima(eurusd_fd, order=c(0,0,1), include.mean=FALSE)
          # optional: seasonal=list(order=c(1,1,0), period=12), include.mean=FALSE)
          # include.mean=TRUE -> assume constant trend in time series
        
        if (i==1){
          print(eurusd.fit)
        }
        
        eurusd.pred <-predict(eurusd.fit, n.ahead=1)
        eurusd_fd <- ts(eurusd_fd)
        
        if (i==1){
          plot(eurusd_fd)
          lines(eurusd.pred$pred, col="blue")
          # see how well model predicts
          lines(eurusd.pred$pred + 2*eurusd.pred$se, col="red")
          lines(eurusd.pred$pred - 2*eurusd.pred$se, col="red")
          # abline(h=0.002, col="red)
        }
        
        if (i==1){
          print(eurusd.pred$pred)
        }

      # Calculation of Hit Rate          
        if (sign(actual.diff[(j+1),])==sign(eurusd.pred$pred)){
          if(i==1){
          print(actual.diff[(j+1),])
          print(eurusd.pred$pred)
          }
          np=np+1
        }
        n=n+1
  }
  
  print(np/n)
  print(paste("End: ", Sys.time()))
  
  