# VAR model
# Michael Kilchenmann
# February 2016

# object.size(mat)
# dev.off(dev.list()["RStudioGD"])
  
  print(paste("Start: ", Sys.time()))
  library(quantmod)
  library(vars)
  
  standalone <- 1
  home <- 1
  if (home==1){
    setwd('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/VAR_DAILY')
    this.dir <- ('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/VAR_DAILY')
  }else{
    setwd('C:/SRDEV/R/VAR_DAILY')
    this.dir <- ('C:/SRDEV/R/VAR_DAILY')
  }
  
  if (standalone==1){
    rm(list=ls())    
    api <-1
    
    
    assets.VAR <- cbind(c("EUR/USD","XAU/USD"),c("oanda","oanda"))
    assets.VAR.number <- nrow(assets.VAR)
    oanda <- 1
    quandl <- 0
    google <- 0
    add.data <- 0
    intraday <- 0
    del.sat <- 0
  }
  
  asset <- 'EUR/USD'
  source("Data_Retrieve.r")
  asset1 <- sc.data
  
  asset <- 'XAU/USD'
  source("Data_Retrieve.r")
  asset2 <- sc.data
  
  asset <- 'SPX500/USD'
  source("Data_Retrieve.r")
  asset3 <- sc.data
  
  # Create Matrix with all prices and ommit NA days along the ways :)
    mat <- merge(asset1, asset2)
    
    mat <- data.frame(value=coredata(mat),timestamp=index(mat))
    mat <- na.omit(mat)
    colnames(mat)[1] <- assets.VAR[1,1]
    colnames(mat)[3] <- "value.SPX"
    mat <- xts(mat[,-4],order.by=mat[,4])
    # Use first differences
    # mat <- na.omit(diff(mat))
    
    #xts(mat)

  # par(mfrow=c(2,2))
  # plot(mat[,1],main="EURUSD")
  # plot(mat[,2],main="XAUUSD")
  # plot(mat[,3],main="SPX")
  # acf(mat,lag.max=10)
  # acf(mat,type="partial",lag.max=10)
  
  print('dim mat')
  print(dim(mat))
  
  # mat.VAR.const = selection
  selection <- VARselect(mat, lag.max=12, type="const")
  # print('selection')
  # print(selection$selection)
  # print(summary(selection))
  
  # VAR estimation
  # mat.VAR.const.0 = estimate
  estimate <- VAR(mat, p=selection$selection[3],type="const")
  options(show.signif.stars=TRUE)
   
  # VAR Forecast
  forecast <- predict(estimate, n.ahead=1, ci=0.8)
  # print(summary(estimate))
  print(forecast)
  # Plot Impulse Respnse Functions
  plot(irf(estimate, impulse="EUR.USD"))
   
  # Ordinary and Partial Autocorrelations of Differenced Series
   
  # par(mfrow=c(2,2))
  # plot(mat[,1],main="EURUSD")
  # plot(mat[,2],main="XAUUSD")
  # plot(mat[,3],main="SPX")
  
  print(paste("End: ", Sys.time()))
  
  