# VAR model
# Michael Kilchenmann
# February 2016

# object.size(mat)
# dev.off(dev.list()["RStudioGD"])
  
  a0 <- Sys.time()  
  print(paste("Start: ", Sys.time()))
  library(quantmod)
  library(vars)
  
  oanda <- 0
  add.data <- 0
  quandl <- 0
  del.sat <- 0
  google <- 0
  api <-1
  
  standalone <- 0
  COLOCATION <- Sys.getenv("COLOCATION", unset = NA)
  print(paste("COLOCATION =",COLOCATION),sep="")
  if (COLOCATION=="HOME"){
    setwd('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/')
    this.dir <- ('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/')
  }else{
    setwd('C:/SRDEV/R/')
    this.dir <- ('C:/SRDEV/R/')
  }
  timeframe <- "D"
  if (standalone==1){
    rm(list=ls())    
    api <-1
    a0 <- Sys.time()
    oanda <- 0
    quandl <- 0
    google <- 0
    add.data <- 0
    intraday <- 0
    del.sat <- 0
    timeframe <- "D"
  }
  
  asset <- 'SPX500_USD'
  source("Data_Retrieve.r")
  asset1 <- sc.data
  names(asset1)[names(asset1)=="closeBid"] <- asset
  
  asset <- 'USD_JPY'
  source("Data_Retrieve.r")
  asset2 <- sc.data
  names(asset2)[names(asset2)=="closeBid"] <- asset
  
  asset <- 'XAU_USD'
  source("Data_Retrieve.r")
  asset3 <- sc.data
  names(asset3)[names(asset3)=="closeBid"] <- asset
  
  # Create Matrix with all prices and ommit NA days along the ways :)
    mat <- merge(asset1, asset2)
    
    # mat <- data.frame(value=coredata(mat),timestamp=index(mat))
    mat <- merge(asset1,asset2)
    mat <- merge(mat,asset3)
    mat <- na.omit(mat)
    # Use first differences
    # mat <- na.omit(diff(mat))
    
    # mat$Time <- substr(mat[,1], 1, 1)
    rownames(mat)<-substr(mat[[1]],1,19)
    mat <- mat[,-1]
    as.xts(mat)
    # mat <- xts(mat[,-4],order.by=mat[,4])
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
  print(selection$selection)
  print(summary(selection))
  
  # VAR estimation
  # mat.VAR.const.0 = estimate
  estimate <- VAR(mat, p=selection$selection[3],type="const")
  options(show.signif.stars=TRUE)
  print(estimate$varresult)
  print(estimate)
   
  # VAR Forecast
  forecast <- predict(estimate, n.ahead=1, ci=0.8)
  # print(summary(estimate))
  print(forecast)
  print(forecast$fcst$SPX500_USD[1])
  fcst.prdt=forecast$fcst$SPX500_USD[1]
  # Plot Impulse Respnse Functions
  # plot(irf(estimate, impulse="EUR.USD"))
  
     
  # Ordinary and Partial Autocorrelations of Differenced Series
   
  # par(mfrow=c(2,2))
  # plot(mat[,1],main="EURUSD")
  # plot(mat[,2],main="XAUUSD")
  # plot(mat[,3],main="SPX")
  
  a1 <- Sys.time()
  print(a1-a0)
  print(paste("End: ", Sys.time()))
  
  