# VAR model
# Michael Kilchenmann
# February 2016
# Adjustments July 2016

# object.size(mat)
# dev.off(dev.list()["RStudioGD"])inst
  
  print(paste("Start: ", Sys.time()))
  library(quantmod)
  library(vars)
  
  standalone <- 0
    COLOCATION <- Sys.getenv("COLOCATION", unset = NA)
    print(paste("COLOCATION =",COLOCATION),sep="")
    if (COLOCATION=="HOME"){
    setwd('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/VAR/DAILY')
    this.dir <- ('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/VAR/')
  }else{
    setwd('C:/SRDEV/R/VAR/DAILY')
    this.dir <- ('C:/SRDEV/R/VAR/DAILY')
  }
  
  oanda <- 0
  google <-0
  quandl <- 0
  add.data <- 0
  del.sat <- 0
  intraday <- 0
  api <-1
  timeframe <- "D"

      np <- 0
      n <- 0
      
      source("VAR_Daily_INDU_USD_XAU.R")
      
      for(j in 250:nrow(mat)-1){
        mat.T <- mat[(j-249):j,]
        selection <- VARselect(mat.T, lag.max=12, type="const")
        # VAR estimation
        estimate <- VAR(mat.T, p=selection$selection[3],type="const")
        options(show.signif.stars=TRUE)
        # VAR Forecast
        forecast <- predict(estimate, n.ahead=1, ci=0.8)
        diff.forecast <- forecast$fcst$SPX500_USD[1] - mat.T$SPX500_USD[nrow(mat.T)]
        diff.actual <- coredata(mat$SPX500_USD[j+1])-coredata(mat$SPX500_USD[j])
        # Calculation of Hit Rate          
        if (abs(diff.forecast)>0.00){
        if (sign(diff.forecast)==sign(diff.actual)){
            np=np+1}#}
          n=n+1}
        # print(summary(forecast))
        }    
      
      print(paste("Hit rate =",round(100*np/n,1),"%"))
      print(paste("End: ", Sys.time()))
  