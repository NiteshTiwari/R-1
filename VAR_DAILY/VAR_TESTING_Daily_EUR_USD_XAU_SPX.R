# VAR model
# Michael Kilchenmann
# February 2016
# Adjustments July 2016

# object.size(mat)
# dev.off(dev.list()["RStudioGD"])inst
  print(paste("Start: ", Sys.time()))
  library(quantmod)
  library(vars)
  
  standalone <- 1
  
  setwd(this.dir)
  
  oanda <- 1
  google <-0
  quandl <- 0
  add.data <- 0
  del.sat <- 0
  intraday <- 0
  api <-1
  
  asset <- 'EUR/USD'
  source("Data_Retrieve.r")
  asset1 <- sc.data
  
  asset <- 'XAU/USD'
  source("Data_Retrieve.r")
  asset2 <- sc.data
  
  asset <- 'SPX500/USD'
  source("Data_Retrieve.r")
  asset3 <- sc.data
  
  # Create Matrix with all prices and ommit NA days along the way :)
    mat <- merge(asset1, asset2, asset3)
    mat <- data.frame(value=coredata(mat),timestamp=index(mat))
    mat <- na.omit(mat)
    colnames(mat)[3] <- "value.SPX"
    mat <- xts(mat[,-4],order.by=mat[,4])
    # Use first differences
    # mat <- na.omit(diff(mat))
      np <- 0
      n <- 0
      
      for(j in 100:246){
        mat.T <- mat[(j-99):j,]
        selection <- VARselect(mat, lag.max=12, type="const")
        # VAR estimation
        estimate <- VAR(mat.T, p=selection$selection[3],type="const")
        options(show.signif.stars=TRUE)
        # VAR Forecast
        forecast <- predict(estimate, n.ahead=1, ci=0.8)
        diff.forecast <- forecast$fcst$value.EUR.USD[1,1] - mat.T$value.EUR.USD[nrow(mat.T)]
        diff.actual <- coredata(mat$value.EUR.USD[j+1,1])-coredata(mat$value.EUR.USD[j,1])
        # Calculation of Hit Rate          
        if (sign(diff.forecast)==sign(diff.actual)){
          np=np+1
          }
          n=n+1
        # print(summary(forecast))
      }    
      
      print(paste("Hit rate =",round(100*np/n,1),"%"))
      print(paste("End: ", Sys.time()))
  