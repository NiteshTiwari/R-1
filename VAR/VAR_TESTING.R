# VAR model
# Michael Kilchenmann
# February 2016

# object.size(mat)
# dev.off(dev.list()["RStudioGD"])
  print(paste("Start: ", Sys.time()))
  library(quantmod)
  library(vars)
  
  standalone <- 1
  
  asset <- "EUR/USD"
  
  oanda <- 1
  google <-0
  quandl <- 0
  
  source("Data_Retrieve.r")
  # Collect index (S&P 500) data from Yahoo
    date.start <- paste(format(start(sc.data),"%Y"),format(start(sc.data),"%m"),format(start(sc.data),"%d"))
    date.end <- paste(format(end(sc.data),"%Y"),format(end(sc.data),"%m"),format(end(sc.data),"%d"))
    SPX <- getYahooData("^GSPC", start=date.start, end=date.end)
    SPX <- SPX$Close
  # collect Gold Prices
    XAU <- getFX("XAU/USD",
                     from = Sys.Date() - 360,
                     tp = Sys.Date(),
                     env = parent.frame(),
                     verbose = FALSE,
                     warning = TRUE,
                     auto.assign = FALSE)
  # Create Matrix with all prices and ommit NA days along the way :)
    mat <- merge(sc.data, XAU, SPX)
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
  