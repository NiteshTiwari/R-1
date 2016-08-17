# Data Retrieve
# mkil

  library(quantmod)
  library(Quandl)
  # Select Asset & Data Source

  standalone <- 0
  
  if (standalone==1){
    print("standalone = TRUE")
    setwd('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/')
    # setwd('C:/SRDEV/R_R/R')
    # setwd('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/')
    # setwd('C:/SRDEV/R_R/R')
    rm(list=ls())
    asset <- "USD/JPY"
    oanda <- 0
    google <- 0
    quandl <- 0
    api <- 1
    add.data <- 0
    del.sat <- 1
    intraday <- 0
  }
  
  if (oanda==1){
  sc.data <- getFX(asset,
                from = Sys.Date() - 360,
                tp = Sys.Date(),
                env = parent.frame(),
                verbose = FALSE,
                warning = TRUE,
                auto.assign = FALSE)
  }
  
  if (google==1){
    sc.data <- getSymbols(asset,
                     from = Sys.Date() - 200,
                     tp = Sys.Date(),
                     #env = parent.frame(),
                     verbose = FALSE,
                     warning = TRUE,
                     auto.assign = FALSE,
                     src="google")
  }

  if (quandl==1){
    Quandl.api_key("g1nk1GaNG6mp7zrP5Nps")
    # sc.data <- sc.data$Last
    sc.data <- Quandl(asset, type="xts")
    sc.data <- sc.data$Last
  }
  
  if (api==1){
    if(home==1){
      sc.data <- read.csv(paste("C:/Users/Michael Kilchenmann/Dropbox/Python/Data/",gsub("/","_",asset),
      "_H1",".csv",sep=""))
    }else{
      sc.data <- read.csv(paste("C:/SRDEV/Data/",gsub("/","_",asset),
      "_H1",".csv",sep=""))
    }
    if (del.sat==1){
      sc.data$Time <- substr(sc.data$Time,1,19)
      # index(sc.data) <- as.Date(sc.data$Time, "%Y-%m-%d")
      rownames(sc.data) <- as.POSIXct(sc.data$Time)
      sc.data$Time <- NULL
      sc.data <- as.xts(sc.data)
    }
  }
  
  if (add.data==1){
    val.to.add <- 1.5
    sc.data <- c(sc.data,xts(as.double(val.to.add),Sys.Date()))
  }
  
  if (del.sat==1){
    print("Data Retrieve: Delete Saturdays = TRUE")
    print(paste("Nrow before del.sat: ",nrow(sc.data),sep=""))
    sc.data <- sc.data[!weekdays(index(sc.data)) %in% c("Saturday")]
    print(paste("Nrow after del.sat: ",nrow(sc.data),sep=""))
  }

  if (intraday==1){

  }