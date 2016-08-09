# Data Retrieve
# mkil

  library(quantmod)
  library(Quandl)
  # Select Asset & Data Source

  standalone <- 1
  
  if (standalone==1){
    print("standalone = TRUE")
    this.dir <- dirname(parent.frame(2)$ofile)
    setwd(this.dir)
    rm(list=ls())
    asset <- "USD/JPY"
    oanda <- 1
    google <- 0
    quandl <- 0
    add.data <- 0
    del.sat <- 1
    intraday <- 0
  }
  
  if (oanda==1){
  sc.data <- getFX(asset,
                from = Sys.Date() - 720,
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
  
  if (add.data==1){
    val.to.add <- 1.5
    sc.data <- c(sc.data,xts(as.double(val.to.add),Sys.Date()))
  }
  
  if (del.sat==1){
    print("Data Retrieve: Delete Saturdays = TRUE")
    sc.data <- sc.data[!weekdays(index(sc.data)) %in% c("Saturday")]
  }

  if (intraday==1){

  }