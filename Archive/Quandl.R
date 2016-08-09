# Data Retrieve
# Michael Kilchenmann

  library(quantmod)
  library(Quandl)
  srcDrc <- getSrcDirectory(function(x) {x})
  rm(list=ls())  
  # Select Asset & Data Source
  asset <- "CHRIS/CME_CL1"
  
  quandl <- 1
  # CHRIS/CME_CL1 -> Crude Oil Futures, Continuous Contract #1 (CL1) (Front Month)
  
    if (quandl==1){
    Quandl.api_key("g1nk1GaNG6mp7zrP5Nps")
    sc.data <- Quandl("CHRIS/CME_CL1", type="xts")
    # sc.data <- sc.data$Last
    }
  