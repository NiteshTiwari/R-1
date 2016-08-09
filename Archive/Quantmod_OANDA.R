# Oanda Quantmod Data Retrieve
# Michael Kilchenmann

  library(quantmod)
  srcDrc <- getSrcDirectory(function(x) {x})

  data.quantmod <- getFX("WTICO/USD",
                   from = Sys.Date() - 360,
                   tp = Sys.Date(),
                   env = parent.frame(),
                   verbose = FALSE,
                   warning = TRUE,
                   auto.assign = FALSE)
