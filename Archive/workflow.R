# Feb 2016
# forecasting workflow
# mkil

rm(list=ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
start <- print(Sys.time())
assets.name <- cbind(c("EUR/USD","USD/JPY","GBP/USD", "USD/CHF", "AUD/USD", "USD/CAD", "EUR/CHF",
                       "NZD/USD", "EUR/SEK", "EUR/NOK", "USD/SEK", "USD/NOK", "EUR/JPY", "XAU/USD","XPD/USD"))

assets.source <- cbind(c("Oanda","Oanda","Oanda", "Oanda", "Oanda", "Oanda", "Oanda", "Oanda", "Oanda", "Oanda",
                         "Oanda", "Oanda", "Oanda", "Oanda", "Oanda"))

assets <- cbind(assets.name,assets.source)
forecasts <- matrix(nrow=nrow(assets),ncol=4)
colnames(forecasts) <- c("Asset", "Forecast","Last Input Price", "AR Alpha")

for (j in 1:nrow(assets)){
  MA <- 0
  AR <- 1
  ARMA <- 0
  asset <- assets[j,1]
  if (assets[j,2]=="Oanda"){
    oanda <- 1
    google <- 0
    quandl <- 0
    source("ARMA_model.R")
    }
  forecasts[j,1] <- asset
  forecasts[j,2] <- round(data.pred$pred,4)
  forecasts[j,3] <- tail(sc.data,1)
  forecasts[j,4] <- round(data.fit$coef[1],2)
}
write.csv(forecasts, paste("forecasts_AR",Sys.Date(),".csv"))

for (j in 1:nrow(assets)){
  MA <- 1
  AR <- 0
  ARMA <- 0
  asset <- assets[j,1]
  if (assets[j,2]=="Oanda"){
    oanda <- 1
    google <- 0
    quandl <- 0
    source("ARMA_model.R")
  }
  forecasts[j,1] <- asset
  forecasts[j,2] <- round(data.pred$pred,4)
  forecasts[j,3] <- tail(sc.data,1)
}
write.csv(forecasts, paste("forecasts_MA",Sys.Date(),".csv"))

for (j in 1:nrow(assets)){
  MA <- 0
  AR <- 0
  ARMA <- 1
  asset <- assets[j,1]
  if (assets[j,2]=="Oanda"){
    oanda <- 1
    google <- 0
    quandl <- 0
    source("ARMA_model.R")
  }
  forecasts[j,1] <- asset
  forecasts[j,2] <- round(data.pred$pred,4)
  forecasts[j,3] <- tail(sc.data,1)
}
write.csv(forecasts, paste("forecasts_ARMA",Sys.Date(),".csv"))

end <- print(Sys.time())
print(end-start)

