# July 2016
# forecasting workflow
# mkil
# ARMA_model.R --> model estimaton
# ARMA_TEST.R --> rolling backtesting
# calculate mean average deviations
# calculate standard deviations

rm(list=ls())

start <- print(Sys.time())
home <- 0
if (home==1){
  setwd('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/')
  this.dir <- ('C:/Users/Michael Kilchenmann/Dropbox/R/AssetPriceForecasting/')
}else{
  setwd('C:/SRDEV/R/')
  this.dir <- ('C:/SRDEV/R/')
}
del.sat <- 1
add.data <- 0
assets.name <- cbind(c("EUR/USD","USD/JPY","GBP/USD", "USD/CHF", "AUD/USD", "USD/CAD", 
                       "EUR/CHF" , "EUR/GBP", "NZD/USD", "EUR/SEK", "EUR/NOK", "USD/SEK",
                       "USD/NOK", "EUR/JPY", "XAU/USD","XAG/USD","XPD/USD","XPT/USD",
                       "SPX500/USD","WTICO_USD","BCO/USD"))

assets.source <- cbind(c("api","api","api", "api", "api", "api", "api", "api",
                         "api", "api","api", "api", "api", "api", "api", "api",
                         "api","api","api","api","api"))

assets <- cbind(assets.name,assets.source)
forecasts <- matrix(nrow=nrow(assets),ncol=24)
# colnames(forecasts) <- c("Date", "Time UTC+1", "Model", "Asset", "AR Forecast", "AR Rolling Hit-Rate",
                         # "AR Alpha", "AR ACD", "MA Forecast", "MA Rolling Hit-Rate", "MA Forecast",
                         # "ARMA Forecast", "AMRA Rolling Hit-Rate", "ARMA ACD", "Forecast",
                         # "Last Input Price", "AR Alpha", "Rolling Hit-Rate", "SD", "MAD", "ACD")
intraday <-0
pt <- 0
# Sys.Date()

for (j in 1:nrow(assets)){

  # AR
  
  MA <- 0
  AR <- 1
  ARMA <- 0
  asset <- assets[j,1]
  if (assets[j,2]=="Oanda"){
    oanda <- 1
    google <- 0
    quandl <- 0
    api <- 0
    }
  if (assets[j,2]=="api"){
    oanda <- 0
    google <- 0
    quandl <- 0
    api <- 1
    }
    strSign="n.a."
    try(source("ARMA_model.R"))
    forecasts[j,4] <- strSign
    forecasts[j,14] <- tail(sc.data,1)
    forecasts[j,6] <- round(data.fit$coef[1],2)
    forecasts[j,7] <- round(data.pred$pred,4)
    try(source("ARMA_TEST.R"))
    
  forecasts[j,1] <- as.character.Date(Sys.Date())
  forecasts[j,3] <- asset
  # forecasts[j,4] <- round(data.pred$pred,4)
  forecasts[j,5] <- round(100*HitRate,1)
  forecasts[j,15] <- d.sd
  forecasts[j,16] <- d.mean
  forecasts[j,17] <- qnt.a[1]
  forecasts[j,18] <- qnt.a[2]
  forecasts[j,19] <- qnt.a[3]
  forecasts[j,20] <- qnt.a[4]
  forecasts[j,21] <- qnt.a[5]
  forecasts[j,22] <- qnt.a[6]
  forecasts[j,23] <- qnt.a[7]
  forecasts[j,24] <- qnt.a[8]
  
  # MA
  
  MA <- 1
  AR <- 0
  ARMA <- 0
  asset <- assets[j,1]
  strSign="n.a."
    try(source("ARMA_model.R"))
    forecasts[j,8] <- strSign
    forecasts[j,10] <- round(data.pred$pred,4)
    try(source("ARMA_TEST.R"))
  forecasts[j,9] <- round(100*HitRate,1)

  # ARMA
  
  MA <- 0
  AR <- 0
  ARMA <- 1
  asset <- assets[j,1]
  strSign="n.a."
    try(source("ARMA_model.R"))
    forecasts[j,11] <- strSign
    forecasts[j,13] <- round(data.pred$pred,4)
    try(source("ARMA_TEST.R"))
  forecasts[j,2] <- substr(as.character.Date(Sys.time()),12,19)
  # forecasts[j,4] <- round(data.pred$pred,4)
  forecasts[j,12] <- round(100*HitRate,1)
}


write.table(forecasts, file=paste(this.dir,"/forecasts/","ARMA_forecasts2016",".csv",sep=""),append=TRUE,col.names=FALSE,sep=",",row.names = FALSE)

end <- print(Sys.time())
print(end-start)







# Additional Code
# write.table(data, "clipboard", sep="\t", row.names=TRUE)
# typeof(data)
# row.names(data) = as.Date(data$row.name, format = '%Y-%d-%m')
# attributes(data)
# write.table(cbind(data[,0],data[,1]), "clipboard", sep="\t", row.names=TRUE)
# write.zoo(data, "clipboard", sep="\t", index.name="Date")write.zoo(data, "clipboard", sep="\t", index.name="Date")