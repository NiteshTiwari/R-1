
rm(list=ls())

require(rugarch)
require(urca)
require(quantmod)
require(PerformanceAnalytics)
require(ggplot2)
require(gridExtra)

standalone <- 1

if (standalone==1) {
  rm(list=ls())
  print('GARCH_model: standalone = TRUE')
  asset <- "EUR/USD"
  oanda <- 1
  google <- 0
  quandl <- 0
  i <- 1
  AR <- 1
  MA <- 0
  ARMA <- 0
  del.sat <- 1
  add.data <- 0
  intraday <- 0
}

source('Data_Retrieve.R')
data <- sc.data
data_fd <- diff(data)
n <- nrow(sc.data)

#get log returns, could also use ROC with type = "continuous"
LG.data<-log(data)
retData<-diff(data)

#data frame allows us to use ggplot with date data from xts
#I have not found any better way to ggplot xts data
df1<-data.frame(index(data),coredata(data))
colnames(df1)<-c("dates",asset)

par(mfrow=c(3,3))
### Plot price:
gg1.1 <- ggplot(df1,aes(dates,data)) + xlab(NULL) + ylab("price") + scale_y_continuous() 
gg1.2 <- gg1.1+geom_line(colour="darkblue")  + ggtitle("asset price")
# print(gg1.2)

#set first return to 0
retData[1]<-0

df2<-data.frame(index(retData),coredata(retData))
colnames(df2)<-c("dates","price")

gg2.1<-ggplot(df2,aes(dates,price)) + xlab(NULL) + ylab("FD") + scale_y_continuous() 
gg2.2<-gg2.1+geom_line(colour="darkred") + ggtitle("Asset Price FD")
# print(gg2.2)

### ACFs and PACFs
par(mfrow=c(2,1))
acf(retData, main="ACF of FD", lag = 50)
pacf(retData, main="PACF FD", lag = 50)

ar9<-arima(retData, order=c(9,0,0))
acf(ar9$residuals)

ressq<-(ar9$residuals)^2

Box.test(ressq, lag = 8, type = "Ljung-Box")

pacf(ressq, main="PACF of Squared Residuals", lag = 30)

# Note that the GARCH order is revered from above
specm1 <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(2,4), submodel = NULL),
                     mean.model=list(armaOrder=c(9,0), include.mean=TRUE))
#this might take a while
fitm1 <- ugarchfit(data = retData, spec = specm1)
fitm1
plot(fitm1, which="all") #use option 8

fittedmodel <- fitm1@fit
sigma1<-fittedmodel$sigma

df2<-data.frame(index(retData),coredata(retData),sigma1)
colnames(df2)<-c("dates","price","sigma1")

gg3.1 <- ggplot(df2,aes(dates)) + xlab(NULL) + ylab("Log Changes")
gg3.2 <- gg3.1 + geom_line(aes(y = price, colour="Diff Returns")) + ggtitle("FD with 2 Conditional sd")
gg3.3 <- gg3.2 + geom_line(aes(y = sigma1*2, colour="2 S.D.")) + geom_line(aes(y = sigma1*-2, colour="2 S.D.")) + scale_colour_hue("Series:")
gg3.3

fitm2 <- ugarchfit(data = retData,out.sample = 6, spec = specm1)
fitm2
pred <- ugarchforecast(fitm2, n.ahead = 6,n.roll = 0)
pred.fpm <- fpm(pred)
pred.fpm

signal<-runMean(as.xts(fittedmodel$z,order.by=index(retData)),50)
# # chartSeries(signal)
signal<-lag(signal,k=1)
signal[is.na(signal)]<-0

ret<-ifelse(signal > 0,ROC(data[,1],1,type="discrete"),0)
returnCompare<-merge(ret,ROC(data[,1],1,type="discrete"))
colnames(returnCompare)<-c("ZSystem","price")
charts.PerformanceSummary(returnCompare, ylog=TRUE, main="Just for Fun Z System")

grid.arrange(gg1.2, gg2.2, gg3.3, nrow=3, ncol=3)