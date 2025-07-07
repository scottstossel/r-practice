if(!"lubridate"%in%rownames(installed.packages())) {install.packages("lubridate")}
library(lubridate)
if(!"zoo"%in%rownames(installed.packages())) {install.packages("zoo")}
library(zoo)
if(!"fable"%in%rownames(installed.packages())) {install.packages("fable")}
library(fable)
if(!"feasts"%in%rownames(installed.packages())) {install.packages("feasts")}
library(feasts)
if(!"forecast"%in%rownames(installed.packages())) {install.packages("forecast")}
library(forecast)
if(!"tseries"%in%rownames(installed.packages())) {install.packages("tseries")}
library(tseries)
if(!"tsibble"%in%rownames(installed.packages())) {install.packages("tsibble")}
library(tsibble)
if(!"plyr"%in%rownames(installed.packages())) {install.packages("plyr")}
library(plyr)
if(!"dplyr"%in%rownames(installed.packages())) {install.packages("dplyr")}
library(dplyr)
if(!"ggplot2"%in%rownames(installed.packages())) {install.packages("ggplot2")}
library(ggplot2)
if(!"ggthemes"%in%rownames(installed.packages())) {install.packages("ggthemes")}
library(ggthemes)
if(!"scales"%in%rownames(installed.packages())) {install.packages("scales")}
library(scales)
if(!"gridExtra"%in%rownames(installed.packages())) {install.packages("gridExtra")}
library(gridExtra)
## ar1()

ts1.sim <- 10 + arima.sim(model=list(ar=c(0.7,0.1,0.1)), n=10000)
par(mfrow=c(2,2))
plot(ts1.sim, xlab="t", ylab="Xt", col="cornflowerblue", main="Time Series")
acf(ts1.sim, col="darkorange2", main="ACF")
pacf(ts1.sim, col="gold3", main="PACF")

ts2.sim <- 10 + arima.sim(model=list(order=c(0,0,3), ma=c(0.7,0.1,0.1)), n=10000)
par(mfrow=c(2,2))
plot(ts2.sim, xlab="t", ylab="Xt", col="cornflowerblue", main="Time Series")
acf(ts2.sim, col="darkorange2", main="ACF")
pacf(ts2.sim, col="gold3", main="PACF")

ts3.sim <- arima.sim(model=list(order=c(4,0,0), ar=rep(0.2,4)), n=10000)
ts4.sim <- arima.sim(model=list(order=c(0,0,4), ma=rep(0.2,4)), n=10000)
ts5.sim <- arima.sim(model=list(order=c(2,0,2), ar=rep(0.2,2), ma=rep(0.2,2)), n=10000)
par(mfrow=c(3,3))
plot(ts3.sim, type = "l", col="orange2", main="AR(4)")
plot(ts4.sim, type = "l", col="orange2", main="MA(4)")
plot(ts5.sim, type = "l", col="orange2", main="ARMA(2,2)")
acf(ts3.sim, col="cornflowerblue", main="AR(4)")
acf(ts4.sim, col="cornflowerblue", main="MA(4)")
acf(ts5.sim, col="cornflowerblue", main="ARMA(2,2)")
pacf(ts3.sim, col="gold3", main="AR(4)")
pacf(ts4.sim, col="gold3", main="MA(4)")
pacf(ts5.sim, col="gold3", main="ARMA(2,2)")

ts6.sim <- arima.sim(model=list(order=c(1,0,1), ar=rep(0.1,1), ma=rep(0.1,1)), n=10000)
ts7.sim <- arima.sim(model=list(order=c(0,1,0)), n=10000)
ts8.sim <- arima.sim(model=list(order=c(1,1,0), ar=rep(0.1,1)), n=10000)
ts9.sim <- arima.sim(model=list(order=c(0,1,1), ma=rep(0.1,1)), n=10000)
par(mfrow=c(3,4))
plot(ts6.sim, type = "l", col="orange2", main="ARMA(1,1)")
plot(ts7.sim, type = "l", col="orange2", main="ARIMA(0,1,0)")
plot(ts8.sim, type = "l", col="orange2", main="ARIMA(1,1,0)")
plot(ts9.sim, type = "l", col="orange2", main="ARIMA(0,1,1)")
acf(ts6.sim, col="cornflowerblue", main="ARMA(1,1)")
acf(ts7.sim, col="cornflowerblue", main="ARIMA(0,1,0)")
acf(ts8.sim, col="cornflowerblue", main="ARIMA(1,1,0)")
acf(ts9.sim, col="cornflowerblue", main="ARIMA(0,1,1)")
pacf(ts6.sim, col="gold3", main="ARMA(1,1)")
pacf(ts7.sim, col="gold3", main="ARIMA(0,1,0)")
pacf(ts8.sim, col="gold3", main="ARIMA(1,1,0)")
pacf(ts9.sim, col="gold3", main="ARIMA(0,1,1)")

ts.series1 <- arima.sim(model=list(order=c(5,0,0), ar=rep(0.1,5)), n=10000)
ts.series2 <- arima.sim(model=list(order=c(0,1,0)), n=10000)
adf.test(ts.series1, alternative = "stationary", k = 5)

PP.test(ts.series1)
PP.test(ts.series2)

dat<-read.csv("EXJPUS.csv")
dat<-dat %>%
  mutate(Date=as.Date(DATE),
         mnth=yearmonth(Date),
         Var=EXJPUS)
head(dat)

dat %>%
  ggplot(aes(Date,Var)) +
  geom_line(color="cornflowerblue") +
  theme_economist_white(gray_bg=F) +
  xlab("Date") +
  ylab("")

dat<-dat %>%
  as_tsibble(index=mnth)

choose.p <- function(x, h, p) {
  arma.model <- arima(x, order = c(p,1,0), include.mean = T)
  forecast(arma.model, h=h)
}
residual.list<-NULL
max.p<-5
for(i in 1:max.p) {
  residual.list[[i]]<-tsCV(dat$Var, choose.p, h=1, p=i)
  print(i)
}

rmse.results<-sapply(residual.list, function(r) sqrt(mean(rˆ2, na.rm=T)))
data.frame("p"=as.factor(1:max.p),"rmse"=rmse.results) %>%
  ggplot(aes(p,rmse)) +
  geom_bar(stat="identity",alpha=0.4,fill="cornflowerblue") +
  theme_economist_white(gray_bg=F) +
  xlab("P") +
  ylab("RMSE")

ar.bic<-dat %>%
  model(ARIMA(Var ~ 1 + pdq(1:5,1,0) + PDQ(0,0,0), ic="bic", stepwise=F, greedy=F))
ar.bic %>%
  report()
