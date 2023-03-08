setwd("D:/szkoła/projekty/szeregi_czasowe")
getwd()
library(forecast)

library(lubridate)

#wczytanie danych----

air_india<-read.csv("air_india.csv",sep = ",")
air_india<-air_india[-c(1:1137),]
air<-as.numeric(air_india$PM2.5)

#zmieniam dane z godzinnych na dzienne by ulatwic wizualizacje
temp<-air_india[!duplicated(air_india[2:4]),]
temp2<-as.numeric(temp$PM2.5)


air_ts<-ts(data = temp2,start=decimal_date(ymd("2018-01-01")),frequency = 365) 


nifty<-read.csv("NIFTY.csv",sep ="," )
nifop<-as.numeric(nifty$Open)

kurs_ts<-ts(data =nifop ,start=decimal_date(ymd("2007-10-01")),frequency = 12) 

#wizualizacja b ----

plot(air_ts,type="l",col="green",main="Jakość powietrza w Indiach",xlab="data",ylab="jakość powietrza")
plot(kurs_ts,type="l",col="blue",main="Kurs akcji Nifty50",xlab="lata",ylab="cena")



lag.plot(kurs_ts,lags=12,do.lines = FALSE,diag.col = "red",col="black"
         ,main = "Wykres rozrzutu dla kursu NIFTY50")
lag.plot(air_ts,lags=30,do.lines = FALSE,diag.col = "red",col= "black",main = "Wykres rozrzutu dla jakości powietrza")

seasonplot(air_ts,  col = rainbow(5),  year.labels = TRUE,  
           pch = 20,main = "Wykres sezonowy jakość powietrza w Indiach") 
seasonplot(kurs_ts,  col = rainbow(12),  year.labels = TRUE,  
           pch = 20,main = "Wykres sezonowy dla kursu NIFTY50") 

monthplot(kurs_ts,main="kurs nifty 50",ylab = "mln")

boxplot(kurs_ts~cycle(kurs_ts),col="blue",
        main="kurs NIFTY50",xlab = "",ylab = "w mln")
boxplot(air_ts~cycle(air_ts),col="green",
        main="Jakość powietrza w Indiach",xlab = "",ylab = "w mln")

acf(air_ts,main="Wykres acf jakości powietrza w Indiach")
acf(kurs_ts,main="Wykres acf kursu nifty 50")

pacf(air_ts,main="Wykres pacf powietrza w Indiach")
pacf(kurs_ts,main="Wykres pacf kursu nifty 50")

tsdisplay(air_ts,main = "Wykres tsdisplay dla jakości powietrza w Indiach",col="green")
tsdisplay(kurs_ts,main="Wykres tsdisplay dla kursu NIFTY50",col="blue")

#dekompozycje c ----

air_d<-decompose(air_ts)
plot(air_d,col="green")
plot(air_d$seasonal,col="green")
plot(air_d$trend,col="green")
plot(air_d$random,col="green")

kurs_d<-decompose(kurs_ts)
plot(kurs_d,col="blue")
plot(kurs_d$seasonal,col="blue")
plot(kurs_d$trend,col="blue")
plot(kurs_d$random,col="blue")


#druga----
air_ts2<-air_ts
air_ts2T<-tslm(air_ts2~trend)
str(air_ts2T)
summary(air_ts2T)
air_ts2TS<-tslm(air_ts2 ~ trend + season)
plot(air_ts2,col="green",main="Dekompozycja jakości powietrza w Indiach")
lines(fitted(air_ts2T), col = "blue", lty = 2) 
lines(fitted(air_ts2TS), col = "red", lty = 2) 
air_ts2D <- decompose(air_ts2, type = "multiplicative")
tsdisplay(air_ts2D$random,col="green") 
tsdisplay(residuals(air_ts2TS),col="green") 

kurs_ts1<-kurs_ts
kurs_ts2T<-tslm(kurs_ts1~trend)
str(kurs_ts2T)
summary(kurs_ts2T)
kurs_ts2TS<-tslm(kurs_ts1 ~ trend + season)
plot(kurs_ts1,col="blue",main="Dekompozycja kursu NIFTY50")
lines(fitted(kurs_ts2T), lty = 2) 
lines(fitted(kurs_ts2TS), col = "red", lty = 2) 
kurs_tsD <- decompose(kurs_ts1, type = "multiplicative")
tsdisplay(kurs_tsD$random,col="blue") 
tsdisplay(residuals(kurs_ts2TS),col="blue") 

#dekompozycja na podstawie regresji----
air_tsT2<- tslm(air_ts2 ~ poly(trend, raw=TRUE, degree = 2)) 
plot(air_ts2,main="Trend wielomianowy - powietrze w Indiach",col="green") 
lines(fitted(air_tsT2), lty=2, lwd = 3, col = "black") 

kurs_tsT2 <- tslm(kurs_ts1 ~ poly(trend, raw=TRUE, degree = 2)) 
plot(kurs_ts1,main="Trend wielomianowy - kurs NIFTY50",col="blue") 
lines(fitted(kurs_tsT2),lty=2, lwd = 3, col = "black") 


#eliminacja trendu i sezonowosci d ----
#odsezonowanie
airt <- decompose(air_ts2, type = "multiplicative")
airSA<-seasadj(airt)
plot(air_ts2,col="green",main="eliminacja trendu i sezonowosci - powietrze w Indiach")
lines(airSA,lty = 1) 

kurst <- decompose(kurs_ts1, type = "multiplicative")
kursSA<-seasadj(kurst)
plot(kurs_ts1, col="blue",main="eliminacja trendu i sezonowosci - kurs NIFTY50",lwd=2)
lines(kursSA,col="red", lty = 1,lwd=1) 

#uczynienie stacjonarnymi powietrze e ----


tsdisplay(air_ts2)
air_tsdb<-BoxCox(air_ts2,lambda=0)
air_ts12<-diff(air_tsdb,lag=12)
tsdisplay(air_ts12,col="green")
air_tsl1<-diff(air_ts12,lag=1)
tsdisplay(air_tsl1,col="green")

Acf(air_tsl1, lag.max=100,col="green")
Pacf(air_tsl1, lag.max=100,col="green")

#uczynienie stacjonarnymi kurs----


tsdisplay(kurs_ts1)
kurs_tsb<-BoxCox(kurs_ts1,lambda=0)
kurs_ts12<-diff(kurs_tsb,lag=12)
tsdisplay(kurs_ts12,col="blue")
kurs_tsl1<-diff(kurs_ts12,lag=1)
tsdisplay(kurs_tsl1,col="blue")

Acf(kurs_tsl1, lag.max=100,col="blue") 
Pacf(kurs_tsl1,lag.max=100,col="blue")


#wyznaczanie wspolczynnikow modelu AR powietrze f ----
plot(air_ts2, main = "Jakosc powietrza Indie") 
tsdisplay(air_ts2,col="green") 
lamb.auto <- BoxCox.lambda(air_ts2)
air_ts2L <- BoxCox(air_ts2, lambda = lamb.auto) 
tsdisplay(air_ts2L,col="green") 
air_ts2LS <- diff(air_ts2L, lag = 12) 
tsdisplay(air_ts2LS,col="green") 
air_ts2LST <- diff(air_ts2LS, lag = 1) 
tsdisplay(air_ts2LST,col="green") 

Acf(air_ts2LST, lag.max = 50,col="green",lwd=2)
Acf(air_ts2LST, lag.max = 90,col="green",lwd=2)

Pacf (air_ts2LST, lag.max = 50,lwd=2,col="green")
Pacf (air_ts2LST, lag.max = 90,lwd=2,col="green")

air_yw<- ar(air_ts2LST, aic = FALSE,order.max = 84, method = "yule-walker")
print(air_yw)

air_mle<- ar(air_ts2LST, aic = FALSE,order.max = 24, method = "mle")
print(air_mle)


air_yw2<- ar(air_ts2LST, aic = TRUE,order.max = 100, method = "yule-walker")
print(air_yw2)


air_mle2<- ar(air_ts2LST, aic = TRUE,order.max = 100, method = "mle")
print(air_mle2)
#wyznaczanie wspolczynnikow modelu AR kurs----

plot(kurs_ts1, main = "kurs") 
tsdisplay(kurs_ts1,col="blue") 
(lamb.auto <- BoxCox.lambda(kurs_ts1)) 
kurs_ts1L <- BoxCox(kurs_ts1, lambda = lamb.auto) 
tsdisplay(kurs_ts1L,col="blue") 
kurs_ts1LS <- diff(kurs_ts1L, lag = 12) 
tsdisplay(kurs_ts1LS,col="blue") 
kurs_ts1LST <- diff(kurs_ts1LS, lag = 1) 
tsdisplay(kurs_ts1LST,col="blue") 

Acf(kurs_ts1LST, lag.max = 50,col="blue",lwd=2)

Pacf (kurs_ts1LST, lag.max = 50,col="blue",lwd=2)

kurs_yw<- ar(kurs_ts1LST, aic = FALSE,order.max = 24, method = "yule-walker")
print(kurs_yw)

kurs_mle <- ar(kurs_ts1LST, aic = FALSE,order.max = 24, method = "mle")
print(kurs_mle)

kurs_yw1 <- ar(kurs_ts1LST, aic = TRUE, order.max = 100, method = "yule-walker")
print(kurs_yw1)
kurs_mle1 <- ar(kurs_ts1LST, aic = TRUE,order.max = 100, method = "mle")
print(kurs_mle1)



#wyznaczanie wspolczynnikow ar g----
#powietrze----
#szum bialy????
air_arLST<- Arima(air_ts2LST, order = c(23,0,0)) 
summary(air_arLST) 
plot(air_ts2LST)
#sezonowosc usunieta
air_arLS <- Arima(air_ts2LS, order = c(23,1,0)) 
summary(air_arLS) 
plot(air_ts2LS)
#sezonowosc
air_arL <- Arima(air_ts2L, order = c(23,1,0),seasonal = list(order=c(0,1,0), period = 12)) 
summary(air_arL) 
plot(air_ts2L)


summary(air_arLST2)
summary(air_arLST) 
#kurs----
kurs_arLST<- Arima(kurs_ts1LST, order = c(23,0,0)) 
summary(kurs_arLST) 
plot(kurs_ts1LST)

#sezonowosc usunieta
kurs_arLS <- Arima(kurs_ts1LS, order = c(23,1,0)) 
summary(kurs_arLS) 
plot(kurs_ts1LS)
#sezonowosc
kurs_arL <- Arima(kurs_ts1L, order = c(23,1,0),seasonal = list(order=c(0,1,0), period = 12)) 
summary(kurs_arL) 
plot(kurs_ts1L)

summary(kurs_arLST2)
summary(kurs_arLST) 


#aicc h ----
#powietrze
air_ts2LST_aicc <- auto.arima(air_ts2LST, ic = c("aicc"))
air_ts2LS_aicc <- auto.arima(air_ts2LS, ic = c("aicc"))

#kurs
kurs_ts1LST_aicc <- auto.arima(kurs_ts1LST, ic = c("aicc"))
kurs_ts1LS_aicc <- auto.arima(kurs_ts1LS, ic = c("aicc"))
kurs_ts1L_aicc <- auto.arima(kurs_ts1L, ic = c("aicc"))
kurs_ts1_aicc <- auto.arima(kurs_ts1, ic = c("aicc"), lambda = "auto")
#porownanie i----
summary(air_ts2LST_aicc)
summary(air_ts2LS_aicc)


summary(kurs_ts1LST_aicc)
summary(kurs_ts1LS_aicc)
summary(kurs_ts1L_aicc)
summary(kurs_ts1_aicc)
#prognozowanie naiwne j ----
#srednia
air_meanf<-meanf(air_ts2,h=100)
plot(air_meanf,main="Prognozowanie jakosci poweitrza na podstawie sredniej",col="green")

kurs_meanf<-meanf(kurs_ts1,h=100)
plot(kurs_meanf,main="prognozowanie kursu na podstawie sredniej",col="blue")

#naiwna
air_naive <- naive(air_ts2, h=150) 
plot(air_naive,main= "Prognoza jakosci powietrza z wykorzystaniem metody naiwnej",col = "green") 

kurs_naive <- naive(kurs_ts1, h=50) 
plot(kurs_naive,main= "Prognoza kursu Nifty z wykorzystaniem metody naiwnej",col="blue") 

air_snaive <- snaive(air_ts2, h=500) 
plot(air_snaive, main = "Prognoza jakosci powietrza na podstawie metody naiwnej sezonowej",col="green") 

kurs_snaive <- snaive(kurs_ts1, h=50) 
plot(kurs_snaive, main = "Prognoza kursu Nifty na podstawie metody naiwnej sezonowej",col="blue") 

#metoda z uwzglednieniem dryfu
air_dryf <- rwf(air_ts2, h = 100, drift = TRUE) 
plot(air_dryf, main = "Prognoza jakosci powietrza na podstawie metody uwzgl. dryf",col = "green")

kurs_dryf <- rwf(kurs_ts1, h = 40, drift = TRUE)
plot(kurs_dryf, main = "Prognoza kursu Nifty na podstawie metody uwzgl. dryf",col="blue")

(accuracy(air_meanf))
(accuracy(air_naive))
(accuracy(air_snaive))
(accuracy(air_dryf))

(accuracy(kurs_meanf))
(accuracy(kurs_naive))
(accuracy(kurs_snaive))
(accuracy(kurs_dryf))
