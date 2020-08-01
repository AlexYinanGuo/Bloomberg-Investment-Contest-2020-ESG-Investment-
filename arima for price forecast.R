### ARIMA for daily closing price ###
# install the packages if needed #
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(readxl)

###### Yokogawa ######
# import data #
library(readxl)
yokogawa <- read_excel("Desktop/ESG/price/yokogawa.xlsx")
View(yokogawa)
plot(yokogawa$`Last Price`~yokogawa$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
yokogawa_p=yokogawa$`Last Price`
acf(yokogawa_p, main='ACF')
pacf(yokogawa_p, main='PACF')
print(adf.test(yokogawa_p)) # p-value = 0.9459, accept the null hypothesis, there is a unit root 
m1 = auto.arima(yokogawa_p, seasonal=FALSE) # auto arima(0,1,0) aic=31986.64 
tsdisplay(residuals(m1), lag.max=40, main='(0,1,0) Residuals')
# custom arima #
m2 = arima(yokogawa_p, order=c(0,1,1)) # aic = 31988.32
tsdisplay(residuals(m2), lag.max=40, main='(0,1,1) Residuals')

m3 = arima(yokogawa_p, order=c(0,1,28)) # aic = 32000.51
tsdisplay(residuals(m3), lag.max=40, main='(0,1,28) Residuals')

m4 = arima(yokogawa_p, order=c(1,1,1)) # aic = 31990.33
tsdisplay(residuals(m4), lag.max=40, main='(1,1,1) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.805404, 98.2% accuracy
accuracy(fcast2) # MAPE=1.806067, 98.2% accuracy
accuracy(fcast3) # MAPE=1.803532, 98.2% accuracy
accuracy(fcast4) # MAPE=1.806049, 98.2% accuracy


###### tokyo century ######
# import data #
library(readxl)
tokyo_century <- read_excel("Desktop/ESG/price/tokyo_century.xlsx")
View(tokyo_century)
plot(tokyo_century$`Last Price`~tokyo_century$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
tokyo_century_p=tokyo_century$`Last Price`
acf(tokyo_century_p, main='ACF')
pacf(tokyo_century_p, main='PACF')
print(adf.test(tokyo_century_p)) # p-value = 0.2025, accept the null hypothesis, there is a unit root 
m1 = auto.arima(tokyo_century_p, seasonal=FALSE) # auto arima(0,1,1) aic=37187.6
tsdisplay(residuals(m1), lag.max=40, main='(0,1,1) Residuals')
# custom arima #
m2 = arima(tokyo_century_p, order=c(0,2,1)) # aic = 37191.27
tsdisplay(residuals(m2), lag.max=40, main='(0,2,1) Residuals')

m3 = arima(tokyo_century_p, order=c(0,1,25)) # aic = 37182.4
tsdisplay(residuals(m3), lag.max=40, main='(0,1,25) Residuals')

m4 = arima(tokyo_century_p, order=c(0,1,30)) # aic = 37175.99
tsdisplay(residuals(m4), lag.max=40, main='(0,1,30) Residuals')

# forecast 40d, result: flat/down #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.783136, 98.22% accuracy
accuracy(fcast2) # MAPE=1.783288, 98.22% accuracy
accuracy(fcast3) # MAPE=1.781136, 98.22% accuracy
accuracy(fcast4) # MAPE=1.78199, 98.22% accuracy


###### tokio marine ######
# import data #
library(readxl)
tokio_marine <- read_excel("Desktop/ESG/price/tokio_marine.xlsx")
View(tokio_marine)
par(mfrow=c(1,1)) 
plot(tokio_marine$`Last Price`~tokio_marine$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
tokio_marine_p=tokio_marine$`Last Price`
acf(tokio_marine_p, main='ACF')
pacf(tokio_marine_p, main='PACF')
print(adf.test(tokio_marine_p)) # p-value = 0.8988, accept the null hypothesis, there is a unit root 
m1 = auto.arima(tokio_marine_p, seasonal=FALSE) # auto arima(2,1,3) aic=38117.86
tsdisplay(residuals(m1), lag.max=40, main='(2,1,3) Residuals')
# custom arima #
m2 = arima(tokio_marine_p, order=c(1,1,1)) # aic = 38144.18
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(tokio_marine_p, order=c(2,1,25)) # aic = 38114.32
tsdisplay(residuals(m3), lag.max=40, main='(2,1,25) Residuals')

m4 = arima(tokio_marine_p, order=c(2,1,37)) # aic = 38121.79
tsdisplay(residuals(m4), lag.max=40, main='(2,1,37) Residuals')

# forecast 40d, result: up #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.535318, 98.465% accuracy
accuracy(fcast2) # MAPE=1.53817, 98.46% accuracy
accuracy(fcast3) # MAPE=1.538359, 98.46% accuracy
accuracy(fcast4) # MAPE=1.540658, 98.46% accuracy
  

###### takeda ######
# import data #
library(readxl)
takeda <- read_excel("Desktop/ESG/price/takeda.xlsx")
View(takeda)
par(mfrow=c(1,1)) 
plot(takeda$`Last Price`~takeda$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
takeda_p=takeda$`Last Price`
acf(takeda_p, main='ACF')
pacf(takeda_p, main='PACF')
print(adf.test(takeda_p)) # p-value = 0.9493, accept the null hypothesis, there is a unit root 
m1 = auto.arima(takeda_p, seasonal=FALSE) # auto arima(0,1,2) aic=38273.02
tsdisplay(residuals(m1), lag.max=40, main='(0,1,2) Residuals')
# custom arima #
m2 = arima(takeda_p, order=c(1,1,1)) # aic = 38278.6
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(takeda_p, order=c(0,1,37)) # aic = 38288.87
tsdisplay(residuals(m3), lag.max=40, main='(0,1,37) Residuals')

m4 = arima(takeda_p, order=c(0,2,2)) # aic = 38284.76
tsdisplay(residuals(m4), lag.max=40, main='(0,2,2) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.103163, 98.9% accuracy
accuracy(fcast2) # MAPE=1.102576, 98.9% accuracy
accuracy(fcast3) # MAPE=1.102521, 98.9% accuracy
accuracy(fcast4) # MAPE=1.100923, 98.9% accuracy


###### sony ######
# import data #
library(readxl)
sony <- read_excel("Desktop/ESG/price/sony.xlsx")
View(sony)
par(mfrow=c(1,1)) 
plot(sony$`Last Price`~sony$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
sony_p=sony$`Last Price`
acf(sony_p, main='ACF')
pacf(sony_p, main='PACF')
print(adf.test(sony_p)) # p-value = 0.7839, accept the null hypothesis, there is a unit root 
m1 = auto.arima(sony_p, seasonal=FALSE) # auto arima(1,2,0) aic=39897.12 
tsdisplay(residuals(m1), lag.max=40, main='(1,2,0) Residuals')
# custom arima #
m2 = arima(sony_p, order=c(1,1,1)) # aic = 38580.02
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(sony_p, order=c(1,2,38)) # aic = 38579.83
tsdisplay(residuals(m3), lag.max=40, main='(1,2,38) Residuals')

m4 = arima(sony_p, order=c(1,1,37)) # aic = 38576.11
tsdisplay(residuals(m4), lag.max=40, main='(1,1,37) Residuals')

# forecast 40d, result: drop #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=2.138861, 97.86% accuracy
accuracy(fcast2) # MAPE=1.726667, 98.27% accuracy
accuracy(fcast3) # MAPE=1.723935, 98.28% accuracy
accuracy(fcast4) # MAPE=1.721398, 98.28% accuracy


###### shionogi ######
# import data #
library(readxl)
shionogi <- read_excel("Desktop/ESG/price/shionogi.xlsx")
View(shionogi)
par(mfrow=c(1,1)) 
plot(shionogi$`Last Price`~shionogi$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
shionogi_p=shionogi$`Last Price`
acf(shionogi_p, main='ACF')
pacf(shionogi_p, main='PACF')
print(adf.test(shionogi_p)) # p-value = 0.8224, accept the null hypothesis, there is a unit root 
m1 = auto.arima(shionogi_p, seasonal=FALSE) # auto arima(3,1,4) aic=37695.99 
tsdisplay(residuals(m1), lag.max=40, main='(3,1,4) Residuals')
# custom arima #
m2 = arima(shionogi_p, order=c(1,1,1)) # aic = 37701.26
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(shionogi_p, order=c(3,1,25)) # aic = 37707.6
tsdisplay(residuals(m3), lag.max=40, main='(3,1,25) Residuals')

m4 = arima(shionogi_p, order=c(3,2,4)) # aic = 37701.04
tsdisplay(residuals(m4), lag.max=40, main='(3,2,4) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.4346, 98.57% accuracy
accuracy(fcast2) # MAPE=1.43308, 98.57% accuracy
accuracy(fcast3) # MAPE=1.439968, 98.56% accuracy
accuracy(fcast4) # MAPE=1.434568, 98.57% accuracy


###### ricoh ######
# import data #
library(readxl)
ricoh <- read_excel("Desktop/ESG/price/ricoh.xlsx")
View(ricoh)
par(mfrow=c(1,1)) 
plot(ricoh$`Last Price`~ricoh$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
ricoh_p=ricoh$`Last Price`
acf(ricoh_p, main='ACF')
pacf(ricoh_p, main='PACF')
print(adf.test(ricoh_p)) # p-value = 0.8329, accept the null hypothesis, there is a unit root 
m1 = auto.arima(ricoh_p, seasonal=FALSE) # auto arima(1,1,0) aic= 31513.61
tsdisplay(residuals(m1), lag.max=40, main='(1,1,0) Residuals')
# custom arima #
m2 = arima(ricoh_p, order=c(1,1,1)) # aic = 31515.35
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(ricoh_p, order=c(1,1,39)) # aic = 31541.93
tsdisplay(residuals(m3), lag.max=40, main='(1,1,39) Residuals')

m4 = arima(ricoh_p, order=c(1,1,12)) # aic = 31525.42
tsdisplay(residuals(m4), lag.max=40, main='(1,1,12) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.647379, 98.35% accuracy
accuracy(fcast2) # MAPE=1.647709, 98.35% accuracy
accuracy(fcast3) # MAPE=1.647618, 98.35% accuracy
accuracy(fcast4) # MAPE=1.647198, 98.35% accuracy


###### olympus ######
# import data #
library(readxl)
olympus <- read_excel("Desktop/ESG/price/olympus.xlsx")
View(olympus)
par(mfrow=c(1,1)) 
plot(olympus$`Last Price`~olympus$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
olympus_p=olympus$`Last Price`
acf(olympus_p, main='ACF')
pacf(olympus_p, main='PACF')
print(adf.test(olympus_p)) # p-value = 0.1464, accept the null hypothesis, there is a unit root 
m1 = auto.arima(olympus_p, seasonal=FALSE) # auto arima(2,1,1) aic= 30112.53
tsdisplay(residuals(m1), lag.max=40, main='(2,1,1) Residuals')
# custom arima #
m2 = arima(olympus_p, order=c(1,1,1)) # aic = 30117.56
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(olympus_p, order=c(2,1,18)) # aic = 30083.83 
tsdisplay(residuals(m3), lag.max=40, main='(2,1,18) Residuals')

m4 = arima(olympus_p, order=c(2,1,26)) # aic = 30089.78
tsdisplay(residuals(m4), lag.max=40, main='(2,1,26) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.862297, 98.14% accuracy
accuracy(fcast2) # MAPE=1.861863, 98.14% accuracy
accuracy(fcast3) # MAPE=1.868237, 98.13% accuracy
accuracy(fcast4) # MAPE=1.868816, 98.13% accuracy



###### nippon tele ######
nippon_tele <- read_excel("Desktop/ESG/price/nippon_tele.xlsx")
View(nippon_tele)
par(mfrow=c(1,1)) 
plot(nippon_tele$`Last Price`~ nippon_tele$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
nippon_tele_p= nippon_tele$`Last Price`
acf(nippon_tele_p, main='ACF')
pacf(nippon_tele_p, main='PACF')
print(adf.test(nippon_tele_p)) # p-value = 0.9388, accept the null hypothesis, there is a unit root 
m1 = auto.arima(nippon_tele_p, seasonal=FALSE) # auto arima(2,1,1) aic= 31362.96
tsdisplay(residuals(m1), lag.max=40, main='(2,1,1) Residuals')
# custom arima #
m2 = arima(nippon_tele_p, order=c(1,1,1)) # aic = 31366.29
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(nippon_tele_p, order=c(2,1,16)) # aic = 31380.7
tsdisplay(residuals(m3), lag.max=40, main='(2,1,16) Residuals')

m4 = arima(nippon_tele_p, order=c(2,1,30)) # aic = 31365.26
tsdisplay(residuals(m4), lag.max=40, main='(2,1,30) Residuals')

# forecast 40d, result: up #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.162787, 98.84% accuracy
accuracy(fcast2) # MAPE=1.164999, 98.84% accuracy
accuracy(fcast3) # MAPE=1.164346, 98.84% accuracy
accuracy(fcast4) # MAPE=1.165683, 98.835% accuracy


###### nec ######
# import data #
library(readxl)
nec <- read_excel("Desktop/ESG/price/nec.xlsx")
View(nec)
par(mfrow=c(1,1)) 
plot(nec$`Last Price`~ nec$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
nec_p= nec$`Last Price`
acf(nec_p, main='ACF')
pacf(nec_p, main='PACF')
print(adf.test(nec_p)) # p-value =  0.7119, accept the null hypothesis, there is a unit root 
m1 = auto.arima(nec_p, seasonal=FALSE) # auto arima(1,1,0) aic= 38009.84
tsdisplay(residuals(m1), lag.max=40, main='(1,1,0) Residuals')
# custom arima #
m2 = arima(nec_p, order=c(1,1,1)) # aic = 38011.28
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(nec_p, order=c(1,1,35)) # aic = 38032.42
tsdisplay(residuals(m3), lag.max=40, main='(1,1,35) Residuals')

m4 = arima(nec_p, order=c(1,1,39)) # aic = 38029.99 
tsdisplay(residuals(m4), lag.max=40, main='(1,1,39) Residuals')

# forecast 40d, result: drop #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.584541, 98.42% accuracy
accuracy(fcast2) # MAPE=1.584111, 98.42% accuracy
accuracy(fcast3) # MAPE=1.592554, 98.41% accuracy
accuracy(fcast4) # MAPE=1.594607, 98.41% accuracy


###### murata ######
# import data #
library(readxl)
murata <- read_excel("Desktop/ESG/price/murata.xlsx")
View(murata)
par(mfrow=c(1,1)) 
plot(murata$`Last Price`~ murata$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
murata_p= murata$`Last Price`
acf(murata_p, main='ACF')
pacf(murata_p, main='PACF')
print(adf.test(murata_p)) # p-value = 0.5457, accept the null hypothesis, there is a unit root 
m1 = auto.arima(murata_p, seasonal=FALSE) # auto arima(0,1,1) aic= 38694.28 
tsdisplay(residuals(m1), lag.max=40, main='(0,1,1) Residuals')
# custom arima #
m2 = arima(murata_p, order=c(0,1,15)) # aic = 38699.72
tsdisplay(residuals(m2), lag.max=40, main='(0,1,15) Residuals')

m3 = arima(murata_p, order=c(0,1,39)) # aic = 38712.88
tsdisplay(residuals(m3), lag.max=40, main='(0,1,39) Residuals')

m4 = arima(murata_p, order=c(1,1,1)) # aic = 38696.27
tsdisplay(residuals(m4), lag.max=40, main='(1,1,1) Residuals')

# forecast 40d, result: flat/up #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.691817, 98.3% accuracy
accuracy(fcast2) # MAPE=1.694083, 98.3% accuracy
accuracy(fcast3) # MAPE=1.703914, 98.3% accuracy
accuracy(fcast4) # MAPE=1.691842, 98.3% accuracy


###### ms&ad ######
# import data #
library(readxl)
msad <- read_excel("Desktop/ESG/price/msad.xlsx")
View(msad)
par(mfrow=c(1,1)) 
plot(msad$`Last Price`~ msad$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
msad_p= msad$`Last Price`
acf(msad_p, main='ACF')
pacf(msad_p, main='PACF')
print(adf.test(msad_p)) # p-value = , accept the null hypothesis, there is a unit root 
m1 = auto.arima(msad_p, seasonal=FALSE) # auto arima(3,1,3) aic= 33470.6
tsdisplay(residuals(m1), lag.max=40, main='(3,1,3) Residuals')
# custom arima #
m2 = arima(msad_p, order=c(1,1,1)) # aic = 33480.42
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(msad_p, order=c(3,1,37)) # aic = 33462.28
tsdisplay(residuals(m3), lag.max=40, main='(3,1,37) Residuals')

m4 = arima(msad_p, order=c(3,1,5)) # aic = 33460.94
tsdisplay(residuals(m4), lag.max=40, main='(3,1,5) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.615361, 98.4% accuracy
accuracy(fcast2) # MAPE=1.608636, 98.4% accuracy
accuracy(fcast3) # MAPE=1.617396, 98.4% accuracy
accuracy(fcast4) # MAPE=1.609118, 98.4% accuracy


###### mitsubishi ######
# import data #
library(readxl)
mitsubishi<- read_excel("Desktop/ESG/price/mitsubishi.xlsx")
View(mitsubishi)
par(mfrow=c(1,1)) 
plot(mitsubishi$`Last Price`~ mitsubishi$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
mitsubishi_p= mitsubishi$`Last Price`
acf(mitsubishi_p, main='ACF')
pacf(mitsubishi_p, main='PACF')
print(adf.test(mitsubishi_p)) # p-value = 0.4165, accept the null hypothesis, there is a unit root 
m1 = auto.arima(mitsubishi_p, seasonal=FALSE) # auto arima(0,1,3) aic= 35042.68
tsdisplay(residuals(m1), lag.max=40, main='(0,1,3) Residuals')
# custom arima #
m2 = arima(mitsubishi_p, order=c(1,1,1)) # aic = 35050.08
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(mitsubishi_p, order=c(0,1,25)) # aic = 35042.14
tsdisplay(residuals(m3), lag.max=40, main='(0,1,25) Residuals')

m4 = arima(mitsubishi_p, order=c(1,1,3)) # aic = 35044.69
tsdisplay(residuals(m4), lag.max=40, main='(1,1,3) Residuals')

# forecast 40d, result: flat/up #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.495055, 98.5% accuracy
accuracy(fcast2) # MAPE=1.493123, 98.5% accuracy
accuracy(fcast3) # MAPE=1.49361, 98.5% accuracy
accuracy(fcast4) # MAPE=1.495028, 98.5% accuracy


###### kurita water ######
# import data #
library(readxl)
kurita_water<- read_excel("Desktop/ESG/price/kurita_water.xlsx")
View(kurita_water)
par(mfrow=c(1,1)) 
plot(kurita_water$`Last Price`~ kurita_water$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
kurita_water_p= kurita_water$`Last Price`
acf(kurita_water_p, main='ACF')
pacf(kurita_water_p, main='PACF')
print(adf.test(kurita_water_p)) # p-value = 0.2838, accept the null hypothesis, there is a unit root 
m1 = auto.arima(kurita_water_p, seasonal=FALSE) # auto arima(2,1,2) aic= 35868.31
tsdisplay(residuals(m1), lag.max=40, main='(2,1,2) Residuals')
# custom arima #
m2 = arima(kurita_water_p, order=c(1,1,1)) # aic = 35886.23
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(kurita_water_p, order=c(2,1,21)) # aic = 35862.21
tsdisplay(residuals(m3), lag.max=40, main='(2,1,21) Residuals')

m4 = arima(kurita_water_p, order=c(2,1,39)) # aic = 35874.95
tsdisplay(residuals(m4), lag.max=40, main='(2,1,39) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.406341, 98.6% accuracy
accuracy(fcast2) # MAPE=1.409237, 98.6% accuracy
accuracy(fcast3) # MAPE=1.4048, 98.6% accuracy
accuracy(fcast4) # MAPE=1.402626, 98.6% accuracy


###### japan_avi ######
# import data #
library(readxl)
japan_avi<- read_excel("Desktop/ESG/price/japan_avi.xlsx")
View(japan_avi)
par(mfrow=c(1,1)) 
plot(japan_avi$`Last Price`~ japan_avi$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
japan_avi_p= japan_avi$`Last Price`
acf(japan_avi_p, main='ACF')
pacf(japan_avi_p, main='PACF')
print(adf.test(japan_avi_p)) # p-value = 0.6814, accept the null hypothesis, there is a unit root 
m1 = auto.arima(japan_avi_p, seasonal=FALSE) # auto arima(2,1,2) aic= 33337.14
tsdisplay(residuals(m1), lag.max=40, main='(2,1,2) Residuals')
# custom arima #
m2 = arima(japan_avi_p, order=c(1,1,1)) # aic = 33348.61
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(japan_avi_p, order=c(2,1,35)) # aic = 33347.84
tsdisplay(residuals(m3), lag.max=40, main='(2,1,35) Residuals')

m4 = arima(japan_avi_p, order=c(2,1,16)) # aic = 33344.05
tsdisplay(residuals(m4), lag.max=40, main='(2,1,16) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=2.018777, 98% accuracy
accuracy(fcast2) # MAPE=2.020153, 98% accuracy
accuracy(fcast3) # MAPE=2.014854, 98% accuracy
accuracy(fcast4) # MAPE=2.012493, 98% accuracy


###### ibiden ######
# import data #
library(readxl)
ibiden<- read_excel("Desktop/ESG/price/ibiden.xlsx")
View(ibiden)
par(mfrow=c(1,1)) 
plot(ibiden$`Last Price`~ ibiden$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
ibiden_p= ibiden$`Last Price`
acf(ibiden_p, main='ACF')
pacf(ibiden_p, main='PACF')
print(adf.test(ibiden_p)) # p-value = 0.7235, accept the null hypothesis, there is a unit root 
m1 = auto.arima(ibiden_p, seasonal=FALSE) # auto arima(2,1,1) aic= 39004.3
tsdisplay(residuals(m1), lag.max=40, main='(2,1,1) Residuals')
# custom arima #
m2 = arima(ibiden_p, order=c(1,1,1)) # aic = 39010.9
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(ibiden_p, order=c(2,1,19)) # aic = 38925.04 
tsdisplay(residuals(m3), lag.max=40, main='(2,1,19) Residuals')

m4 = arima(ibiden_p, order=c(2,1,36)) # aic = 38883
tsdisplay(residuals(m4), lag.max=40, main='(2,1,36) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.932728, 98% accuracy
accuracy(fcast2) # MAPE=1.934788, 98% accuracy
accuracy(fcast3) # MAPE=1.968075, 98% accuracy
accuracy(fcast4) # MAPE=1.979022, 98% accuracy


###### hitachi ######
# import data #
library(readxl)
hitachi <- read_excel("Desktop/ESG/price/hitachi.xlsx")
View(hitachi)
par(mfrow=c(1,1)) 
plot(hitachi$`Last Price`~ hitachi$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
hitachi_p= hitachi$`Last Price`
acf(hitachi_p, main='ACF')
pacf(hitachi_p, main='PACF')
print(adf.test(hitachi_p)) # p-value = 0.6028, accept the null hypothesis, there is a unit root 
m1 = auto.arima(hitachi_p, seasonal=FALSE) # auto arima(0,1,0) aic= 36792.65 
tsdisplay(residuals(m1), lag.max=40, main='(0,1,0) Residuals')
# custom arima #
m2 = arima(hitachi_p, order=c(1,1,1)) # aic = 36795.95
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(hitachi_p, order=c(0,1,8)) # aic = 36798.4
tsdisplay(residuals(m3), lag.max=40, main='(0,1,8) Residuals')

m4 = arima(hitachi_p, order=c(0,1,39)) # aic = 36830.63
tsdisplay(residuals(m4), lag.max=40, main='(0,1,39) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.576209, 98.42% accuracy
accuracy(fcast2) # MAPE=1.576832, 98.42% accuracy
accuracy(fcast3) # MAPE=1.579006, 98.42% accuracy
accuracy(fcast4) # MAPE=1.579729, 98.42% accuracy



###### fujitsu ######
# import data #
library(readxl)
fujitsu <- read_excel("Desktop/ESG/price/fujitsu.xlsx")
View(fujitsu)
par(mfrow=c(1,1)) 
plot(fujitsu$`Last Price`~ fujitsu$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
fujitsu_p= fujitsu$`Last Price`
acf(fujitsu_p, main='ACF')
pacf(fujitsu_p, main='PACF')
print(adf.test(fujitsu_p)) # p-value = 0.4062, accept the null hypothesis, there is a unit root 
m1 = auto.arima(fujitsu_p, seasonal=FALSE) # auto arima(2,1,3) aic= 42025.04
tsdisplay(residuals(m1), lag.max=40, main='(2,1,3) Residuals')
# custom arima #
m2 = arima(fujitsu_p, order=c(1,1,1)) # aic = 42042.58
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(fujitsu_p, order=c(2,1,21)) # aic = 42044.57
tsdisplay(residuals(m3), lag.max=40, main='(2,1,21) Residuals')

m4 = arima(fujitsu_p, order=c(2,2,3)) # aic = 42035.59
tsdisplay(residuals(m4), lag.max=40, main='(2,2,3) Residuals')

# forecast 40d, result: flat/up? #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.662961, 98.34% accuracy
accuracy(fcast2) # MAPE=1.662949, 98.34% accuracy
accuracy(fcast3) # MAPE=1.662115, 98.34% accuracy
accuracy(fcast4) # MAPE=1.661207, 98.34% accuracy


###### fuji ######
# import data #
library(readxl)
fuji <- read_excel("Desktop/ESG/price/fuji.xlsx")
View(fuji)
par(mfrow=c(1,1)) 
plot(fuji$`Last Price`~ fuji$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
fuji_p= fuji$`Last Price`
acf(fuji_p, main='ACF')
pacf(fuji_p, main='PACF')
print(adf.test(fuji_p)) # p-value = 0.9366, accept the null hypothesis, there is a unit root 
m1 = auto.arima(fuji_p, seasonal=FALSE) # auto arima(2,1,3) aic= 35716.37
tsdisplay(residuals(m1), lag.max=40, main='(2,1,3) Residuals')
# custom arima #
m2 = arima(fuji_p, order=c(1,1,1)) # aic = 35711.51
tsdisplay(residuals(m2), lag.max=40, main='(1,1,1) Residuals')

m3 = arima(fuji_p, order=c(2,1,22)) # aic = 35738.56
tsdisplay(residuals(m3), lag.max=40, main='(2,1,22) Residuals')

m4 = arima(fuji_p, order=c(2,2,3)) # aic = 35716.85
tsdisplay(residuals(m4), lag.max=40, main='(2,2,3) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.89728, 98.1% accuracy
accuracy(fcast2) # MAPE=1.896703, 98.1% accuracy
accuracy(fcast3) # MAPE=1.896936, 98.1% accuracy
accuracy(fcast4) # MAPE=1.900863, 98.1% accuracy


###### eizo ######
# import data #
library(readxl)
eizo <- read_excel("Desktop/ESG/price/eizo.xlsx")
View(eizo)
par(mfrow=c(1,1)) 
plot(eizo$`Last Price`~ eizo$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
eizo_p= eizo$`Last Price`
acf(eizo_p, main='ACF')
pacf(eizo_p, main='PACF')
print(adf.test(eizo_p)) # p-value = 0.7916, accept the null hypothesis, there is a unit root 
m1 = auto.arima(eizo_p, seasonal=FALSE) # auto arima(1,1,1) aic= 37105.97
tsdisplay(residuals(m1), lag.max=40, main='(1,1,1) Residuals')
# custom arima #
m2 = arima(eizo_p, order=c(1,1,4)) # aic = 37106.16
tsdisplay(residuals(m2), lag.max=40, main='(1,1,4) Residuals')

m3 = arima(eizo_p, order=c(1,1,25)) # aic = 37115.11
tsdisplay(residuals(m3), lag.max=40, main='(1,1,25) Residuals')

m4 = arima(eizo_p, order=c(1,2,1)) # aic = 37112.49
tsdisplay(residuals(m4), lag.max=40, main='(1,2,1) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.599829, 98.4% accuracy
accuracy(fcast2) # MAPE=1.59757, 98.4% accuracy
accuracy(fcast3) # MAPE=1.596165, 98.4% accuracy
accuracy(fcast4) # MAPE=1.596552, 98.4% accuracy


###### astellas ######
# import data #
library(readxl)
astellas <- read_excel("Desktop/ESG/price/astellas.xlsx")
View(astellas)
par(mfrow=c(1,1)) 
plot(astellas$`Last Price`~ astellas$Date, yabl='Last Price', type='l')
# plot ACF & PACF, ACF->P, PACF->Q #
par(mfrow=c(1,2)) 
astellas_p= astellas$`Last Price`
acf(astellas_p, main='ACF')
pacf(astellas_p, main='PACF')
print(adf.test(astellas_p)) # p-value = 0.8038, accept the null hypothesis, there is a unit root 
m1 = auto.arima(astellas_p, seasonal=FALSE) # auto arima(1,1,1) aic= 29720.04
tsdisplay(residuals(m1), lag.max=40, main='(1,1,1) Residuals')
# custom arima #
m2 = arima(astellas_p, order=c(1,1,22)) # aic = 29708.23
tsdisplay(residuals(m2), lag.max=40, main='(1,1,21) Residuals')

m3 = arima(astellas_p, order=c(1,1,39)) # aic = 29716.25
tsdisplay(residuals(m3), lag.max=40, main='(1,1,39) Residuals')

m4 = arima(astellas_p, order=c(1,2,1)) # aic = 29727.66
tsdisplay(residuals(m4), lag.max=40, main='(1,2,1) Residuals')

# forecast 40d, result: flat #
par(mfrow=c(2,2))
term<-40
fcast1 <- forecast(m1, h=term)
plot(fcast1, include=10)
fcast2 <- forecast(m2, h=term)
plot(fcast2, include=10)
fcast3 <- forecast(m3, h=term)
plot(fcast3, include=10)
fcast4 <- forecast(m4, h=term)
plot(fcast4, include=10)

# accuracy #
accuracy(fcast1) # MAPE=1.246058, 98.75% accuracy
accuracy(fcast2) # MAPE=1.24353, 98.76% accuracy
accuracy(fcast3) # MAPE=1.24381, 98.76% accuracy
accuracy(fcast4) # MAPE=1.247838, 98.75% accuracy

