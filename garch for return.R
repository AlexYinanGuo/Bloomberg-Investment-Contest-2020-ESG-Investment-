### GARCH for day-to-day return (open price minus closing price) ###
library(quantmod)
library(rugarch)
library(forecast)
library(readxl)
library(stats)

### yokogawa ###
# import data #
yokogawa <- read_excel("Desktop/ESG/day-to-day-return/yokogawa.xlsx")
View(yokogawa)
par(mfrow=c(1,1)) 
plot(yokogawa$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~yokogawa$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
yokogawa_r=yokogawa$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(yokogawa_r, seasonal=FALSE) # auto arima(0,0,0)
yokogawa_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model='std')
yokogawa_garch <- ugarchfit(spec=yokogawa_spec, data=yokogawa_r)
# forecast #
yokogawa_f <- ugarchforecast(yokogawa_garch, n.ahead=40)
yokogawa_return_f <- yokogawa_f@forecast$seriesFor
yokogawa_volatility_f <- yokogawa_f@forecast$sigmaFor
plot(yokogawa_return_f, type='l') 
plot(yokogawa_volatility_f, type='l') 


### tokyo century ###
# import data #
tokyo_century <- read_excel("Desktop/ESG/day-to-day-return/tokyo_century.xlsx")
View(tokyo_century)
plot(tokyo_century$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ tokyo_century$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
tokyo_century_r= tokyo_century$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(tokyo_century_r, seasonal=FALSE) # auto arima(0,0,1)
fit
tokyo_century_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,1)), distribution.model='std')
tokyo_century_garch <- ugarchfit(spec= tokyo_century_spec, data= tokyo_century_r)
# forecast #
tokyo_century_f <- ugarchforecast(tokyo_century_garch, n.ahead=40)
tokyo_century_return_f <- tokyo_century_f@forecast$seriesFor
tokyo_century_volatility_f <- tokyo_century_f@forecast$sigmaFor
plot(tokyo_century_return_f, type='l') 
plot(tokyo_century_volatility_f, type='l') 


### tokio_marine ###
# import data #
tokio_marine <- read_excel("Desktop/ESG/day-to-day-return/tokio_marine.xlsx")
View(tokio_marine)
plot(tokio_marine$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ tokio_marine$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
tokio_marine_r= tokio_marine$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(tokio_marine_r, seasonal=FALSE) # auto arima(1,0,3)
fit
tokio_marine_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,3)), distribution.model='std')
tokio_marine_garch <- ugarchfit(spec= tokio_marine_spec, data= tokio_marine_r)
# forecast #
tokio_marine_f <- ugarchforecast(tokio_marine_garch, n.ahead=40)
tokio_marine_return_f <- tokio_marine_f@forecast$seriesFor
tokio_marine_volatility_f <- tokio_marine_f@forecast$sigmaFor
plot(tokio_marine_return_f, type='l') 
plot(tokio_marine_volatility_f, type='l') 


### takeda ###
# import data #
takeda <- read_excel("Desktop/ESG/day-to-day-return/takeda.xlsx")
View(takeda)
plot(takeda$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ takeda$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
takeda_r= takeda$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(takeda_r, seasonal=FALSE) # auto arima(1,0,3)
fit
takeda_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,3)), distribution.model='std')
takeda_garch <- ugarchfit(spec= takeda_spec, data= takeda_r)
# forecast #
takeda_f <- ugarchforecast(takeda_garch, n.ahead=40)
takeda_return_f <- takeda_f@forecast$seriesFor
takeda_volatility_f <- takeda_f@forecast$sigmaFor
plot(takeda_return_f, type='l') 
plot(takeda_volatility_f, type='l') 


### sony ###
# import data #
sony <- read_excel("Desktop/ESG/day-to-day-return/sony.xlsx")
View(sony)
plot(sony$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ sony$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
sony_r= sony$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(sony_r, seasonal=FALSE) # auto arima(1,0,0)
fit
sony_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model='std')
sony_garch <- ugarchfit(spec= sony_spec, data= sony_r)
# forecast #
sony_f <- ugarchforecast(sony_garch, n.ahead=40)
sony_return_f <- sony_f@forecast$seriesFor
sony_volatility_f <- sony_f@forecast$sigmaFor
plot(sony_return_f, type='l') 
plot(sony_volatility_f, type='l') 



### shionogi ###
# import data #
shionogi <- read_excel("Desktop/ESG/day-to-day-return/shionogi.xlsx")
View(shionogi)
plot(shionogi $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ shionogi $Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
shionogi_r= shionogi$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(shionogi_r, seasonal=FALSE) # auto arima(0,0,2)
fit
shionogi_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,2)), distribution.model='std')
shionogi_garch <- ugarchfit(spec= shionogi_spec, data= shionogi_r)
# forecast #
shionogi_f <- ugarchforecast(shionogi_garch, n.ahead=40)
shionogi_return_f <- shionogi_f@forecast$seriesFor
shionogi_volatility_f <- shionogi_f@forecast$sigmaFor
plot(shionogi_return_f, type='l') 
plot(shionogi_volatility_f, type='l') 


### ricoh ###
# import data #
ricoh <- read_excel("Desktop/ESG/day-to-day-return/ricoh.xlsx")
View(ricoh)
plot(ricoh$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ ricoh$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
ricoh_r= ricoh$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(ricoh_r, seasonal=FALSE) # auto arima(0,0,0)
fit
ricoh_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model='std')
ricoh_garch <- ugarchfit(spec= ricoh_spec, data= ricoh_r)
# forecast #
ricoh_f <- ugarchforecast(ricoh_garch, n.ahead=40)
ricoh_return_f <- ricoh_f@forecast$seriesFor
ricoh_volatility_f <- ricoh_f@forecast$sigmaFor
plot(ricoh_return_f, type='l') 
plot(ricoh_volatility_f, type='l') 

### olympus ###
# import data #
olympus <- read_excel("Desktop/ESG/day-to-day-return/olympus.xlsx")
View(olympus)
plot(olympus$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ olympus$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
olympus_r= olympus$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(olympus_r, seasonal=FALSE) # auto arima(3,0,0)
fit
olympus_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(3,0)), distribution.model='std')
olympus_garch <- ugarchfit(spec= olympus_spec, data= olympus_r)
# forecast #
olympus_f <- ugarchforecast(olympus_garch, n.ahead=40)
olympus_return_f <- olympus_f@forecast$seriesFor
olympus_volatility_f <- olympus_f@forecast$sigmaFor
plot(olympus_return_f, type='l') 
plot(olympus_volatility_f, type='l') 


### nippon_tele ###
# import data #
nippon_tele <- read_excel("Desktop/ESG/day-to-day-return/nippon_tele.xlsx")
View(nippon_tele)
plot(nippon_tele$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ nippon_tele$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
nippon_tele_r= nippon_tele$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(nippon_tele_r, seasonal=FALSE) # auto arima(1,0,2)
fit
nippon_tele_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)), distribution.model='std')
nippon_tele_garch <- ugarchfit(spec= nippon_tele_spec, data= nippon_tele_r)
# forecast #
nippon_tele_f <- ugarchforecast(nippon_tele_garch, n.ahead=40)
nippon_tele_return_f <- nippon_tele_f@forecast$seriesFor
nippon_tele_volatility_f <- nippon_tele_f@forecast$sigmaFor
plot(nippon_tele_return_f, type='l') 
plot(nippon_tele_volatility_f, type='l')


### nec ###
# import data #
nec <- read_excel("Desktop/ESG/day-to-day-return/nec.xlsx")
View(nec)
plot(nec$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ nec$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
nec_r= nec $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(nec_r, seasonal=FALSE) # auto arima(0,0,0)
fit
nec_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model='std')
nec_garch <- ugarchfit(spec= nec_spec, data= nec_r)
# forecast #
nec_f <- ugarchforecast(nec_garch, n.ahead=40)
nec_return_f <- nec_f@forecast$seriesFor
nec_volatility_f <- nec_f@forecast$sigmaFor
plot(nec_return_f, type='l') 
plot(nec_volatility_f, type='l') 


### murata ###
# import data #
murata <- read_excel("Desktop/ESG/day-to-day-return/murata.xlsx")
View(murata)
plot(murata$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ murata$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
murata_r= murata $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(murata_r, seasonal=FALSE) # auto arima(1,0,2)
fit
murata_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)), distribution.model='std')
murata_garch <- ugarchfit(spec= murata_spec, data= murata_r)
# forecast #
murata_f <- ugarchforecast(murata_garch, n.ahead=40)
murata_return_f <- murata_f@forecast$seriesFor
murata_volatility_f <- murata_f@forecast$sigmaFor
plot(murata_return_f, type='l') 
plot(murata_volatility_f, type='l') 


### msad ###
# import data #
msad <- read_excel("Desktop/ESG/day-to-day-return/msad.xlsx")
View(msad)
plot(msad$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ msad$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
msad_r= msad $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(msad_r, seasonal=FALSE) # auto arima(1,0,1)
fit
msad_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model='std')
msad_garch <- ugarchfit(spec= msad_spec, data= msad_r)
# forecast #
msad_f <- ugarchforecast(msad_garch, n.ahead=40)
msad_return_f <- msad_f@forecast$seriesFor
msad_volatility_f <- msad_f@forecast$sigmaFor
plot(msad_return_f, type='l') 
plot(msad_volatility_f, type='l') 


### mitsubishi ###
# import data #
mitsubishi <- read_excel("Desktop/ESG/day-to-day-return/mitsubishi.xlsx")
View(mitsubishi)
plot(mitsubishi$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ mitsubishi$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
mitsubishi_r= mitsubishi $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(mitsubishi_r, seasonal=FALSE) # auto arima(0,0,0)
fit
mitsubishi_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model='std')
mitsubishi_garch <- ugarchfit(spec= mitsubishi_spec, data= mitsubishi_r)
# forecast #
mitsubishi_f <- ugarchforecast(mitsubishi_garch, n.ahead=40)
mitsubishi_return_f <- mitsubishi_f@forecast$seriesFor
mitsubishi_volatility_f <- mitsubishi_f@forecast$sigmaFor
plot(mitsubishi_return_f, type='l') 
plot(mitsubishi_volatility_f, type='l') 


### kurita_water ###
# import data #
kurita_water <- read_excel("Desktop/ESG/day-to-day-return/kurita_water.xlsx")
View(kurita_water)
plot(kurita_water$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ kurita_water$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
kurita_water_r= kurita_water $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(kurita_water_r, seasonal=FALSE) # auto arima(4,0,4)
fit
kurita_water_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(4,4)), distribution.model='std')
kurita_water_garch <- ugarchfit(spec= kurita_water_spec, data= kurita_water_r)
# forecast #
kurita_water_f <- ugarchforecast(kurita_water_garch, n.ahead=40)
kurita_water_return_f <- kurita_water_f@forecast$seriesFor
kurita_water_volatility_f <- kurita_water_f@forecast$sigmaFor
plot(kurita_water_return_f, type='l') 
plot(kurita_water_volatility_f, type='l')


### japan_avi ###
# import data #
japan_avi <- read_excel("Desktop/ESG/day-to-day-return/japan_avi.xlsx")
View(japan_avi)
plot(japan_avi$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ japan_avi$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
japan_avi_r= japan_avi $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(japan_avi_r, seasonal=FALSE) # auto arima(0,0,3)
fit
japan_avi_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,3)), distribution.model='std')
japan_avi_garch <- ugarchfit(spec= japan_avi_spec, data= japan_avi_r)
# forecast #
japan_avi_f <- ugarchforecast(japan_avi_garch, n.ahead=40)
japan_avi_return_f <- japan_avi_f@forecast$seriesFor
japan_avi_volatility_f <- japan_avi_f@forecast$sigmaFor
plot(japan_avi_return_f, type='l') 
plot(japan_avi_volatility_f, type='l') 


### ibiden ###
# import data #
ibiden <- read_excel("Desktop/ESG/day-to-day-return/ibiden.xlsx")
View(ibiden)
plot(ibiden$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ ibiden$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
ibiden_r= ibiden $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(ibiden_r, seasonal=FALSE) # auto arima(2,0,1)
fit
ibiden_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,0,1)), distribution.model='std')
ibiden_garch <- ugarchfit(spec= ibiden_spec, data= ibiden_r)
# forecast #
ibiden_f <- ugarchforecast(ibiden_garch, n.ahead=40)
ibiden_return_f <- ibiden_f@forecast$seriesFor
ibiden_volatility_f <- ibiden_f@forecast$sigmaFor
plot(ibiden_return_f, type='l') 
plot(ibiden_volatility_f, type='l') 


### hitachi ###
# import data #
hitachi <- read_excel("Desktop/ESG/day-to-day-return/hitachi.xlsx")
View(hitachi)
plot(hitachi$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ hitachi$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
hitachi_r= hitachi $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(hitachi_r, seasonal=FALSE) # auto arima(0,0,0)
fit
hitachi_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model='std')
hitachi_garch <- ugarchfit(spec= hitachi_spec, data= hitachi_r)
# forecast #
hitachi_f <- ugarchforecast(hitachi_garch, n.ahead=40)
hitachi_return_f <- hitachi_f@forecast$seriesFor
hitachi_volatility_f <- hitachi_f@forecast$sigmaFor
plot(hitachi_return_f, type='l') 
plot(hitachi_volatility_f, type='l') 


### fujitsu ###
# import data #
fujitsu <- read_excel("Desktop/ESG/day-to-day-return/fujitsu.xlsx")
View(fujitsu)
plot(fujitsu$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ fujitsu$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
fujitsu_r= fujitsu $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(fujitsu_r, seasonal=FALSE) # auto arima(0,0,0)
fit
fujitsu_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model='std')
fujitsu_garch <- ugarchfit(spec= fujitsu_spec, data= fujitsu_r)
# forecast #
fujitsu_f <- ugarchforecast(fujitsu_garch, n.ahead=40)
fujitsu_return_f <- fujitsu_f@forecast$seriesFor
fujitsu_volatility_f <- fujitsu_f@forecast$sigmaFor
plot(fujitsu_return_f, type='l') 
plot(fujitsu_volatility_f, type='l') 


### fuji ###
# import data #
fuji <- read_excel("Desktop/ESG/day-to-day-return/fuji.xlsx")
View(fuji)
plot(fuji$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ fuji$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
fuji_r= fuji $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(fuji_r, seasonal=FALSE) # auto arima()
fit
fuji_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model='std')
fuji_garch <- ugarchfit(spec= fuji_spec, data= fuji_r)
# forecast #
fuji_f <- ugarchforecast(fuji_garch, n.ahead=40)
fuji_return_f <- fuji_f@forecast$seriesFor
fuji_volatility_f <- fuji_f@forecast$sigmaFor
plot(fuji_return_f, type='l') 
plot(fuji_volatility_f, type='l') 


### eizo ###
# import data #
eizo <- read_excel("Desktop/ESG/day-to-day-return/eizo.xlsx")
View(eizo)
plot(eizo$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ eizo$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
eizo_r= eizo $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(eizo_r, seasonal=FALSE) # auto arima(1,0,2)
fit
eizo_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)), distribution.model='std')
eizo_garch <- ugarchfit(spec= eizo_spec, data= eizo_r)
# forecast #
eizo_f <- ugarchforecast(eizo_garch, n.ahead=40)
eizo_return_f <- eizo_f@forecast$seriesFor
eizo_volatility_f <- eizo_f@forecast$sigmaFor
plot(eizo_return_f, type='l') 
plot(eizo_volatility_f, type='l') 


### astellas ###
# import data #
astellas <- read_excel("Desktop/ESG/day-to-day-return/astellas.xlsx")
View(astellas)
plot(astellas$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~ astellas$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
astellas_r= astellas $DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(astellas_r, seasonal=FALSE) # auto arima(1,0,1)
fit
astellas_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model='std')
astellas_garch <- ugarchfit(spec= astellas_spec, data= astellas_r)
# forecast #
astellas_f <- ugarchforecast(astellas_garch, n.ahead=40)
astellas_return_f <- astellas_f@forecast$seriesFor
astellas_volatility_f <- astellas_f@forecast$sigmaFor
plot(astellas_return_f, type='l') 
plot(astellas_volatility_f, type='l') 



