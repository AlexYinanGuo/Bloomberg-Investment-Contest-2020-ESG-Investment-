STEP1 - GARCH for day-to-day return
================
Keio University Team

``` r
library(quantmod)
library(rugarch)
library(forecast)
library(readxl)
library(stats)
knitr::opts_chunk$set(fig.width=3, fig.height=3) 
```

# Yokogawa Electric Corp (6841)

### 1\. import data

``` r
yokogawa <- read_excel("ESG/day-to-day-return/yokogawa.xlsx")
par(mfrow=c(1,1)) 
plot(yokogawa$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS~yokogawa$Date, yabl='DAY_TO_DAY_TOT_RETURN_GROSS_DVDS', type='l')
```

![](STEP1_GARCH_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### 2\. build GARCH model

``` r
yokogawa_r=yokogawa$DAY_TO_DAY_TOT_RETURN_GROSS_DVDS
fit = auto.arima(yokogawa_r, seasonal=FALSE) # auto arima(0,0,0)
fit
```

    ## Series: yokogawa_r 
    ## ARIMA(0,0,0) with zero mean 
    ## 
    ## sigma^2 estimated as 6.632:  log likelihood=-7801.88
    ## AIC=15605.76   AICc=15605.76   BIC=15611.86

``` r
yokogawa_spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model='std')
yokogawa_garch <- ugarchfit(spec=yokogawa_spec, data=yokogawa_r)
```

### 3\. forecast

``` r
yokogawa_f <- ugarchforecast(yokogawa_garch, n.ahead=40)
yokogawa_return_f <- yokogawa_f@forecast$seriesFor
yokogawa_volatility_f <- yokogawa_f@forecast$sigmaFor
plot(yokogawa_return_f, type='l') 
```

![](STEP1_GARCH_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
plot(yokogawa_volatility_f, type='l') 
```

![](STEP1_GARCH_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->
