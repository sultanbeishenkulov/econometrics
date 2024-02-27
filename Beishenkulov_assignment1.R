# Preamble (sets working directory, clears memory, resets) -------------------------------------
rm(list=ls())
if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd("/Users/sultanbeishenkulov/4sem/econometrics")#set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y());options(scipen=0);dev.off();rm(list=ls())

# Packages -------------------------------------------------------------------------------------

library("readxl")
library("quantmod")
library("tseries")
library("PerformanceAnalytics")
library("ggplot2")
library("dplyr")
library("tsibble") 
library("feasts") # feature extraction and statistics
library("fable") # time series modeling and forecasting
library("forecast")
library("lubridate") # date, time and frequency
library("gridExtra")
library(Quandl)
library("urca")


#1) data
#connecting to quandl.com
Quandl.api_key("tGWEq2foJsz8pqdn8pxp")
?Quandl
#getting the LBMA_GOLD dataset from quandl dataset with weekly freq and "xts" type
LBMA_GOLD <- Quandl("LBMA/GOLD", 
                    type="xts", 
                    collapse = c("weekly"))  
                    
head(LBMA_GOLD)

table(is.na(LBMA_GOLD$`USD (AM`))
table(is.na(LBMA_GOLD$`USD (PM`))
#AM price has fewer missing values, so i gonna use it for time series

usd <- LBMA_GOLD$`USD (AM`
head(usd)

#creating dataset only with date and usd price
LBMA_GOLD <- cbind(usd)
colnames(LBMA_GOLD) <- c("price")
head(LBMA_GOLD)


#2) creating dataframe with variables: day, price and log-return
#n <- length(usd) #total amount

#creating the dataset
data <- data.frame(
  day = rownames(data.frame(LBMA_GOLD)),
  price = usd,
  #return = 100 * log(usd[-1] / c(NA, usd[-n]))  
  return = 100*diff(log(usd), lag = 1))  


colnames(data) <- c("day", "price", "log-return") #renaming the columns
head(data)
#convinience of multiplication by 100 is getting the percentage 
#it is much easier to work with percentagies because you 
#can understand the data of return just by looking at it

#3)
#price plot
plot(ts(data$price, start = c(1968, 1), frequency = 52), 
     main = "Gold price",xlab="Time", ylab = "Price") 

#return plot
plot(ts(data$`log-return`, start = c(1968, 1), frequency = 52), 
     main = "Gold return",xlab="Time", ylab = "Returns") 

#normality test for the price
skewness(data$price) #1.136999 > 0.5 meaning that price is moderately skewed
kurtosis(data$price) #0.1186234 < 3 (can be assumed that 
#it is normal but does not look like that)
jarque.bera.test(data$price) #Reject Ho (Non-Normal Distribution)

#normality test for the log return
skewness(data$`log-return`) #0.5471292, can say that it is more or less symmetrical
kurtosis(data$`log-return`) #11.93605>3, fat tails
jarque.bera.test(na.remove(data$`log-return`)) #p-value < 2.2e-16(very close to 0)  
#Reject Ho (Non-Normal Distribution)
#histogram of price
?hist
hist(data$price,
     main = paste("histogram of price"),
     xlab = paste("price"),
     ylab = paste("frequency"),
     col = "blue",
     breaks = 200)

#histogram of return
hist(data$`log-return`,
     main = paste("histogram of price"),
     xlab = paste("log return"),
     ylab = paste("frequency"),
     col = "darkmagenta",
     breaks = 200,  #to make it more visual
     xlim = c(-10, 10))  #to get rid of outliers       

#4) mins and maxs
 
data[which.min(data$price), ]
#lowest price  is  34.77 at 1970-01-18

data[which.max(data$price), ]
#highest price is 2061.5 at 2020-08-09

data[which.min(data$`log-return`), ]
#lowest log return is -18.05796 at 1980-03-16

data[which.max(data$`log-return`), ]
#highest log return is 30.25303 at 1980-01-20

#first of all I think in terms of seasonality it is better to use
#bigger interval of time because we gonna have more significant fluctuation
# in terms of this assignment, it is not that good because we do not 
#specific day but the week period of time


#5converting dataset
data$month <- as.yearmon(data$day) #adding the month data to dataset
?slice

#creating the monthly dataset
data_monthly <- data %>%
  group_by(month) %>%
  arrange(day) %>%
  slice(1) %>%
  ungroup

#creating modern tsibble object
data_monthly$month <- yearmonth(data_monthly$month)
data_monthly <- as_tsibble(na.omit(data_monthly), index = month)
 
#6 checking for autocorrelation
?acf
acf(data_monthly$`log-return`, plot = TRUE ) #drops out after the first lag
pacf(data_monthly$`log-return`, plot = TRUE )


#ACF and PACF both are tailing off, it 
#indicates that there is an underlying ARMA(p,q) model.
#according to acf there is no significant AR because it drops after the first lag

#7 checking existence of a trend
trend <- na.omit(data_monthly$`log-return`) %>%
  ur.df(selectlags = "BIC", type ="trend") %>%
  summary()
#p value is less than 0.05 meaning that we reject the null hypothesis (h0 is unit root exists)
# -> unit root does not exists meaning that our series is stationary


#8 choosing the model
log_return <- na.omit(data_monthly$`log-return`)
arma <- data_monthly %>% # write down some possible combinations of AR(p) and MA(q) orders
  model(
    arma00 = ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(log_return ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(log_return ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(log_return ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma40 = ARIMA(log_return ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(log_return ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(log_return ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(log_return ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(log_return ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma41 = ARIMA(log_return ~ 1 + pdq(4, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(log_return ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(log_return ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(log_return ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(log_return ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma42 = ARIMA(log_return ~ 1 + pdq(4, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(log_return ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(log_return ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(log_return ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0)),
    arma43 = ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0)),
    arma04 = ARIMA(log_return ~ 1 + pdq(0, 0, 4) + PDQ(0, 0, 0)),
    arma14 = ARIMA(log_return ~ 1 + pdq(1, 0, 4) + PDQ(0, 0, 0)),
    arma24 = ARIMA(log_return ~ 1 + pdq(2, 0, 4) + PDQ(0, 0, 0)),
    arma34 = ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma44 = ARIMA(log_return ~ 1 + pdq(4, 0, 4) + PDQ(0, 0, 0))
  )
?glance
glance(arma)
glance(arma)[which.min(glance(arma)[["AIC"]]), ] # arma00
glance(arma)[which.min(glance(arma)[["BIC"]]), ] # arma00
#' arma00 minimizes the IC.
#' #d = 0 because we no not need to perform any integration part (stationarity)


#9 res.autocorrelation
arma %>%
  select(arma00) %>%
  report() #  summary() 

a <- arma %>%
  residuals() %>% # get the residuals from arma00
  group_by(.model) %>% # groups the residuals by model
  features(features = ljung_box, lag = 20) # Ljung-Box test for autocorrelation
a[1,]
#p-value = 0.284 which is bigger than 0.05 -> FTR => no autocorrelation 
#=> stick with this model

#10 prediction for log return for the period of May 2020- May 2021

data_pred <- data_monthly %>% 
  filter(month >= yearquarter("2020-05-01")) %>% # beginning of TS
  filter(month<= yearquarter("2021-05-01")) # end of TS
?filter
# Forecast with ARMA(0,0) Model for a year ahead
forecast <- data_pred %>%
  model(ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0))) %>%
  forecast(h = 4, level = 95) # future prediction of a TS from the fitted model

accuracy(forecast, data_monthly) # evaluation of the forecast model wth of descriptive measures
