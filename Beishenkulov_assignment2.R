# Preamble (sets working directory, clears memory, resets) -------------------------------------
rm(list=ls())
if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd("/Users/sultanbeishenkulov/4sem/econometrics")#set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y());options(scipen=0);dev.off();rm(list=ls())

# Packages -------------------------------------------------------------------------------------

library("readxl")
library("magrittr") 
library("tidyverse")
library("ggplot2")
library("tsibble")
library("fable")
library("fabletools")
library("feasts") 
library("lubridate")
library("dplyr")
library("moments") 
library("quantmod")
library("rugarch")
library("urca")
library("feasts") 
library("forecast")
library("PerformanceAnalytics")
library("GAS")
install.packages('cvar')
UTblue <- rgb(5/255, 110/255, 167/255, 1)

#Volatility modelling
#1.loading EUROSTOXX50 as modern ts and plotting it
data <- read_excel("Part2.xlsx")
data <- data %>%
  mutate(return = difference(log(data$RGDP), 1) * 100) %>% # create a log-return variable
  tail(-1) %>%
  arrange(DATE) %>%
  mutate(tradingday = row_number(DATE)) %>%
  as_tsibble(index = tradingday)
data

#Plots 
plot1 <- data %>%
   autoplot(vars(RGDP)) +
  geom_line(col = "#db0e0e") +
  xlab("Trading Day") + ylab("Index Units") +
  ggtitle("RGDP") +
  theme(plot.title = element_text(hjust = 0.5))
plot1

plot2 <- data %>%
  ggplot(aes(DATE, return)) +
  geom_line(aes(col = Symbol), col = UTblue, size = 1) +
  theme_bw() +
  xlab("Date") +
  ylab("Return") +
  theme(legend.position = "none")
plot2

#2.Looking for adequate ARMA-GARCH model with a help of AIC and autocorrelation
acf(data$return, plot = TRUE )
pacf(data$return, plot = TRUE )
#Geometric decay at each 4th lag (4, 8, 12..) (ACF)
#Significant at 4th lag (PACF)


#going through all pairs for the interval of {0,1,2,3,4,5} for identifying the optimal choice 
#wrt AIC (normal residuals)
for (p in 1:5) 
{ 
  for (q in 1:5)  
  { 
    spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p,q), submodel = "GARCH"),  
                      mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = "norm") 
    fit <- ugarchfit(spec = spec, data = data$return) 
    AIC <- infocriteria(fit)[1] 
    
    print(paste0("(p = ", p, ", q = ", q, ")", " AIC =  ", AIC )) 
  } 
} 
?ugarchspec
# ARMA(5,4) is the best wrt AIC (smallest value at AIC)

#t-distr residuals
for (p in 1:5) 
{ 
  for (q in 1:5)  
  { 
    spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p,q), submodel = "GARCH"),  
                      mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = "sstd") 
    fit <- ugarchfit(spec = spec, data = data$return) 
    AIC <- infocriteria(fit)[1] 
    
    print(paste0("(p = ", p, ", q = ", q, ")", " AIC =  ", AIC )) 
  } 
} 
# ARMA(5,4) is the second best wrt AIC (smallest value at AIC)

#Trying other options of choosing the lags:
 
arma_fit_AIC <- autoarfima(data = data$return, 
                        ar.max = 5, ma.max = 5, include.mean = NULL, 
                        criterion = c("AIC"),
                        method = "partial")
arma_fit_AIC
#ARMA(3, 3) has the lowest AIC according to this package

auto.arima(data$return, ic = "aic", trace=TRUE, max.p = 5, max.q = 5)
#This function suggests ARMA(4,3)

#In conclusion I choose ARMA(4,3)-GARCH(1,1) 

specGARCH54 <- ugarchspec(                            
  mean.model         = list(armaOrder = c(4, 3)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm")

GARCH11 <- ugarchfit(specGARCH54, na.omit(data$return))
GARCH11

#Constant term in the AR is positive and significant 
#Variance part shows a positive constant term,
#All variance terms are positive and significant,

residGARCH11 <- resid(GARCH11@fit)
residGARCH11


#Conditional Variance
condvarGARCH11 <- c(GARCH11@fit$var)

#Standardize Residuals by Conditional Standard Deviation
residstdGARCH11 <- residGARCH11/I(sqrt(condvarGARCH11))

#Check the form of the distribution
kurtosis(residstdGARCH11)
#Kurtosis = 14.90615 > 3 => excess kurtosis
skewness(residstdGARCH11)
#Skewness = 1.562539
#data is highly skewed (>1)

#Ljung-Box test for autocorrelation in standardized residuals
Box.test(residstdGARCH11, lag = 5, type = "Ljung-Box") 
#p-value = is very small,  reject the H0, autocorrelation exists


#Ljung-Box test for autocorrelation in squared standardized residuals
Box.test(residstdGARCH11^2, lag = 5, type = "Ljung-Box") 
#p-value = 0.9897, Reject the H0
#With 5 lags there is no autocorrelation in the squared residuals,


#Estimate a EGARCH(1,1) model
specEGARCH11 <- ugarchspec(
  mean.model = list(armaOrder = c(4, 3)),
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm"
)

EGARCH11 <- ugarchfit(specEGARCH11, na.omit(data$return))
EGARCH11

#there's no guarantee that a model will converge, so it is the case



#Estimate a GJR-GARCH(1,1) model
specGJRGARCH11 <- ugarchspec(
  mean.model = list(armaOrder = c(4, 3)),
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm"
)

GJRGARCH11 <- ugarchfit(specGJRGARCH11, na.omit(data$return))
GJRGARCH11
#Constant term in the AR is positive and insignificant
#Constant term in a variance part is positive and significant


#Find standardized residuals for a heteroscedasticity test
#Repeat the procedure of finding the standardized residuals
residGJRGARCH11 <- resid(GJRGARCH11@fit)
condvarGJRGARCH11 <- c(GJRGARCH11@fit$var)
residstdGJRGARCH11 <- residGJRGARCH11/I(sqrt(condvarGJRGARCH11))

#Check the form of the distribution
kurtosis(residstdGJRGARCH11)
#Kurtosis = 16.56883 > 3 => excess kurtosis
skewness(residstdGJRGARCH11)
#Skewness = 1.351201
#again highly skewed

#Ljung-Box test 
Box.test(residstdGJRGARCH11, lag = 5, type = "Ljung-Box")
#p-value = 0.02096, reject the H0
#autocorrelation exists

#Test for conditional heteroscedasticity
Box.test(residstdGJRGARCH11^2, lag = 10, type = "Ljung-Box") 
#p-value = 0.9993, Failed to reject the H0
#p-value is almost 1 which is a confident result

#LB test shows that GJR-GARCH is the best model for this stochastic process
#However the overall significance of variance terms in EGARCH is better

#Leverage effect occurs only in the EGARCH
GJRGARCH11@fit$matcoef[9,1] #alpha = 0.7752136
EGARCH11@fit$matcoef[9,1] #alpha = NULL
#So in case of the negative shock EGARCH shows the bigger value


#Downside risk modeling
plot(ecdf(data$return), xlab = "", ylab = "")


#Var ES at 99% level
?cvar
cvar::VaR(data$return, x = 0.01, dist.type = "")
#VaR = 6.685496


cvar::ES(data$return, x = 0.01, dist.type = "norm")
#ES = 5.891127

VaR01 <- rep(NA, dim(data)[1])


for(i in 191:(dim(data)[1])){ # construct quantiles
VaR01[i] <- quantile(data[((i-250):(i-1)), "return"], c(0.01))
}
spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model=list(armaOrder=c(4,3), include.mean = TRUE), 
                    distribution="norm") 
roll1 <- ugarchroll(spec1, data[,"return"], n.start=191, refit.window = "moving", 
                    VaR.alpha = c(0.01)) #' VaR tail level to calculate
VaR11 <- c(rep(NA, 191),roll1@forecast$VaR[,1])

VaR99 <- ts(VaR11[-(1:1000),], 
              start = c(1970, 04, 01), # correspond to roughly 5 years
              frequency = 252) # typical daily frequency


BT99 <- matrix(NA,5,3)
for(i in 4:8){ # construction of an informative structured summary
  
  
#Backtest  
  BT <- BacktestVaR(VaR11[,3],VaR99[,i],0.01)
  BT99.5Tab[(i-3),1] <- BT$AE
  BT99.5Tab[(i-3),2] <- BT$LRuc[2] # Unconditional Coverage
  BT99.5Tab[(i-3),3] <- BT$LRcc[2] # Conditional Coverage
  
  