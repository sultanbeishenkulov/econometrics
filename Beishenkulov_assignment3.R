# Preamble (sets working directory, clears memory, resets) -------------------------------------
rm(list=ls())
if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd("/Users/sultanbeishenkulov/4sem/econometrics")#set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y());options(scipen=0);dev.off()
# Packages -------------------------------------------------------------------------------------
library("readxl") 
library("dplyr") 
library("tsibble") 
library("lubridate") 
library("ggplot2") 
library("reshape2") # melt
#install.packages("systemfit")
library("systemfit")
library("fable") #' modern time series modeling but we also work with [{vars}] as
#' fable does not yet have a straightforward IRFs support  
library("vars")
library("priceR")
library("tibbletime")
library("rmgarch")
library("rugarch")
library("parallel")
UTblue <- rgb(5/255, 110/255, 167/255, 1)

#1. (7 points) Multivariate (Macro-)Modeling
#1.1 loading data and calculating the inflation rate
DATA <- read_excel("Assignment3.xlsx")
DATA$year = lubridate::year(DATA$date)  #we have the monthly data, we need a yearly inflation
DATA<- DATA %>%
  group_by(year) %>%
  summarize(CPI = mean(CPI), FEDFUNDS =  mean(FEDFUNDS), UNRATE = mean(UNRATE)) %>%
  as.data.frame
#taking the mean of 12 months in order to have a yearly data
DATA$INFLATION <- c(NA, diff(DATA$CPI, lag=1)) / lag(DATA$CPI, k =1)*100 #inflation formula 
DATA <- na.omit(DATA) 
DATA <- subset(DATA, select = -c(CPI) ) #getting rid of CPI

#1.2 declaring variables as plain ts (as in the notes) and plotting them:

DATA <- as_tsibble(DATA, index = year)
DATA <- DATA%>%
  mutate(
    INFLATION = ts(INFLATION, start = 1961, frequency = 12),
    UNRATE = ts(UNRATE, start = 1961, frequency = 12),
    FEDFUNDS =  ts(FEDFUNDS, start = 1961, frequency = 12))
#from the very beginning

#building the plot
plot1 <- DATA %>%
  melt(id = "year") %>% 
  ggplot() +
  aes(x = year, y = value, colour = variable) +
  geom_line() +
  labs(x = "", y = "%", color = "Instances: ") +
  scale_color_manual(labels = c("FEDFUNDS", "UNRATE", "INFLATION"),
                     values = c("black", UTblue, "red"))
plot1


#1.3 Selection and estimation of 3 best VAR models
VAR <- as.data.frame(DATA[, c("INFLATION", "UNRATE", "FEDFUNDS")]) # re-order if needed
# VAR is created from data and is now a data frame object
VARselect(VAR, lag.max = 6, type = c("const")) #best fit is var(1)

## Estimate VAR(1) Model 
fit <- vars::VAR(VAR, p = 1, type = "const") 
fit
glance(fit) 

#1.4 autocorrelation test (12 lags, 3 lags), lag length selection

# 12 lags
Box.test(fit$varresult$FEDFUNDS$residuals, lag = 12, type = "Ljung-Box")
#p-value = 0.02004 < 0.05, autocorrelation exists

Box.test(fit$varresult$UNRATE$residuals, lag = 12, type = "Ljung-Box")
#p-value = 0.2844 > 0.05, no autocorrelation

Box.test(fit$varresult$INFLATION$residuals, lag = 12, type = "Ljung-Box")
#p-value = 0.01405 < 0.05, autocorrelation exists

# 3 lags
Box.test(fit$varresult$FEDFUNDS$residuals, lag = 3, type = "Ljung-Box")
#p-value = 0.003028 < 0.05, autocorrelation exists

Box.test(fit$varresult$UNRATE$residuals, lag = 3, type = "Ljung-Box")
#p-value = 0.01967 < 0.05, autocorrelation exists

Box.test(fit$varresult$INFLATION$residuals, lag = 3, type = "Ljung-Box")
#p-value = 0.01859 < 0.05, autocorrelation exists

#1.5 increasing the VAR(p) order, any changes?
# p = 3:
fit1 <- vars::VAR(VAR, p = 3, type = "const") 
fit1

# 12 lags
Box.test(fit1$varresult$FEDFUNDS$residuals, lag = 12, type = "Ljung-Box")
#p-value = 0.03702 < 0.05, autocorrelation exists

Box.test(fit1$varresult$UNRATE$residuals, lag = 12, type = "Ljung-Box")
#p-value = 0.7916 > 0.05, no autocorrelation 
Box.test(fit1$varresult$INFLATION$residuals, lag = 12, type = "Ljung-Box")
#p-value = 0.7055 > 0.05, no autocorrelation 

# 3 lags
Box.test(fit1$varresult$FEDFUNDS$residuals, lag = 3, type = "Ljung-Box")
#p-value = 0.7709 > 0.05, no autocorrelation 

Box.test(fit1$varresult$UNRATE$residuals, lag = 3, type = "Ljung-Box")
#p-value = 0.9584, > 0.05, no autocorrelation 

Box.test(fit1$varresult$INFLATION$residuals, lag = 3, type = "Ljung-Box")
#p-value = 0.308 > 0.05, no autocorrelation 

#at 12 lags we see the autocorellation but at 3 lags there is totally no autocorrelation.

#1.6 Impulse response functions. Explain choice of Cholesky

#' Now inference can be made
irf_INFLATION <- irf(fit1, impulse = "INFLATION", n.ahead = 48, runs = 500)  #' an impulse is 1 s.d. 
                                                                            #' in the error term (size)
irf_UNRATE <- irf(fit1, impulse = "UNRATE", n.ahead = 48, runs = 500)
irf_FEDFUNDS <- irf(fit1, impulse = "FEDFUNDS", n.ahead = 48, runs = 500)


plot(irf_INFLATION, xlab = "", main = "Impulse: INFLATION",
     col = UTblue, lwd = 2.5)
plot(irf_UNRATE, xlab = "", main = "Impulse: UNRATE",
     col = UTblue, lwd = 2.5)
plot(irf_FEDFUNDS, xlab = "", main = "Impulse: FEDFUNDS",
     col = UTblue, lwd = 2.5)

# Ordering by decreasing exogeneity(we can see the dependencies on plots especially) we have: 
#inflation rate -> unemployment rate -> fed funds

#1.7 Interpretation of results with regard to significance, persistence and stationarity

#inflation rate:
#having stationarity as they fade, significant till fourth period, peak is around 1.5

#unemployment rate:
#having stationarity as they fade, significant till fourth period, peak is around 0.9


#fedfunds
#having stationarity as they fade, significant till fourth period, peak is around 1.05


--------------------------------------------------------------------------------------------
  
  
# 2. (3 points) Multivariate GARCH Modeling
  
#2.1
help(priceR)
?historical_exchange_rates
EURUSD <- historical_exchange_rates("EUR", to = "USD", start_date = "2010-01-01", end_date = "2021-07-10")
GBPUSD <- historical_exchange_rates("GBP", to = "USD", start_date = "2010-01-01", end_date = "2021-07-10")

#creating dataframe
FOREX <- cbind.data.frame(EURUSD, GBPUSD)
FOREX <- FOREX[,c(1,2,4)]
colnames(FOREX) <- c('date','EURUSD','GBPSD')

#ts
FOREX <- as_tbl_time(FOREX, date)
FOREX <- as_period(FOREX, period = "monthly")
FOREX
