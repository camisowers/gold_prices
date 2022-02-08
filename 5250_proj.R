rm(list=ls())


library(tsibble)
library(fpp3)

setwd("C:/Users/Cameron Sower/Desktop/STA 5250/5250_project")

gold <- read.csv("gold prices.csv")
gold_us <- gold[,1:2]
#head(gold_us)

x <- as.Date(gold_us$Date, format="%d-%m-%Y")
gold_us$Date <- yearmonth(x)
colnames(gold_us) <- c("Date", "Price")
head(gold_us)


## Split into Training & Test Sets
# Training: 504 months   (Jan 1979 - Dec 2020)
train <- gold_us[1:504,]
# Test: 7 months (Jan 2021 - July 20201)
test <- gold_us[505:511,]
test




## Plot the data
#gold_ts <- as_tsibble(gold_us)

gold_ts <- as_tsibble(train)
gold_ts %>% autoplot()+ labs(title="Gold Prices Jan. 1979 - July 2021")


## Explore ACF and PACF
gold_ts %>% gg_tsdisplay(Price, plot_type="partial")

gold_ts %>% features(log(Price), unitroot_kpss)              # p-value 0.01 is significant, take difference
gold_ts %>% features(difference(Price), unitroot_kpss)  # 0.10 borderline, probably don't need another

gold_ts %>% gg_tsdisplay(difference(Price), plot_type="partial", lag_max = 50)  #differenced data



## Fitting SARIMA models

# Guesses
fit <- gold_ts %>% model(
  m1 = ARIMA(Price~ pdq(0,1,1) + PDQ(0,0,1)),
  m2 = ARIMA(Price~ pdq(1,1,1) + PDQ(1,0,1)),
  auto = ARIMA(Price)
)
glance(fit)



## m1 forecast
m1 <- gold_ts %>% model(ARIMA(Price~ pdq(0,1,1) + PDQ(0,0,1)))
fcm1_5 <- m1 %>% forecast(h=60)    # next 5 years
fcm1_5 %>% autoplot(gold_us)
fcm1_10 <- m1 %>% forecast(h=120)  # next 10 years
fcm1_10 %>% autoplot(gold_ts)


# Algorithm
fit1 <- gold_ts %>% model(auto = ARIMA(Price))
report(fit1)
# outputs ARIMA(0,1,1)(2,0,0)[12] aka no seasonal differencing just one normal difference
# same as above work indicated

# forecasting
fc5 <- fit1 %>% forecast(h=60)    # next 5 years
fc5 %>% autoplot(gold_ts)
fc10 <- fit1 %>% forecast(h=120)  # next 10 years
fc10 %>% autoplot(gold_ts)

# no approximation, no stepwise
fit2 <- gold_ts %>% model(auto = ARIMA(Price, stepwise=FALSE,approximation=FALSE))
report(fit2)
# ARIMA (2,1,2) with drift, AICc = 5200
# much lower AICc than any other models




# forecasting second model
#fit2_manual <- gold_ts %>% model(ARIMA(3,1,3))
FC5 <- fit2 %>% forecast(h=60)
FC5 %>% autoplot(gold_ts)
FC10 <- fit2 %>% forecast(h=120)
FC10 %>% autoplot(gold_ts)

