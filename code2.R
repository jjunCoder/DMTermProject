library(ggplot2)
library(tseries)
library(forecast)
library(dplyr)
library(zoom)

# data preprocessing
bitcoin.df <- read.csv("Bitcoin_Hourly.csv")
bitcoin.df <- na.omit(bitcoin.df)
bitcoin.df <- bitcoin.df[,-2]
bitcoin.df$Date = as.POSIXct(bitcoin.df$Date, format = "%Y-%m-%d %I-%p")
bitcoin.df <- bitcoin.df[ order(bitcoin.df$Date, decreasing = FALSE), ]
bitcoin.df <- filter(bitcoin.df, bitcoin.df$Volume.KRW > 0)

# 2019.05.29 WED
# 2019년 데이터는 1월 데이터 일부가 누락되어 있기 때문에
# 2019년 2월 부터 시작한다.

################### BUILD MODEL ###################
# data partitioning - model process data
startDate = as.POSIXct("2018-01-01");
endDate = as.POSIXct("2018-06-30");
all_dates = seq(startDate, endDate, 3600);

for (j in 1:length(all_dates)) {
  filterdate = all_dates[j];
  model.df = subset(bitcoin.df, bitcoin.df$Date %in% all_dates)
}
# apply log2 to our target value
model.df$Volume.KRW <- log2(model.df$Volume.KRW)

################### BUILD TS & ARIMA ###################
model.ts <- ts(model.df$Volume.KRW, frequency = 24)
clean.ts <- tsclean(model.ts)
# stl(model.ts, "periodic") # we can get frequency by this function
plot(model.ts , xlab = "Day")
plot(clean.ts , xlab = "Day")
# plot(stl(train.ts, s.window="periodic"))
# adf.test(diff(log(train.ts)), alternative = "stationary", k=0)

#Predicting, Validating
# arima = auto.arima(model.ts, D=1)
# arima = auto.arima(model.ts)
arima = auto.arima(clean.ts, D=1)

################### BUILD VALIDATION DATA ###################
# data partitioning - validation data
startDate = as.POSIXct("2018-07-01");
endDate = as.POSIXct("2018-07-31");
all_dates = seq(startDate, endDate, 3600);

for (j in 1:length(all_dates)) {
  filterdate = all_dates[j];
  valid.df = subset(bitcoin.df, bitcoin.df$Date %in% all_dates)
}
valid.df$Volume.KRW <- log2(valid.df$Volume.KRW)

################### TEST ###################
# forecasting
result <- forecast(arima, h=dim(valid.df)[1])
accuracy(result, valid.df$Volume.KRW)
# MAPE - mean absolute percantage error
# Training set : 1.54%
# Test set     : 3.92%

# Fitting validation set by using Arima with the model created from above data 
valid.ts = ts(valid.df$Volume.KRW, frequency = 24)
fittest = Arima(valid.ts, model = arima)

accuracy(fittest) #Gives Error of 2.62% and Mean Absolute Scaled Error (MASE = 0.70)
plot(fittest)

fcast <- forecast(fittest, h=31)
autoplot(fcast)

tsdisplay(residuals(fittest))

# Now prediction for few dates ahed needed
pred = predict(fittest, n.ahead = 1)





