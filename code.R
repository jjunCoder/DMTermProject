library(ggplot2)
library(tseries)
library(forecast)
library(dplyr)

# data preprocessing
bitcoin.df <- read.csv("Bitcoin_Hourly.csv")
bitcoin.df <- na.omit(bitcoin.df)
bitcoin.df <- bitcoin.df[,-2]
bitcoin.df$Date = as.POSIXct(bitcoin.df$Date, format = "%Y-%m-%d %I-%p")
bitcoin.df <- bitcoin.df[ order(bitcoin.df$Date, decreasing = FALSE), ]
bitcoin.df <- filter(bitcoin.df, bitcoin.df$Volume.KRW > 0)

# data partitioning - training data
startDate = as.POSIXct("2017-07-01");
endDate = as.POSIXct("2017-08-01");
all_dates = seq(startDate, endDate, 3600);

for (j in 1:length(all_dates)) {
  filterdate = all_dates[j];
  train.df = subset(bitcoin.df, bitcoin.df$Date %in% all_dates)
}

# 2019년 데이터는 1월 데이터 일부가 누락되어 있기 때문에
# 2019년 2월 부터 시작한다.
# data partitioning - validation data
startDate = as.POSIXct("2018-08-01");
endDate = as.POSIXct("2018-08-31");
all_dates = seq(startDate, endDate, 3600);

for (j in 1:length(all_dates)) {
  filterdate = all_dates[j];
  valid.df = subset(bitcoin.df, bitcoin.df$Date %in% all_dates)
}

# apply log2 to our target value
train.df$Volume.KRW <- log2(train.df$Volume.KRW)
valid.df$Volume.KRW <- log2(valid.df$Volume.KRW)

head(bitcoin.df)
View(bitcoin.df)
dim(bitcoin.df)
summary(bitcoin.df)

# first model - time series linear model
# Creating Time series model
library(forecast)
bitcoin.ts <- ts(bitcoin.df$Volume.KRW, start = c(2017,7), end = c(2018,1), frequency = 4320)
plot(bitcoin.ts, xlab = "Hour", ylab = "bitcoin Volume.KRW", ylim = c(0, 5e+10))

# Create Linear model
bitcoin.lm <- tslm(bitcoin.ts ~ trend + I(trend^2), lambda = "auto")
lines(bitcoin.lm$fitted.values, lwd = 2, col = "red")

bitcoin.lm <- tslm(bitcoin.ts ~ trend + I(trend^3), lambda = "auto")
lines(bitcoin.lm$fitted.values, lwd = 2, col = "blue")

bitcoin.lm <- tslm(bitcoin.ts ~ trend + I(trend^5), lambda = "auto")
lines(bitcoin.lm$fitted.values, lwd = 2, col = "green")

bitcoin.lm <- tslm(bitcoin.ts ~ trend + I(trend^10), lambda = "auto")
lines(bitcoin.lm$fitted.values, lwd = 2)
summary(bitcoin.lm)

head(train.df)
View(train.df)




# second model - time series ARIMA model
# using ARIMA

train.ts <- ts(train.df$Volume.KRW, frequency = 24)
plot(train.ts)
plot(stl(train.ts, s.window="periodic"))

adf.test(diff(log(train.ts)), alternative = "stationary", k=0)
arima = auto.arima(train.ts, seasonal = TRUE, D=1)
result <- forecast(arima, h = 24)
accuracy(result)
plot(result)


count_ma = ts(train.df$Volume.KRW, frequency = 365)
arima = auto.arima(count_ma, D=1)
result <- forecast(arima, h = 365)
accuracy(result)
plot(result)

library(zoom)
plot(result) + abline(h=20)
zm()


# 2019.05.29 WED
# 2019년 데이터는 1월 데이터 일부가 누락되어 있기 때문에
# 2019년 2월 부터 시작한다.

# data partitioning - model process data
startDate = as.POSIXct("2019-02-01");
endDate = as.POSIXct("2019-03-31");
all_dates = seq(startDate, endDate, 3600);

for (j in 1:length(all_dates)) {
  filterdate = all_dates[j];
  model.df = subset(bitcoin.df, bitcoin.df$Date %in% all_dates)
}
# apply log2 to our target value
model.df$Volume.KRW <- log2(model.df$Volume.KRW)

# data partitioning - validation data
startDate = as.POSIXct("2019-04-01");
endDate = as.POSIXct("2019-04-16");
all_dates = seq(startDate, endDate, 3600);

for (j in 1:length(all_dates)) {
  filterdate = all_dates[j];
  valid.df = subset(bitcoin.df, bitcoin.df$Date %in% all_dates)
}
valid.df$Volume.KRW <- log2(valid.df$Volume.KRW)


model.ts <- ts(model.df$Volume.KRW, frequency = 24)
plot(model.ts , xlab = "Day")
# plot(stl(train.ts, s.window="periodic"))
# adf.test(diff(log(train.ts)), alternative = "stationary", k=0)
arima = auto.arima(model.ts, D=1)
result <- forecast(arima, h=24)
plot(result)
predict(arima, model.df$Volume.KRW)

accuracy(result, model.df$Volume.KRW)
plot(result)






