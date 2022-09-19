library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(caret)
library(tidyverse)
library(lubridate)

ret <- 0
ma <- 9

# 1 min
dat_zoo1min <- read.zoo("XAUUSD_1min_2021.csv", header=TRUE, index.column=1,sep=",",format="%d.%m.%Y %H:%M:%OS",tz="UTC")
gold1min <- as.xts(dat_zoo1min)
sma9_1min <- SMA(gold1min$Close, n = 9)
bbands_1min <- BBands(gold1min$Close, n = 9, sd = 3)
vol_1min <- volatility(gold1min, n = 9, N = 252, calc = "garman.klass")
data_wsma9_1min <- merge(gold1min, sma9_1min)
# 5 min
dat_zoo5min <- read.zoo("XAUUSD_5m_2021.csv", header=TRUE, index.column=1,sep=",",format="%d.%m.%Y %H:%M:%OS",tz="UTC")
gold5min <- as.xts(dat_zoo5min)
sma9_5min <- SMA(gold5min$Close, n = 9)
bbands_5min <- BBands(gold5min$Close, n = 9, sd = 3)
vol_5min <- volatility(gold5min, n = 9, N = 252, calc = "garman.klass")
data_wsma9_5min <- merge(gold5min, sma9_5min)
# 15 min
dat_zoo15min <- read.zoo("XAUUSD_15m_2021.csv", header=TRUE, index.column=1,sep=",",format="%d.%m.%Y %H:%M:%OS",tz="UTC")
gold15min <- as.xts(dat_zoo15min)
sma9_15min <- SMA(gold15min$Close, n = 9)
bbands_15min <- BBands(gold15min$Close, n = 9, sd = 3)
vol_15min <- volatility(gold15min, n = 9, N = 252, calc = "garman.klass")
data_wsma9_15min <- merge(gold15min, sma9_15min)
# 1 hour
dat_zoo1h <- read.zoo("XAUUSD_1H_2021.csv", header=TRUE, index.column=1,sep=",",format="%d.%m.%Y %H:%M:%OS",tz="UTC")
gold1h <- as.xts(dat_zoo1h)
sma9_1h <- SMA(gold1h$Close, n = 9)
bbands_1h <- BBands(gold1h$Close, n = 9, sd = 3)
vol_1h <- volatility(gold1h, n = 9, N = 252, calc = "garman.klass")
data_wsma9_1h <- merge(gold1h, sma9_1h)
# 4 hour
dat_zoo4h <- read.zoo("XAUUSD_4H_2021.csv", header=TRUE, index.column=1,sep=",",format="%d.%m.%Y %H:%M:%OS",tz="UTC")
gold4h <- as.xts(dat_zoo4h)
sma9_4h <- SMA(gold4h$Close, n = 9)
bbands_4h <- BBands(gold4h$Close, n = 9, sd = 3)
vol_4h <- volatility(gold4h, n = 9, N = 252, calc = "garman.klass")
data_wsma9_4h <- merge(gold4h, sma9_4h)
# 1 day
dat_zoo1d <- read.zoo("XAUUSD_1D_2021.csv", header=TRUE, index.column=1,sep=",",format="%d.%m.%Y %H:%M:%OS",tz="UTC")
gold1d <- as.xts(dat_zoo1d)
sma9_1d <- SMA(gold1d$Close, n = 9)
bbands_1d <- BBands(gold1d$Close, n = 9, sd = 3)
vol_1d <- volatility(gold1d, n = 9, N = 252, calc = "garman.klass")
# print(vol_1d[,1][[9]])
data_wsma9_1d <- merge(gold1d, sma9_1d)
# 1 week
dat_zoo1w <- read.zoo("XAUUSD_1W_2021.csv", header=TRUE, index.column=1,sep=",",format="%d.%m.%Y %H:%M:%OS",tz="UTC")
gold1w <- as.xts(dat_zoo1w)
sma9_1w <- SMA(gold1w$Close, n = 9)
bbands_1w <- BBands(gold1w$Close, n = 9, sd = 3)
vol_1w <- volatility(gold1w, n = 9, N = 252, calc = "garman.klass")
#print(vol_1w)
data_wsma9_1w <- merge(gold1w, sma9_1w)
# 1 month
dat_zoo1mon <- read.zoo("XAUUSD_1M_2021.csv", header=TRUE, index.column=1,sep=",",format="%d.%m.%Y %H:%M:%OS",tz="UTC")
gold1mon <- as.xts(dat_zoo1mon)
sma9_1mon <- SMA(gold1mon$Close, n = 9)
bbands_1mon <- BBands(gold1mon$Close, n = 9, sd = 3)
vol_1mon <- volatility(gold1mon, n = 9, N = 252, calc = "garman.klass")
#print(vol_1mon)
data_wsma9_1mon <- merge(gold1mon, sma9_1mon)
# print(data_wsma9)

data <- list(
    "1_MIN" = data_wsma9_1min,
    "5_MIN" = data_wsma9_5min,
    "15_MIN" = data_wsma9_15min,
    "1_HOUR" = data_wsma9_1h,
    "4_HOUR" = data_wsma9_4h,
    "1_DAY" = data_wsma9_1d,
    "1_WEEK" = data_wsma9_1w,
    "1_MON" = data_wsma9_1mon)

sma9_data <- list(
    "MIN_1" = sma9_1min,
    "MIN_5" = sma9_5min,
    "MIN_15" = sma9_15min,
    "HOUR_1" = sma9_1h,
    "HOUR_4" = sma9_4h,
    "DAY_1" = sma9_1d,
    "WEEK_1" = sma9_1w,
    "MON_1" = sma9_1mon)
