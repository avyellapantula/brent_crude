set up our libraries
library(fpp2)
library(readxl)
library(mFilter)

#get our data
bd_ts <- read.csv(file="C:/Users/avocado/Desktop/crude_oil/crude_samp.csv",header=TRUE,sep=",")
#set up date and price to match values
bd_ts$date=as.Date(bd_ts$date)
bd_ts$price=as.numeric(paste(bd_ts$price))

#set vars for these columns
date_b=bd_ts$date
price_b = bd_ts$price

#make sure the classes match
sapply(bd_ts,class)
bd_ts[is.na(bd_ts)]<- 0
plot(bd_ts,type="l",col=118)

#now we set up our train and test data
#first train, should be 3/4 of our data set

#get the dimensions of the data set
dim(bd_ts)

numTrain = round(10*0.75)
numTrain
numTest = 10-numTrain
numTest
#We shall have the first 3/4 dedicated to the training and the last for test

bd_train = bd_ts[1:8,]
bd_test = bd_ts[9:10,]

#plot the training
plot(bd_train,type="l",col=118)

#let's get our data for the ending of our training, jsyk
bd_ts[8,]

#now we're going to attempt a detrending through a smoothing spline
#hp-filter

bd_ts.hp <- hpfilter(bd_ts, type=c("lambda"), freq=3)

trend=ts(bd_ts$trend)
cycle=ts(bd_ts$cycle)
plot(ts.union(trend,cycle),type="l",xlab="Time",ylab="", main='Decomposition of brent monthly price as trend and cycle')

bd_low <- ts(loess(brent$price~brent$time,span=0.4)$fitted)
bd_hi <- ts(brent$price - loess(brent$price~brent$time,span=0.07)$fitted)
wti_cycles <- brent$price - bd_low - bd_hi
plot(ts.union(brent$price, bd_low,bd_hi,wti_cycles),
     main="Decomposition of brent monthly price as trend + noise + cycles")

res <- residuals(naive(bd_ts))

plot(brent$price,type='l',ylab='brent price')

plot(log(brent$price),type='l',ylab='log(brent)')

plot(diff(log(brent$price),differences = 1),type='l',ylab='difference of log(brent)')

plot(diff(log(brent$price),differences = 2),type='l',ylab='difference of log(brent)')

diff_logprice=diff(log(brent$price),differences = 1)
f=spectrum(diff_logprice,spans=c(2,2), main="Smoothed periodogram")

1/f$freq[which.max(f$spec)]

# Model for ARIMA

sarima=arima(log(brent$price),order=c(2,1,2),seasonal=list(order=c(1,0,1),period=12))
sarima

