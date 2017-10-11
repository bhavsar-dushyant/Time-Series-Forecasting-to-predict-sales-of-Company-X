## Useful libraries for time series forecast. There are overlaps in functionalities ##
library(smooth)
library(forecast)
library(graphics)
library(datasets)
library(tseries)
library(fpp2)
install.packages("xlsx")
library(xlsx)
# Import datasets #

# Load CompanyX data from Library{datasets}#
CompanyX <- ts(CompanyX[[3]], start=c(2011, 1), end=c(2017, 5), frequency=12)
plot(CompanyX)
BY <- CompanyX


BY3 <- ma(BY, order=3) 
BY9 <- ma(BY, order=5)
BY19 <- ma(BY, order=7)

ts.plot(BY, BY3, BY9, BY19, lty=c(1:4), col=c('black','red','dark blue', 'forest green'))
BY3
# Forecasting using MA #
#Library{smooth} #

BY3H <- sma(BY, order=3, h=17, holdout=T, intervals="p", level=0.95) 
BY9H <- sma(BY, order=9, h=10, holdout=T, intervals="p", level=0.95) 
plot(BY3H)
BY3H$forecast
BY9H$forecast
#################################
## TS Decomposition ##

monthplot(CompanyX, col='maroon', main="Seasonal Subplot")


# Decompose time series into multiplicative components #
CompX.mult <- decompose(CompanyX, type="multiplicative")
plot(CompX.mult$figure, type="l") # Plots seasonality indices
plot(CompX.mult$trend, type="l") # Plots trend
plot(CompX.mult)
CompX.mult$trend
CompX.mult$random
# Predictive Accuracy: MAPE #

## CompanyX  ##
CompX1 <- window(CompanyX, start=c(2011,1), end=c(2015,12))
CompXHO <- window(CompanyX, start=c(2016,1), end=c(2017,5))

# Predict next 17 future values and compare #


## CompanyX ##

CompX.hw1 <- hw(CompanyX, seasonal='m', initial='o')
CompX.hw1$model
# simple exponential - models level
fit1 <- HoltWinters(CompanyX, alpha=0.0527, beta=FALSE, gamma=FALSE, seasonal = "mult")
plot(fit1)

# double exponential - models level and trend
fit2 <- HoltWinters(CompanyX, alpha=0.0527, beta=0.0001, gamma=FALSE)
plot(fit2)

# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(CompanyX, alpha=0.03, beta=0.01, gamma=0.6, seasonal = "mult")
plot(fit3, main="HW (alpha=0.3, beta=0.01, gamma=0.6)")

# Final tunned Holt-Winter model
CompXFC <- hw(CompX1, seasonal='m',alpha=0.0628, beta=0.0001, gamma=0.0001, h=17 )
plot(CompXFC)
CompXFC$mean



CompXFC$fitted
write.xlsx(CompXFC$mean, "/ALBERTA2.xlsx")

## Forecast by Exponential Smoothing ##

Vec2<- cbind(CompXHO,CompXFC$mean)
ts.plot(Vec2, col=c("blue", "red"), main="CompanyX: Actual vs Forecast")
MAPE <- mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1])
MAPE

# Detred Data #

BY1 <- lag(BY, k=-1)
df1 <- diff(BY, order=1)
cbind(BY, BY1, diff(BY, order=1))

####################
# Test for Stationarity
library(tseries)
adf.test(na.omit(CompX.mult$random))

######################
