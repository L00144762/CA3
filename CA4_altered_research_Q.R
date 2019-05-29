#### time series: altering the research question due to poor prediction quality of linear regression
#
a <- data.frame(AirPassengers)


library(tseries)
library(forecast)

#####ULSTER####
# converting Ulster temp to time series
Ulster_test <- slice(Ulster, 1:(n()-3))

ts_Ulster_Temp <- ts(Ulster_test$`Malin Head Temp`, start = c(1990,1),
                     frequency = 12)

plot(ts_Ulster_Temp, main = "Raw time series")
par(resetPar())
# there seems to be some seasonal effect and random fluctuations
# but no overall trend which is good 
y_range <- c(min(ts_Ulster_Temp), max(ts_Ulster_Temp))
y_range

plot(ma(ts_Ulster_Temp, 3), main = "Simple moving avg (k=3)",
     ylim = y_range)
plot(ma(ts_Ulster_Temp, 7), main = "Simple moving avg (k=7)",
     ylim = y_range)
plot(ma(ts_Ulster_Temp, 15), main = "Simple moving avg (k=15)",
     ylim = y_range)

#ma smooths the time seires. as k increases so does smoothness

adf.test(ts_Ulster_Temp)
# p value less than 0.05: time series is stationary
# and there is no trend
ndiffs(ts_Ulster_Temp)

# confirms that nothing needs to be done to the series
# and that it is stationary i.e series does not need
# to be differenced
Pacf(ts_Ulster_Temp, main = "partial auto correlation plot for Ulster Temp")
Acf(ts_Ulster_Temp, main = "auto correlation plot for Ulster temp")

#there are loads of different p and q values that can be chosen,
# best to use auto ARIMA instead of cycling through each
# set manually

#data has to be seasonally decomposed first
# take the log of the series to convert from multiplicative
# to additive
log_Ulster <- log(ts_Ulster_Temp)
plot(log_Ulster)

# stl function to dempose the series
# "period" so the seasonal components remain the same
# across each year
season_decomp <- stl(log_Ulster, s.window = "period")
plot(season_decomp)

# components to be reverted back to original metric
# take the exponential of the logged series
ts_Ulster_temp_converted <- exp(season_decomp$time.series)
plot(ts_Ulster_temp_converted)
ts_Ulster_temp_converted
# example (Ulster): sep 2017 saw a 37% seasonal change 

# seasonadj removes the seasonalitiy   ULSTER
ulster_seasonadj <- seasadj(season_decomp)

seasonplot(ts_Ulster_Temp, 12, col=rainbow(12),
           year.labels =  TRUE,
           main = "seasonal plot of ulster temp")
##outlier in september 2013; temperature was very low compared to every other
# year. this indicates either the influence of an unseen variable (heatwave maybe)
# or it is an error in the data. we'll see how it effects the series

seasonplot(ulster_seasonadj, 12, col=rainbow(12),
           year.labels =  TRUE,
           main = "seasonal element removed from ulster temp")

ndiffs(ulster_seasonadj)
# d value will be 0
adf.test(ulster_seasonadj)
#stationary, regardless of the mad outlier in 2013 september

# using auto arima to find the best model
# there were too many options for the p and q values
# 1,0,0 appears to be the best one
aa_ulster <- auto.arima((ulster_seasonadj))
aa_ulster
qqnorm(aa_ulster$residuals)
qqline(aa_ulster$residuals)

# trying a different arima model to compare accuracy
arima_ulster <- Arima(ts_Ulster_Temp, order = c(1,0,1))
arima_ulster

accuracy(aa_ulster)
accuracy(arima_ulster)
#MAPE is a much lower for auto arima model. its the best one 
# to use

# testing the model fit
# H0 =autocorrelations are all zero
# but in this example p>0.05 and so H0 is rejected
# the arima model is good fit for the data
Box.test(aa_ulster$residuals, type = "Ljung-Box")

forecast(aa_ulster, 3)
# not far off the actual values. Good model
# outlier must be accounted for 
plot(forecast(aa_ulster, 3))
plot(ts_Ulster_Temp)

# after reading met erieann weather report for 2013 September,
# it appears there was no obvious cause to the outlier (low temperature)
# this is likely an error in the data, and not caused by some unseen variable

############ CONNACHT #################

#removing the last 3 rows from the original dataframe
# predict these 3 and compare the predicted to actual
Connacht_test <- slice(Connaucht, 1:(n()-3))

ts_Connacht_Temp <- ts(Connacht_test$Mean_Temp, start = c(1990,1),
                       frequency = 12)
plot(ts_Connacht_Temp, main = "Raw time series")
par(resetPar())
# there seems to be some seasonal effect and random fluctuations
# but no overall trend which is good 
ts_Connacht_Temp 

adf.test(ts_Connacht_Temp)
# p value less than 0.05: time series is stationary
# and there is no trend

ndiffs(ts_Connacht_Temp)
# confirms that nothing needs to be done to the series
# and that it is stationary i.e series does not need
# to be differenced

Pacf(ts_Connacht_Temp, main = "partial auto correlation plot for Connacht temp")
Acf(ts_Connacht_Temp, main = "auto correlation plot for Connacht temp")
#there are loads of different p and q values that can be chosen,
# best to use auto ARIMA instead of cycling through each
# set manually

#data has to be seasonally decomposed first
# take the log of the series to convert from multiplicative
# to additive

log_Connacht <- log(ts_Connacht_Temp)
plot(log_Connacht)
# stl function to dempose the series
# "period" so the seasonal components remain the same
# across each year

season_decomp_conn <- stl(log_Connacht, s.window = "period")
plot(season_decomp_conn)
# components to be reverted back to original metric
# take the exponential of the logged series

ts_conn_converted <- exp(season_decomp_conn$time.series)
plot(ts_conn_converted)
# example (Ulster): sep 2017 saw a 37% seasonal change 

# seasonadj removes the seasonalitiy
connacht_seasonadj <- seasadj(season_decomp_conn)

seasonplot(ts_Connacht_Temp, 12, col = rainbow(12),
           year.labels = TRUE, 
           main = "seasonal plot of connacht temp")

seasonplot(connacht_seasonadj, 12, col = rainbow(12),
           year.labels = TRUE, 
           main = "seasonal plot of connacht temp")
#massive outlier again for this dataset. this time in december 2010
# this will effect the prediction
ts_Connacht_Temp

ndiffs(connacht_seasonadj)
# no differencing needed

adf.test(connacht_seasonadj)
# p value < 0.05
# good model fit 

aa_connacht <- auto.arima(connacht_seasonadj)
aa_connacht

qqnorm(aa_connacht$residuals)
qqline(aa_connacht$residuals)
# the outlier has caused problems
# need to sort it out
# replace this value with median/mean
# depending on the distribution

accuracy(aa_connacht)
# mape of 19%

forecast(aa_connacht, 3)
plot(forecast(aa_connacht, 3))


# the outlier for this year is explained by the met ireland website
# which states 2009/2010 was the coldest winter in Ireland since the 1960's
# this explains the low temperature for the months of jan and feb
# the major outlier in december of 2010 is explained by met eireann
# Mullingar had the coldest recorded temperature in over 50 years

#### Munster #####

