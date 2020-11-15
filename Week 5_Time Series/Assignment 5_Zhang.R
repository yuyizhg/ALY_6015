# Part A:
# Data Preparation
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries

# Decomposing Time Series
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)


# Part B: ARIMA Models
# Data Preparation
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust, start=c(1500))
volcanodustseries
plot.ts(volcanodustseries)

# Differencing a Time Series
#volcanodustseriesdiff1 <- diff(volcanodustseries, differences=1)
#plot.ts(volcanodustseriesdiff1)

# Selecting a Candidate ARIMA Model
acf(volcanodustseries, lag.max=20)
acf(volcanodustseries, lag.max=20, plot = FALSE)

pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot = FALSE)

library(forecast)
auto.arima(volcanodust)
