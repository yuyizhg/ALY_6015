# Loading packages and file
library(readr)
library(dplyr)
Air_Traffic_Passenger_Statistics <- read_csv("~/FCR/NEU/CPS/Analytics_2018/ALY 6015_Intermediate Analytics/Week 6_Collaborative Project/Air_Traffic_Passenger_Statistics.csv", head(TRUE))


# Part A: Descriptive and Regression
# Aggregate Data
Passenger_Statistics <- Air_Traffic_Passenger_Statistics %>%
  group_by(`Activity Period`) %>%
  summarise(`Passenger Count` = sum(`Passenger Count`))
summary(Passenger_Statistics)
ps <- Passenger_Statistics$`Passenger Count`

# Create Histogram
hist(ps, breaks = 15, ylim = c(0,20), col = "#ffffe6", main = "Passenger Count by Month 2005-2019", xlab = "Passenger Counts")

# Create Density Plots
plot(density(ps), bty = "n", main = "Passenger Counts")
polygon(density(ps), col = "#ffffe6")
abline(v = mean(ps), lwd = 2, col = "#999999")
abline(v = median(ps), lwd = 2, lty = 3, col = "#999999")

# Boxplots
boxplot(ps, col = "#ffffe6", main = "Passenger Count by Month 2005-2019", xlab = "Passenger Counts", frame.plot = TRUE, boxwex = 0.35, horizontal = TRUE)
rug(ps, side = 1)

# Normal probability plots
head(Passenger_Statistics)
qqnorm(ps)
qqline(ps, col = "red")


# Regression Equation
San_Francisco_Monthly_Average_Temperature <- read_csv("~/FCR/NEU/CPS/Analytics_2018/ALY 6015_Intermediate Analytics/Week 6_Collaborative Project/San Francisco Monthly Average Temperature.csv")
summary(San_Francisco_Monthly_Average_Temperature)

United_only <- Air_Traffic_Passenger_Statistics %>%
  filter(`Operating Airline` == "United Airlines" | `Operating Airline` == "United Airlines - Pre 07/01/2013" ) %>%
  group_by(`Activity Period`) %>%
  summarise(`Passenger Count` = sum(`Passenger Count`))
summary(United_only)

Regression <- lm(Passenger_Statistics$`Passenger Count` ~ San_Francisco_Monthly_Average_Temperature$Temperature + United_only$`Passenger Count`)
summary(Regression)


# Part B: Time Series
# Time Series Preparation
pstimeseries <- ts(Passenger_Statistics$`Passenger Count`, frequency=12, start=c(2005,7))
pstimeseries
plot.ts(pstimeseries)

# Decomposing Time Series
pstimeseriescomponents <- decompose(pstimeseries)
plot(pstimeseriescomponents)

# Differencing a Time Series
pstimeseriesdiff1 <- diff(pstimeseries, differences=1)
plot.ts(pstimeseriesdiff1)

# Selecting a Candidate ARIMA Model
acf(pstimeseriesdiff1, lag.max=20)
acf(pstimeseriesdiff1, lag.max=20, plot = FALSE)

pacf(pstimeseriesdiff1, lag.max=20)
pacf(pstimeseriesdiff1, lag.max=20, plot = FALSE)

library("forecast")
auto.arima(Passenger_Statistics$`Passenger Count`)
pstimeseriesarima <- Arima(pstimeseries, order = c(5,1,1), include.drift = TRUE)
pstimeseriesarima
