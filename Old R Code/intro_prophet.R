##### INTRODUCTION TO PROPHET MODELS #####
### Last Update: 12/13/2023

# Load packages
library(prophet)
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(fable)
library(ggplot2)


# Tutorial #1: Peyton Manning ---------------------------------------------

# Read in the data
manning <- read.csv('https://raw.githubusercontent.com/facebook/prophet/main/examples/example_wp_log_peyton_manning.csv') %>%
  mutate(Date=ymd(ds)) %>%
  rename(log.Views=y) %>%
  select(Date, log.Views) %>%
  tsibble()
manning
c(min(manning$Date), max(manning$Date))

# Plot the data
manning %>%
  autoplot(log.Views, color="#4600d1", lwd=0.5)+
  theme_bw()+
  labs(x="Date", y="log(Views)")+
  ggtitle("Peyton Manning Views on Wikipedia")

# Examine the prophet() function help documentation
?prophet

# Call prophet() to fit the model - note the renaming of variables!
m <- manning %>%
  rename(ds=Date, y=log.Views) %>%
  prophet()

# Explore the model fit
names(m)

# Examine make_future_dataframe() help documentation
?make_future_dataframe

# Dataframe of historical + periods to forecast
future <- make_future_dataframe(m, periods = 365*5)
head(future, n=10)  # start of ts: "2007-12-10"
tail(future, n=10)  # end of ts: "2016-01-20" + 365 days = "2017-01-19"

# Make forecasts on future timepoints
forecast <- predict(m, future)

# Explore the forecast
names(forecast)

# Display some forecast values
forecast %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

# Plot the forecast
plot(m, forecast)+
  theme_bw()+
  labs(x="Date", y="log(Views)")+
  ggtitle("Peyton Manning Views on Wikipedia - Forecast")
  
# Break the forecast down by trend, seasonality, and random terms
prophet_plot_components(m, forecast)

# Interactive plot to explore the data
dyplot.prophet(m, forecast)
