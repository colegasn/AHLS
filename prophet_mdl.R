##### AHLS TIME SERIES - PROPHET MODEL #####
### Last Update: 8/29/2024

# Load packages
library(readxl)
library(prophet)
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(fable)
library(ggplot2)

# Data Import ---------------------------------------------

# Daily admissions
asthma <- readRDS("asthma.rds") %>%
  tibble() %>%
  mutate(WEEK_NAME=wday(Day, label=TRUE, abbr=TRUE),
         WEEK_NUMBER=epiweek(Day),
         WeekDate=floor_date(Day, unit="week", week_start=7),
         Quarter=quarter(Day, fiscal_start = 7, type="date_first")) %>%
  relocate(Quarter, .before=MONTH_NUMBER) %>%
  relocate(WEEK_NUMBER, .after=MONTH_NUMBER) %>%
  relocate(WEEK_NAME, .after=MONTH_NAME) %>%
  relocate(WeekDate, .after=MonthDate)
print(asthma, n=15)

# Weekly admissions
asthma_ts <- asthma %>%
  group_by(WeekDate) %>%
  summarise(AllAdmissions=sum(AllAdmissions)) %>%
  mutate(WEEK_NUMBER=epiweek(WeekDate),
         ROW_NUMBER=row_number()) %>%
  as_tsibble(index = ROW_NUMBER) %>%
  relocate(ROW_NUMBER, WEEK_NUMBER, WeekDate, AllAdmissions) %>%
  select(WeekDate, AllAdmissions)
print(asthma_ts, n=15)

# Quarterly admissions
asthma_tsq <- asthma %>%
  group_by(Quarter) %>%
  summarise(AllAdmissions=sum(AllAdmissions)) %>%
  as_tsibble(index=Quarter)
asthma_tsq

# Prophet Modeling --------------------------------------------------------

### Daily Admissions ------------------------------------------------------
# Call prophet() to fit the model - note the renaming of variables!
m_day <- asthma %>%
  rename(ds=Day, y=AllAdmissions) %>%
  prophet()

# Dataframe of historical + periods to forecast
future_day <- make_future_dataframe(m_day, periods = 365)
head(future_day, n=10)  
tail(future_day, n=10)

# Make forecasts on future timepoints
forecast_day <- predict(m_day, future_day)

# Explore the forecast
names(forecast_day)

# Display some forecast values
forecast_day %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

# Plot the forecast
plot(m_day, forecast_day)+
  theme_bw()+
  labs(x="Date", y="# of Admissions")+
  ggtitle("All Asthma Admissions - Daily")

# Break the forecast down by trend, seasonality, and random terms
prophet_plot_components(m_day, forecast_day)

# Interactive plot to explore the data
dyplot.prophet(m_day, forecast_day)


### Weekly Admissions -----------------------------------------------------
# Call prophet() to fit the model - note the renaming of variables!
m_wk <- asthma_ts %>%
  rename(ds=WeekDate, y=AllAdmissions) %>%
  data.frame() %>%
  prophet()

# Dataframe of historical + periods to forecast
future_wk <- make_future_dataframe(m_wk, periods = 52, freq = "week")
head(future_wk, n=10)  
tail(future_wk, n=10)

# Make forecasts on future timepoints
forecast_wk <- predict(m_wk, future_wk)

# Explore the forecast
names(forecast_wk)

# Display some forecast values
forecast_wk %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

# Plot the forecast
plot(m_wk, forecast_wk)+
  theme_bw()+
  theme(title = element_text(size=22),
        axis.text = element_text(size=16))+
  scale_x_continuous(name="",
                     breaks=c(as.POSIXct("2016-01-01"), as.POSIXct("2017-01-01"),
                              as.POSIXct("2018-01-01"), as.POSIXct("2019-01-01"),
                              as.POSIXct("2020-01-01"), as.POSIXct("2021-01-01"),
                              as.POSIXct("2022-01-01"), as.POSIXct("2023-01-01")),
                     limits = c(as.POSIXct("2016-01-01"), as.POSIXct("2023-07-01")),
                     labels = c("2016","2017","2018","2019","2020","2021","2022","2023"),
               expand = c(0,0))+
  scale_y_continuous(name="# of Admissions",
                     breaks=seq(0,50,by=5))+
  geom_hline(yintercept = 0, color="grey50", linewidth=0.8)+
  ggtitle("Prophet - Weekly Admissions Forecast ")

# Break the forecast down by trend, seasonality, and random terms
prophet_plot_components(m_wk, forecast_wk)

# Interactive plot to explore the data
dyplot.prophet(m_wk, forecast_wk)

### Quarterly Admissions --------------------------------------------------
# Call prophet() to fit the model - note the renaming of variables!
m_q <- asthma_tsq %>%
  rename(ds=Quarter, y=AllAdmissions) %>%
  data.frame() %>%
  prophet()

# Dataframe of historical + periods to forecast
future_q <- make_future_dataframe(m_q, periods = 8, freq = "quarter")
head(future_q, n=10)  
tail(future_q, n=10)

# Make forecasts on future timepoints
forecast_q <- predict(m_q, future_q)

# Explore the forecast
names(forecast_q)

# Display some forecast values
forecast_q %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

# Plot the forecast
plot(m_q, forecast_q)+
  theme_bw()+
  labs(x="Date", y="# of Admissions")+
  ggtitle("All Asthma Admissions - Quarterly")

# Break the forecast down by trend, seasonality, and random terms
prophet_plot_components(m_q, forecast_q)

# Interactive plot to explore the data
dyplot.prophet(m_q, forecast_q)

# Dynamic Modelling -------------------------------------------------------

# Read in time series with asthma and AQI
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
asthma <- readRDS(paste(dir, "asthma.rds", sep="")) %>%
  select(Day, AllAdmissions, aqi) %>%
  rename(ds=Day, y=AllAdmissions)
asthma
c(min(asthma$ds), max(asthma$ds))

# Split to training and test sets
asthma_train <- asthma %>%
  filter(year(ds) < "2022")
asthma_test <- asthma %>%
  filter(year(ds) >= "2022")

# Check date ranges of training and test sets
c(min(asthma_train$ds), max(asthma_train$ds))
c(min(asthma_test$ds), max(asthma_test$ds))

#####
# BASE

# Call prophet() to fit the model
m <- asthma_train %>%
  prophet()

# Dataframe of historical + periods to forecast
future_m <- make_future_dataframe(m, periods = nrow(asthma_test), freq = "day")
head(future_m, n=10)  
tail(future_m, n=10)

# Make forecasts on future timepoints
forecast_m <- predict(m, future_m)

# Display some forecast values
forecast_m %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=39)

#####
# BASE MODEL + AQI

# Add AQI to prophet model
m_aqi <- prophet()
m_aqi <- add_regressor(m_aqi, "aqi")
m_aqi <- fit.prophet(m_aqi, asthma_train)

# Dataframe of historical + periods to forecast
future_m_aqi <- make_future_dataframe(m_aqi, periods = nrow(asthma_test), freq = "day")
future_m_aqi <- inner_join(future_m_aqi, asthma_test, by=c("ds"))
head(future_m_aqi, n=10)  
tail(future_m_aqi, n=10)

# Make forecasts on future timepoints
forecast_m_aqi <- predict(m_aqi, future_m_aqi)

# Display some forecast values
forecast_m_aqi %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

# Merge and display predictions
f.base <- forecast_m %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(yhat_b=yhat, yhat_lower_b=yhat_lower, yhat_upper_b=yhat_upper)
f.base_aqi <- forecast_m_aqi %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(yhat_aqi=yhat, yhat_lower_aqi=yhat_lower, yhat_upper_aqi=yhat_upper)
f <- inner_join(f.base, f.base_aqi, by="ds")
f2 <- asthma_test %>%
  select(ds, y, aqi) %>%
  as.data.frame() %>%
  inner_join(f, by="ds") %>%
  mutate(ds=ymd(ds)) %>%
  relocate(ds, y, aqi,
           yhat_lower_b, yhat_lower_aqi,
           yhat_b, yhat_aqi,
           yhat_upper_b, yhat_upper_aqi) %>%
  as_tsibble(index=ds)
f2

# Obtain error measurements
f.error <- f2 %>%
  summarise(yhat.SSE=sum((y-yhat_b)^2), yhat_aqi.SSE=sum((y-yhat_aqi)^2),
            yhat.SAE=sum(abs(y-yhat_b)), yhat_aqi.SAE=sum(abs(y-yhat_aqi)),
            yhat.MAE=median(abs(y-yhat_b)), yhat_aqi.MAE=median(abs(y-yhat_aqi)),
            yhat.MAPE=(1/nrow(f2))*sum(abs((y-yhat_b)/y)), yhat_aqi.MAPE=(1/nrow(f2))*sum(abs((y-yhat_aqi)/y)))
f.error

# Sum of squared error
f.error[,2:3]

# Sum of absolute error
f.error[,4:5]

# Median absolute error
f.error[,6:7]

# Median absolute percentage error
f.error[,8:9]

################################################################################

##### WEEKLY PREDICTIONS
# Read in daily asthma admissions
asthma <- readRDS("asthma.rds") %>%
  tibble() %>%
  mutate(WEEK_NAME=wday(Day, label=TRUE, abbr=TRUE),
         WEEK_NUMBER=epiweek(Day),
         WeekDate=floor_date(Day, unit="week", week_start=7),
         Quarter=quarter(Day, fiscal_start = 7, type="date_first")) %>%
  relocate(Quarter, .before=MONTH_NUMBER) %>%
  relocate(WEEK_NUMBER, .after=MONTH_NUMBER) %>%
  relocate(WEEK_NAME, .after=MONTH_NAME) %>%
  relocate(WeekDate, .after=MonthDate)

# Aggregate
asthma_ts <- asthma %>%
  group_by(WeekDate) %>%
  summarise(AllAdmissions=sum(AllAdmissions),
            aqi.sum=sum(aqi),
            aqi.mean=mean(aqi),
            aqi.median=median(aqi)) %>%
  mutate(WEEK_NUMBER=epiweek(WeekDate),
         ROW_NUMBER=row_number()) %>%
  as_tsibble(index = ROW_NUMBER) %>%
  relocate(ROW_NUMBER, WEEK_NUMBER, WeekDate, AllAdmissions) %>%
  select(WeekDate, AllAdmissions, aqi.sum, aqi.mean, aqi.median) %>%
  rename(ds=WeekDate, y=AllAdmissions)
print(asthma_ts, n=15)

# Split to training and test sets
asthma_train <- asthma_ts %>%
  filter(year(ds) < "2022")
asthma_test <- asthma_ts %>%
  filter(year(ds) >= "2022")

# Check date ranges of training and test sets
c(min(asthma_train$ds), max(asthma_train$ds))
c(min(asthma_test$ds), max(asthma_test$ds))

#####
# BASE

# Call prophet() to fit the model
m <- asthma_train %>%
  prophet()

# Dataframe of historical + periods to forecast
future_m <- make_future_dataframe(m, periods = nrow(asthma_test), freq = "week")
head(future_m, n=10)  
tail(future_m, n=10)

# Make forecasts on future timepoints
forecast_m <- predict(m, future_m) %>%
  mutate(ds=ymd(ds)) %>%
  as_tsibble(index="ds")

# Display some forecast values
forecast_m %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=39)

#####
# BASE MODEL + AQI

### AQI SUM
# Add AQI to prophet model
m_aqi.sum <- prophet()
m_aqi.sum <- add_regressor(m_aqi.sum, "aqi.sum")
m_aqi.sum <- fit.prophet(m_aqi.sum, asthma_train)

# Dataframe of historical + periods to forecast
future_m_aqi.sum <- make_future_dataframe(m_aqi.sum, periods = nrow(asthma_test), freq = "week")
future_m_aqi.sum <- inner_join(future_m_aqi.sum, asthma_test, by=c("ds"))
head(future_m_aqi.sum, n=10)  
tail(future_m_aqi.sum, n=10)

# Make forecasts on future timepoints
forecast_m_aqi.sum <- predict(m_aqi.sum, future_m_aqi.sum) %>%
  mutate(ds=ymd(ds)) %>%
  as_tsibble(index="ds")

# Display some forecast values
forecast_m_aqi.sum %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

### AQI MEAN
# Add AQI to prophet model
m_aqi.mean <- prophet()
m_aqi.mean <- add_regressor(m_aqi.mean, "aqi.mean")
m_aqi.mean <- fit.prophet(m_aqi.mean, asthma_train)

# Dataframe of historical + periods to forecast
future_m_aqi.mean <- make_future_dataframe(m_aqi.mean, periods = nrow(asthma_test), freq = "week")
future_m_aqi.mean <- inner_join(future_m_aqi.mean, asthma_test, by=c("ds"))
head(future_m_aqi.mean, n=10)  
tail(future_m_aqi.mean, n=10)

# Make forecasts on future timepoints
forecast_m_aqi.mean <- predict(m_aqi.mean, future_m_aqi.mean) %>%
  mutate(ds=ymd(ds)) %>%
  as_tsibble(index="ds")

# Display some forecast values
forecast_m_aqi.mean %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

### AQI MEDIAN
# Add AQI to prophet model
m_aqi.median <- prophet()
m_aqi.median <- add_regressor(m_aqi.median, "aqi.median")
m_aqi.median <- fit.prophet(m_aqi.median, asthma_train)

# Dataframe of historical + periods to forecast
future_m_aqi.median <- make_future_dataframe(m_aqi.median, periods = nrow(asthma_test), freq = "week")
future_m_aqi.median <- inner_join(future_m_aqi.median, asthma_test, by=c("ds"))
head(future_m_aqi.median, n=10)  
tail(future_m_aqi.median, n=10)

# Make forecasts on future timepoints
forecast_m_aqi.median <- predict(m_aqi.median, future_m_aqi.median) %>%
  mutate(ds=ymd(ds)) %>%
  as_tsibble(index="ds")

# Display some forecast values
forecast_m_aqi.median %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

#####

# Get actual observations
f <- asthma_test %>%
  select(ds, y, aqi.sum, aqi.mean, aqi.median) %>%
  as.data.frame() %>%
  mutate(ds=ymd(ds)) %>%
  as_tsibble(index=ds)

# Merge in base predictions
f.base <- forecast_m %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  inner_join(f, f.base, by="ds")
f.base

# Merge in base + AQI SUM predictions
f.base_aqi.sum <- forecast_m_aqi.sum %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(yhat_aqi.sum=yhat, yhat_lower_aqi.sum=yhat_lower, yhat_upper_aqi.sum=yhat_upper) %>%
  inner_join(f.base, by="ds")
f.base_aqi.sum

# Merge in base + AQI MEAN predictions
f.base_aqi.mean <- forecast_m_aqi.mean %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(yhat_aqi.mean=yhat, yhat_lower_aqi.mean=yhat_lower, yhat_upper_aqi.mean=yhat_upper) %>%
  inner_join(f.base_aqi.sum, by="ds")
f.base_aqi.mean

# Merge in base + AQI MEAN predictions
f.base_aqi.median <- forecast_m_aqi.median %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(yhat_aqi.median=yhat, yhat_lower_aqi.median=yhat_lower, yhat_upper_aqi.median=yhat_upper) %>%
  inner_join(f.base_aqi.mean, by="ds")
f.base_aqi.median

# Rearrange
f2 <- f.base_aqi.median %>%
  mutate(ds=ymd(ds)) %>%
  as_tibble(index="ds") %>%
  relocate(ds, y, yhat, yhat_aqi.sum, yhat_aqi.mean, yhat_aqi.median,
           yhat_lower, yhat_lower_aqi.sum, yhat_lower_aqi.mean, yhat_lower_aqi.median,
           yhat_upper, yhat_upper_aqi.sum, yhat_upper_aqi.mean, yhat_upper_aqi.median)
f2

# Obtain error measurements
f.error <- f2 %>%
  summarise(yhat.SSE=sum((y-yhat)^2), yhat_aqi.sum.SSE=sum((y-yhat_aqi.sum)^2), yhat_aqi.mean.SSE=sum((y-yhat_aqi.mean)^2), yhat_aqi.median.SSE=sum((y-yhat_aqi.median)^2),
            yhat.SAE=sum(abs(y-yhat)), yhat_aqi.sum.SAE=sum(abs(y-yhat_aqi.sum)), yhat_aqi.mean.SAE=sum(abs(y-yhat_aqi.mean)), yhat_aqi.median.SAE=sum(abs(y-yhat_aqi.median)),
            yhat.MAE=median(abs(y-yhat)), yhat_aqi.sum.MAE=median(abs(y-yhat_aqi.sum)), yhat_aqi.mean.MAE=median(abs(y-yhat_aqi.mean)), yhat_aqi.median.MAE=median(abs(y-yhat_aqi.median)),
            yhat.MAPE=(1/nrow(f2))*sum(abs((y-yhat)/y)), yhat_aqi.sum.MAPE=(1/nrow(f2))*sum(abs((y-yhat_aqi.sum)/y)),
            yhat_aqi.mean.MAPE=(1/nrow(f2))*sum(abs((y-yhat_aqi.mean)/y)), yhat_aqi.median.MAPE=(1/nrow(f2))*sum(abs((y-yhat_aqi.median)/y)))

# Sum of squared error
f.error[,1:4]

# Sum of absolute error
f.error[,5:8]

# Median absolute error
f.error[,9:12]

# Median absolute percentage error
f.error[,13:16]

# Plot of base predictions
base.plot <- asthma_ts %>%
  ggplot()+
  geom_line(data=filter(asthma_ts, ds>=date("2021-01-01") & ds<=date("2021-12-31")),
            aes(x=ds, y=y, color="black"), lwd=0.8, lty="solid")+
  geom_line(data=filter(asthma_ts, ds>=date("2022-01-01") & ds<=date("2023-06-30")),
            aes(x=ds, y=y, color="grey30"), lwd=0.8, lty="solid")+
  geom_ribbon(data=f2, aes(x=ds, ymin=yhat_lower, ymax=yhat_upper),
              fill="#4285f4", alpha=0.15)+
  geom_line(data=f2, aes(x=ds, y=yhat, color="#4285f4"), lwd=0.8, lty="solid")+
  scale_color_manual(name="", values=c("#4285f4","black","grey30"),
                     labels=c("Predicted","Actual","Observed"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=8),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Asthma Forecast - Base Model Only",
          subtitle="January 1, 2021 - June 30, 2023")
base.plot

# Plot of AQI SUM predictions
aqi_sum.plot <- asthma_ts %>%
  ggplot()+
  geom_line(data=filter(asthma_ts, ds>=date("2021-01-01") & ds<=date("2021-12-31")),
            aes(x=ds, y=y, color="black"), lwd=0.8, lty="solid")+
  geom_line(data=filter(asthma_ts, ds>=date("2022-01-01") & ds<=date("2023-06-30")),
            aes(x=ds, y=y, color="grey30"), lwd=0.8, lty="solid")+
  geom_ribbon(data=f2, aes(x=ds, ymin=yhat_lower_aqi.sum, ymax=yhat_upper_aqi.sum),
              fill="#fbbc05", alpha=0.15)+
  geom_line(data=f2, aes(x=ds, y=yhat_aqi.sum, color="#fbbc05"), lwd=0.8, lty="solid")+
  scale_color_manual(name="", values=c("#fbbc05","black","grey30"),
                     labels=c("Predicted","Actual","Observed"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=8),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Asthma Forecast - Base + Weekly AQI Sum",
          subtitle="January 1, 2021 - June 30, 2023")
aqi_sum.plot

# Plot of AQI MEAN predictions
aqi_mean.plot <- asthma_ts %>%
  ggplot()+
  geom_line(data=filter(asthma_ts, ds>=date("2021-01-01") & ds<=date("2021-12-31")),
            aes(x=ds, y=y, color="black"), lwd=0.8, lty="solid")+
  geom_line(data=filter(asthma_ts, ds>=date("2022-01-01") & ds<=date("2023-06-30")),
            aes(x=ds, y=y, color="grey30"), lwd=0.8, lty="solid")+
  geom_ribbon(data=f2, aes(x=ds, ymin=yhat_lower_aqi.mean, ymax=yhat_upper_aqi.mean),
              fill="#34a853", alpha=0.15)+
  geom_line(data=f2, aes(x=ds, y=yhat_aqi.mean, color="#34a853"), lwd=0.8, lty="solid")+
  scale_color_manual(name="", values=c("#34a853","black","grey30"),
                     labels=c("Predicted","Actual","Observed"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=8),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Asthma Forecast - Base + Weekly AQI Average",
          subtitle="January 1, 2021 - June 30, 2023")
aqi_mean.plot

# Plot of AQI MEDIAN predictions
aqi_median.plot <- asthma_ts %>%
  ggplot()+
  geom_line(data=filter(asthma_ts, ds>=date("2021-01-01") & ds<=date("2021-12-31")),
            aes(x=ds, y=y, color="black"), lwd=0.8, lty="solid")+
  geom_line(data=filter(asthma_ts, ds>=date("2022-01-01") & ds<=date("2023-06-30")),
            aes(x=ds, y=y, color="grey30"), lwd=0.8, lty="solid")+
  geom_ribbon(data=f2, aes(x=ds, ymin=yhat_lower_aqi.median, ymax=yhat_upper_aqi.median),
              fill="#ea4335", alpha=0.15)+
  geom_line(data=f2, aes(x=ds, y=yhat_aqi.median, color="#ea4335"), lwd=0.8, lty="solid")+
  scale_color_manual(name="", values=c("#ea4335","black","grey30"),
                     labels=c("Predicted","Actual","Observed"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=8),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Asthma Forecast - Base + Weekly AQI Median",
          subtitle="January 1, 2021 - June 30, 2023")
aqi_median.plot

# Put all plots on one page
library(ggpubr)
ggarrange(base.plot, aqi_sum.plot, aqi_mean.plot, aqi_median.plot)

# Plot everything
all.plot <- asthma_ts %>%
  ggplot()+
  geom_line(data=filter(asthma_ts, ds>=date("2021-01-01") & ds<=date("2021-12-31")),
            aes(x=ds, y=y, color="black"), lwd=0.8, lty="solid")+
  geom_line(data=filter(asthma_ts, ds>=date("2022-01-01") & ds<=date("2023-06-30")),
            aes(x=ds, y=y, color="grey30"), lwd=0.8, lty="solid")+
  geom_ribbon(data=f2, aes(x=ds, ymin=yhat_lower_aqi.sum, ymax=yhat_upper_aqi.sum),
              fill="#fbbc05", alpha=0.2)+
  geom_line(data=f2, aes(x=ds, y=yhat_aqi.sum, color="#fbbc05"), lwd=0.8, lty="solid", alpha=0.5)+
  geom_ribbon(data=f2, aes(x=ds, ymin=yhat_lower_aqi.mean, ymax=yhat_upper_aqi.mean),
              fill="#34a853", alpha=0.2)+
  geom_line(data=f2, aes(x=ds, y=yhat_aqi.mean, color="#34a853"), lwd=0.8, lty="solid", alpha=0.5)+
  geom_ribbon(data=f2, aes(x=ds, ymin=yhat_lower_aqi.median, ymax=yhat_upper_aqi.median),
              fill="#ea4335", alpha=0.2)+
  geom_line(data=f2, aes(x=ds, y=yhat_aqi.median, color="#ea4335"), lwd=0.8, lty="solid", alpha=0.5)+
  geom_ribbon(data=f2, aes(x=ds, ymin=yhat_lower, ymax=yhat_upper),
              fill="#4285f4", alpha=0.25)+
  geom_line(data=f2, aes(x=ds, y=yhat, color="#4285f4"), lwd=0.8, lty="solid",alpha=1)+
  scale_color_manual(name="", values=c("#34a853","#4285f4","#ea4335","#fbbc05","black","grey30"),
                     labels=c("AQI Average","No AQI","AQI Median","AQI Sum","Actual","Observed"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=8),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Asthma Forecast - Base Model Only",
          subtitle="January 1, 2021 - June 30, 2023")
all.plot
