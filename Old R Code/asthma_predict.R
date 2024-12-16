##### AHLS: TIME SERIES PREDICTION #####
### Last Update: 2/5/2024

# Load packages
library(tibble)
library(lubridate)
library(readxl)
library(distr)
library(dplyr)
library(fpp3)
library(feasts)
library(seasonal)
library(ggplot2)

# Read in time series with asthma and AQI
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS"
asthma_ts <- readRDS(paste(dir, "asthma_ts.rds")) %>%
  select(WeekDate, AllAdmissions, aqi)
asthma_ts
c(min(asthma_ts$WeekDate), max(asthma_ts$WeekDate))

# Read in function that makes predictions
source("ARIMA functions/ts_fit.R")


# Test asthma prediction function -----------------------------------------

### 'lookahead' method

# Subset to only one year
asthma_ts_1yr <- asthma_ts %>%
  filter(year(WeekDate) == 2021)

# Read in function that makes predictions
source("ARIMA functions/ts_fit.R")

# Predict asthma admissions for the first 5 weeks of 2022
asthma_2022_predict <- ts_fit(asthma_ts_1yr, lookahead = 5)
# asthma_2022_predict <- ts_fit(asthma_ts_1yr, lookahead = 5, ts_keep = TRUE)
asthma_2022_predict

# Merge predictions with actual observations
asthma_2022_1yr <- left_join(asthma_2022_predict, asthma_ts, by="WeekDate")
asthma_2022_1yr

# Request 95% confidence intervals with predictions
asthma_2022_predict <- ts_fit(asthma_ts_1yr, lookahead = 5, level=95)
asthma_2022_predict

# Unpack confidence interval bands
asthma_2022_predict <- asthma_2022_predict %>%
  unpack_hilo(c("95%"))
asthma_2022_predict

#####

### Test set method
source("ARIMA functions/ts_fit.R")

# Fit asthma admissions data
asthma_ts <- readRDS(paste(dir, "asthma_ts.rds")) %>%
  select(WeekDate, AllAdmissions, aqi) %>%
  filter(year(WeekDate)<=2022 & year(WeekDate)>=2016) %>%
  filter(ymd(WeekDate)<=ymd("2022-03-31"))
asthma_ts
tail(asthma_ts)
c(min(asthma_ts$WeekDate), max(asthma_ts$WeekDate))

# Plot the time series 
asthma_ts %>%
  autoplot(AllAdmissions, color="#00b5d1", lwd=0.8)+
  theme_bw()+
  labs(title="Training Set", y="# of Admissions", x="Week",
       subtitle="January 1, 2016 - March 31, 2022")

# Subset next 13 weeks of 2022
asthma_test <- readRDS(paste(dir, "asthma_ts.rds")) %>%
  select(WeekDate, AllAdmissions, aqi) %>%
  filter(ymd(WeekDate)>=ymd("2022-04-01") & ymd(WeekDate)<=ymd("2022-06-30"))
asthma_test
tail(asthma_test)

# Plot the test time series 
asthma_test %>%
  autoplot(AllAdmissions, color="#4600d1", lwd=0.8)+
  theme_bw()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(title="Test Set", y="# of Admissions", x="Week",
       subtitle="April 1, 2022 - June 30, 2023")

# Predict asthma admissions for the entire test set
asthma_test_predict <- ts_fit(ts_train = asthma_ts,
                              ts_test = asthma_test)
asthma_test_predict

# Predict asthma admissions for the first three weeks of test set
asthma_test_predict <- ts_fit(ts_train = asthma_ts,
                              ts_test = asthma_test,
                              lookahead = 3)
asthma_test_predict

# Request 95% confidence intervals with the predictions
asthma_test_predict <- ts_fit(ts_train = asthma_ts,
                              ts_test = asthma_test,
                              lookahead = 3, level=90)
asthma_test_predict


# Predict on future weeks with 80% and 95% confidence intervals
ts_predict <- ts_fit(ts_train = asthma_ts, ts_test = asthma_test,
                     level = c(80,95), ts_keep = FALSE)
ts_predict

# Unpack confidence interval bands
ts_predict2 <- ts_predict %>%
  unpack_hilo(c("80%","95%"))
ts_predict2

# Plot the predictions
asthma_ts %>%
  filter(WeekDate>=date("2016-01-01") & WeekDate<=date("2022-06-30")) %>%
  ggplot()+
  geom_line(aes(x=WeekDate, y=AllAdmissions, color="#00b5d1"), lwd=0.8, lty="solid")+
  geom_line(data=asthma_test, aes(x=WeekDate, y=AllAdmissions, color="#4600d1"), lwd=0.8, lty="solid")+
  geom_ribbon(data=ts_predict2, aes(x=WeekDate, ymin=`80%_lower`, ymax=`80%_upper`),
              fill="grey40", alpha=0.3)+
  geom_ribbon(data=ts_predict2, aes(x=WeekDate, ymin=`95%_lower`, ymax=`95%_upper`),
              fill="grey40", alpha=0.15)+
  geom_line(data=ts_predict2, aes(x=WeekDate, y=Predict, color="black"), lwd=0.8)+
  scale_color_manual(name="", values=c("#00b5d1","#4600d1","black"),
                     labels=c("Observed","Actual","Predict"))+
  theme_bw()+
  scale_x_date(date_labels = "%Y")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=12),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Forecast Patients Admitted for Asthma",
          subtitle="January 1, 2016 - June 30, 2022")

# Closer look
asthma_ts %>%
  filter(WeekDate>=date("2021-01-01") & WeekDate<=date("2022-06-30")) %>%
  ggplot()+
  geom_line(aes(x=WeekDate, y=AllAdmissions, color="#00b5d1"), lwd=0.8, lty="solid")+
  geom_line(data=asthma_test, aes(x=WeekDate, y=AllAdmissions, color="#4600d1"), lwd=0.8, lty="solid")+
  geom_ribbon(data=ts_predict2, aes(x=WeekDate, ymin=`80%_lower`, ymax=`80%_upper`),
              fill="grey40", alpha=0.3)+
  geom_ribbon(data=ts_predict2, aes(x=WeekDate, ymin=`95%_lower`, ymax=`95%_upper`),
              fill="grey40", alpha=0.15)+
  geom_line(data=ts_predict2, aes(x=WeekDate, y=Predict, color="black"), lwd=0.8)+
  scale_color_manual(name="", values=c("#00b5d1","#4600d1","black"),
                     labels=c("Observed","Actual","Predict"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=12),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Forecast Patients Admitted for Asthma",
          subtitle="January 1, 2021 - June 30, 2023")
