##### AHLS: TIME SERIES PREDICTION #####
### Last Update: 10/19/2023

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
source("ts_fit.R")

# Read in function that makes rolling predictions
source("ts_roll.R")

# Read in function that makes rolling predictions with refit
source("ts_refit.R")


# Data Setup --------------------------------------------------------------

# Subset to only one year
asthma_ts_1yr <- asthma_ts %>%
  filter(year(WeekDate) == 2021)

# Subset first 5 weeks of 2022
asthma_ts_2022 <- asthma_ts %>%
  filter(year(WeekDate) == 2022) %>%
  slice(1:5)


# Test rolling prediction function ----------------------------------------

# Read in function that makes rolling predictions
source("ts_roll.R")

# Predict rolling asthma admissions for the first 5 weeks of 2022
asthma_2022_roll <- ts_roll(ts_train = asthma_ts_1yr,
                            ts_test = asthma_ts_2022)

#####

# For debugging purposes - train: all 2021 obs. / test: first five wks of 2022
ts_train <- asthma_ts_1yr
ts_test <- asthma_ts_2022

####

# Initial train and test set
i <- 1
ts_train_i <- ts_train
ts_train_i
tail(ts_train_i)

# Select the week to make prediction
ts_test_i <- ts_test %>%
  slice(i)
ts_test_i

# Get next prediction(s) based on specified model
ts_new <- ts_fit(ts_train = ts_train_i,
                 ts_test = ts_test_i)
ts_new

# Save prediction at each iteration
ts_predict <- ts_new
  
# Remove for next iteration
rm(ts_train_i, ts_test_i, ts_new)
  
# Go to next iteration
i <- i+1

# Add weeks to train set based on iteration
ts_train_i <- ts_train %>%
  add_row(slice(ts_test, 1:(i-1)))
ts_train_i
tail(ts_train_i)

# Select the week to make prediction
ts_test_i <- ts_test %>%
  slice(i)
ts_test_i

# Get next prediction(s) based on specified model
ts_new <- ts_fit(ts_train = ts_train_i,
                 ts_test = ts_test_i)
ts_new  # Updating training set breaks ts_fit???

# Select the week to make prediction
ts_test_i <- ts_test %>%
  slice(i-1)
ts_test_i

# Get next prediction(s) based on specified model
ts_new <- ts_fit(ts_train = ts_train_i,
                 ts_test = ts_test_i)
ts_new  # This contradicts the earlier error message???

################################################################################

# Read in functions
source("ts_fit.R")
source("ts_roll.R")

# For debugging purposes - train: all 2021 obs. / test: first five wks of 2022
ts_train <- asthma_ts %>%
  filter(year(WeekDate) == 2021)
ts_test <- asthma_ts %>%
  filter(year(WeekDate) == 2022) %>%
  slice(1)
ts_train
tail(ts_train)
ts_test

ts_new <- ts_fit(ts_train = ts_train, ts_test = ts_test)
ts_new

# Now modify the training set and make prediction for the following week
ts_train <- asthma_ts %>%
  filter(year(WeekDate) == 2021) %>%
  add_row((slice(ts_test, 1)))
ts_test <- asthma_ts %>%
  filter(year(WeekDate) == 2022) %>%
  slice(2)
ts_train
tail(ts_train)
ts_test

ts_new <- ts_fit(ts_train = ts_train, ts_test = ts_test)
ts_new

################################################################################

# For debugging purposes - train: all 2021 obs. / test: first weeks of 2022
ts_train <- asthma_ts
ts_train <- asthma_ts %>%
  filter(year(WeekDate) == 2021)
ts_test <- asthma_ts %>%
  filter(year(WeekDate) == 2022) %>%
  slice(1:13)

# Fit a default ARIMA model
ts_mdl <- ts_train |>
  model(search=ARIMA(AllAdmissions, stepwise = FALSE))

# Refit based on new data
ts_refit <- refit(ts_mdl, new_data = ts_test, reestimate = TRUE)

# Make predictions
ts_next <- fitted(ts_refit)
ts_next

# Merge with test data
ts_predict <- left_join(ts_next, ts_test, by="WeekDate") %>%
  rename(Predict=.fitted) %>%
  select(-.model) %>%
  relocate(WeekDate, Predict)
ts_predict

# Merge with training data
ts_predict2 <- bind_rows(ts_train, ts_predict)
ts_predict2
tail(ts_predict2, n=20)


# Test the ts_refit.R function --------------------------------------------

# For debugging purposes - train: all 2021 obs. / test: first weeks of 2022
ts_2021 <- asthma_ts %>%
  filter(year(WeekDate) == 2021)
ts_2021
ts_2022 <- asthma_ts %>%
  filter(year(WeekDate) == 2022) %>%
  slice(1:13)
ts_2022

# Read in function that makes rolling predictions with refit
source("ts_refit.R")

# Get rolling predictions for first 13 weeks of 2022 using all weekly 2021 data
ts_predict <- ts_refit(ts_train = ts_2021, ts_test = ts_2022,
                       reestimate = TRUE, ts_keep = FALSE)
ts_predict

# Again, but do not update model each week
ts_predict2 <- ts_refit(ts_train = ts_2021, ts_test = ts_2022,
                       reestimate = FALSE, ts_keep = FALSE)
ts_predict2

# Compare the two predictions
ts_compare <- ts_predict %>%
  rename(Predict.Update=Predict) %>%
  left_join(ts_predict2) %>%
  rename(Predict.Frozen=Predict) %>%
  relocate("WeekDate","AllAdmissions","Predict.Update","Predict.Frozen")
ts_compare

# MAPE comparison
MAPE.Update <- 100/nrow(ts_compare)*sum(abs(ts_compare$AllAdmissions-ts_compare$Predict.Update)/ts_compare$AllAdmissions)
MAPE.Frozen <- 100/nrow(ts_compare)*sum(abs(ts_compare$AllAdmissions-ts_compare$Predict.Frozen)/ts_compare$AllAdmissions)
MAPE.Update
MAPE.Frozen


# All Asthma Admissions ---------------------------------------------------

# Read in time series with asthma and AQI
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS"
asthma_ts <- readRDS(paste(dir, "asthma_ts.rds")) %>%
  select(WeekDate, AllAdmissions, aqi)
asthma_ts
c(min(asthma_ts$WeekDate), max(asthma_ts$WeekDate))

# Read in function that makes rolling predictions with refit
source("ts_refit.R")

# All 2016 - 2022 weekly admissions
ts_data <- asthma_ts %>%
  filter(year(WeekDate)>=2016 & year(WeekDate)<=2022) %>%
  filter(ymd(WeekDate)<=ymd("2022-04-01"))
ts_data
tail(ts_data)

# Plot the time series 
asthma_ts %>%
  autoplot(AllAdmissions, color="#00b5d1", lwd=0.8)+
  theme_bw()+
  labs(title="Training Set", y="# of Admissions", x="Week",
       subtitle="January 1, 2016 - March 31, 2022")

# 2022 weekly admissions
ts_2022 <- asthma_ts %>%
  filter(ymd(WeekDate) >= ymd("2022-04-01"))
ts_2022
tail(ts_2022)

# Plot the test time series 
ts_2022 %>%
  autoplot(AllAdmissions, color="#4600d1", lwd=0.8)+
  theme_bw()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(title="Test Set", y="# of Admissions", x="Week",
       subtitle="April 1, 2022 - September 30, 2022")

# Roll predict through 9-30-2022 using all asthma data
ts_predict <- ts_refit(ts_train = ts_data, ts_test = ts_2022,
                       reestimate = TRUE, ts_keep = FALSE)
ts_predict

# Determine if the forecast is higher or lower than previous week
ts_predict <- ts_predict %>%
  mutate(Change=ifelse(Predict < lag(AllAdmissions, n=1), "LOWER",
                         ifelse(Predict > lag(AllAdmissions, n=1), "HIGHER", "SAME"))) %>%
  mutate(LastWk=lag(AllAdmissions, n=1)) %>%
  rename(Actual=AllAdmissions) %>%
  relocate(WeekDate, LastWk, Predict, Change, Actual) %>%
  mutate(Correct=ifelse(Actual < LastWk & Change=="LOWER", 1,
                        ifelse(Actual > LastWk & Change=="HIGHER", 1,
                               ifelse(Actual==LastWk & Change=="SAME", 1, 0)))) %>%
  mutate(Change.P=Predict-LastWk, Change.A=Actual-LastWk)
print(ts_predict %>% select(-aqi), n=30)

# Count number of incorrect forecast directions
sum(ts_predict$Correct, na.rm = TRUE)
sum(ts_predict$Correct, na.rm = TRUE)/nrow(ts_predict)

# Count number of predicted increases and decreases
sum(ifelse(ts_predict$Change=="HIGHER",1,0), na.rm=TRUE)
sum(ifelse(ts_predict$Change=="LOWER",1,0), na.rm=TRUE)

# Count number of actual increases and decreases
sum(ifelse(ts_predict$Actual-ts_predict$LastWk>0,1,0), na.rm=TRUE)
sum(ifelse(ts_predict$Actual-ts_predict$LastWk<0,1,0), na.rm=TRUE)

###

# Mean Error
(1/nrow(ts_predict))*(sum(ts_predict$Actual-ts_predict$Predict))

# Mean Absolute Error
(1/nrow(ts_predict))*(sum(abs(ts_predict$Actual-ts_predict$Predict)))

# Mean Squared Error
(1/nrow(ts_predict))*(sum((ts_predict$Actual-ts_predict$Predict)^2))

# Median Error
median(ts_predict$Actual-ts_predict$Predict)

# Median Absolute Error
median(abs(ts_predict$Actual-ts_predict$Predict))

# Median Absolute Percentage Error
100/nrow(ts_predict)*sum(abs(ts_predict$Actual-ts_predict$Predict)/ts_predict$Actual)

#####

# Plot time series with rolling predictions
asthma_ts %>%
  filter(WeekDate>=date("2016-01-01") & WeekDate<=date("2022-04-01")) %>%
  autoplot(AllAdmissions, aes(color="#00b5d1"), lwd=0.8, lty="solid")+
  geom_line(data=filter(asthma_ts, WeekDate>date("2022-04-01") & WeekDate<=date("2022-09-30")),
            aes(y=AllAdmissions, color="#4600d1"), lwd=0.8, lty="solid", alpha=0.7)+
  geom_line(data=ts_predict, aes(y=Predict, color="black"), lwd=0.8)+
  #geom_vline(xintercept=as.numeric(as.Date("2022-01-02")),color="black", lty="dotted", lwd=0.8)+
  scale_color_manual(name="", values=c("#00b5d1","#4600d1","black"),
                     labels=c("Observed","Actual","Predict"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %d")+
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
  ggtitle("All Patients Admitted for Asthma - Forecast",
          subtitle="Jan 1, 2016 - Sep 30, 2022")

# Closer look (from October 2021 on)
asthma_ts %>%
  filter(WeekDate>=date("2021-10-01") & WeekDate<=date("2022-04-01")) %>%
  autoplot(AllAdmissions, aes(color="#00b5d1"), lwd=0.8, lty="solid")+
  geom_line(data=filter(asthma_ts, WeekDate>date("2022-04-01") & WeekDate<=date("2022-09-30")),
            aes(y=AllAdmissions, color="#4600d1"), lwd=0.8, lty="solid", alpha=0.7)+
  geom_line(data=ts_predict, aes(y=Predict, color="black"), lwd=0.8)+
  #geom_vline(xintercept=as.numeric(as.Date("2021-12-26")),color="black", lty="dotted", lwd=0.8)+
  #geom_vline(xintercept=as.numeric(as.Date("2022-01-02")),color="black", lty="dotted", lwd=0.8)+
  scale_color_manual(name="", values=c("#00b5d1","#4600d1","black"),
                     labels=c("Observed","Actual","Predicted"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %y")+
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
  ggtitle("All Patients Admitted for Asthma - Forecast",
          subtitle="Oct 1, 2021 - Sep 30, 2022")

# Shift predictions one week back
ts_predict2 <- ts_predict %>%
  mutate(Lead.Predict=lead(Predict, n=1))
ts_predict2

# Reaction plot
asthma_ts %>%
  filter(WeekDate>=date("2021-10-01") & WeekDate<=date("2022-04-01")) %>%
  autoplot(AllAdmissions, aes(color="#00b5d1"), lwd=0.8, lty="solid")+
  geom_line(data=filter(asthma_ts, WeekDate>date("2022-04-01") & WeekDate<=date("2022-09-30")),
            aes(y=AllAdmissions, color="#4600d1"), lwd=0.8, lty="solid", alpha=0.7)+
  geom_line(data=ts_predict2, aes(y=Lead.Predict, color="grey40"), lwd=0.8)+
  #geom_vline(xintercept=as.numeric(as.Date("2021-12-26")),color="black", lty="dotted", lwd=0.8)+
  #geom_vline(xintercept=as.numeric(as.Date("2022-01-02")),color="black", lty="dotted", lwd=0.8)+
  scale_color_manual(name="", values=c("#00b5d1","#4600d1","grey40"),
                     labels=c("Observed","Actual","Reaction"))+
  theme_bw()+
  scale_x_date(date_labels = "%b %y")+
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
  ggtitle("All Patients Admitted for Asthma - Prediction Reaction",
          subtitle="Oct 1, 2021 - Sep 30, 2022")
