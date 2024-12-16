##### AHLS: TIME SERIES ANALYSIS #####
### Last Update: 8/26/2024

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

# Read in time series with asthma admissions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
asthma <- readRDS(paste(dir, "asthma.rds", sep="")) %>%
  tsibble(index=Day) %>%
  fill_gaps()
asthma

#####

# Distribution of days with admissions: 1/1/2016 to 6/30/2023 (n=2738 days)
asthma %>%
  count(AllAdmissions) %>%
  mutate(Prop=n/sum(n))
asthma %>%
  drop_na(AllAdmissions) %>%
  count(AllAdmissions) %>%
  mutate(Prop=n/sum(n))
asthma %>%
  drop_na(AllAdmissions) %>%
  filter(AllAdmissions>=4) %>%
  count(AllAdmissions) %>%
  mutate(Sum=sum(n))
413/2738
asthma %>%
  drop_na(AllAdmissions) %>%
  as_tibble() %>%
  summarise(Min=min(AllAdmissions),
         Q1=quantile(AllAdmissions, probs=0.25),
         Med=median(AllAdmissions),
         Q3=quantile(AllAdmissions, probs=0.75),
         max=max(AllAdmissions),
         IQR=Q3-Q1)
#####

# Plot the time series for all admissions
asthma %>%
  autoplot(AllAdmissions, color="#00b5d1", lwd=0.8)+
  theme_bw()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=12),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank())+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Daily Hospitalizations of Patients Admitted for Asthma",
          subtitle="Jan 1, 2016 - June 30, 2023")


# ETS Model -------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma %>%
  filter(Day<"2022-01-01")
asthma_test <- asthma %>%
  filter(Day>="2022-01-01")

# Fit ETS models
asthma_mdl <- asthma_train |>
  mutate(AllAdmissions=replace_na(AllAdmissions, 0)) |>
  model(SES = ETS(AllAdmissions ~ error("A")+trend("N")+season("N")),
        Holt = ETS(AllAdmissions ~ error("A")+trend("A")+season("N")),
        Damped = ETS(AllAdmissions ~ error("A")+trend("Ad")+season("N")),
        search = ETS(AllAdmissions))
asthma_mdl
glance(asthma_mdl)

# Test accuracy of the ETS models
asthma_mdl |>
  forecast(h = 500) |>
  accuracy(asthma_test)

# Estimated parameters of SES model
ses_mdl <- asthma_train |>
  mutate(AllAdmissions=replace_na(AllAdmissions, 0)) |>
  model(SES = ETS(AllAdmissions ~ error("A")+trend("N")+season("N")))
report(ses_mdl)

# Estimated parameters of Holt model
holt_mdl <- asthma_train |>
  mutate(AllAdmissions=replace_na(AllAdmissions, 0)) |>
  model(Holt = ETS(AllAdmissions ~ error("A")+trend("A")+season("N")))
report(holt_mdl)

# Estimated parameters of Damped model
damped_mdl <- asthma_train |>
  mutate(AllAdmissions=replace_na(AllAdmissions, 0)) |>
  model(Damped = ETS(AllAdmissions ~ error("A")+trend("Ad")+season("N")))
report(damped_mdl)

# Estimated parameters of best searched model
search_mdl <- asthma_train |>
  mutate(AllAdmissions=replace_na(AllAdmissions, 0)) |>
  model(search = ETS(AllAdmissions))
report(search_mdl)

###

# Forecast the next 7 days using best SES model
ci.level=0.95
hilo.name <- as.name(paste(100*ci.level, "%", sep=""))
ts_next <- forecast(search_mdl, h=7) |>
  hilo(level=100*ci.level) |>
  rename(CI=all_of(hilo.name)) |>
  mutate(Lower=CI$lower, Upper=CI$upper) |>
  rename(Dist=AllAdmissions, Predict=.mean) |>
  select(Day, Predict, Lower, Upper)
ts_next

# Forecast the entire test set using best SES model
ci.level=0.95
hilo.name <- as.name(paste(100*ci.level, "%", sep=""))
ts_next <- forecast(search_mdl, h=nrow(asthma_test)) |>
  hilo(level=100*ci.level) |>
  rename(CI=all_of(hilo.name)) |>
  mutate(Lower=CI$lower, Upper=CI$upper) |>
  rename(Dist=AllAdmissions, Predict=.mean) |>
  select(Day, Predict, Lower, Upper)
ts_next

# Merge with observed admissions
asthma_ets <- inner_join(asthma, ts_next, by="Day") %>%
  select(Day, AllAdmissions, Predict, Lower, Upper)
asthma_ets
tail(asthma_ets)

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
# saveRDS(asthma_ets, paste(dir, "asthma_ets.rds", sep=""))

# Read back in rolling predictions
asthma_ets <- readRDS(paste(dir, "asthma_ets.rds", sep="")) %>%
  tsibble(index=Day) %>%
  fill_gaps()
asthma_ets

# Calculate MAPE
median(abs(asthma_ets$AllAdmissions-asthma_ets$Predict)/asthma_ets$AllAdmissions, na.rm=TRUE)

# Calculate MSE
sum((asthma_ets$AllAdmissions-asthma_ets$Predict)^2)/nrow(asthma_ets)


# SES Function to Roll Predict Admissions ----------------------------------
source("ETS Functions/roll_predict2.R")

# Split into train and test data
asthma_train <- asthma %>%
  filter(Day<"2022-01-01") %>%
  mutate(AllAdmissions=replace_na(AllAdmissions, 0))
asthma_test <- asthma %>%
  filter(Day>="2022-01-01")

# Test the function by forecasting next 7 days
roll_predict2(ts_data = asthma, start_date = "2022-01-01", iter = 7, ci.level = 0.90)

# Forecast Test Data (Rolling) --------------------------------------------

# Number of days to forecast in test data
nrow(asthma_test)

# Forecast by updating ARIMA model each day (365-day window)
future_ts <- roll_predict2(ts_data=asthma, start_date = "2022-01-01", iter = nrow(asthma_test))

# Merge with observed admissions
asthma_ets2 <- inner_join(asthma, future_ts, by="Day") %>%
  select(Day, AllAdmissions, Predict, Lower, Upper)
asthma_ets2
tail(asthma_ets2)

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
# saveRDS(asthma_ets2, paste(dir, "asthma_ets2.rds", sep=""))

# Read back in rolling predictions
asthma_ets2 <- readRDS(paste(dir, "asthma_ets2.rds", sep="")) %>%
  tsibble(index=Day) %>%
  fill_gaps()
asthma_ets2

# Calculate MAPE
median(abs(asthma_ets2$AllAdmissions-asthma_ets2$Predict)/asthma_ets2$AllAdmissions, na.rm=TRUE)
