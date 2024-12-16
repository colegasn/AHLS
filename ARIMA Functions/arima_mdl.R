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


# ARIMA Model -------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma %>%
  filter(Day<"2022-01-01")
asthma_test <- asthma %>%
  filter(Day>="2022-01-01")

# Examine time plot, ACF and PACF of original series
asthma_train |>
  gg_tsdisplay(AllAdmissions, plot_type = 'partial')

# Test if seasonal differencing is needed
asthma_train |>
  features(AllAdmissions, unitroot_nsdiffs) # no seasonal difference needed

# Test if differencing is needed
asthma_train |>
  features(AllAdmissions, unitroot_ndiffs) # differencing required

# Examine time plot, ACF and PACF of differenced series
asthma_train |>
  gg_tsdisplay(difference(AllAdmissions), plot_type = 'partial')

# Fit ARIMA models
asthma_mdl <- asthma_train |>
  model(arima012102=ARIMA(AllAdmissions ~ 1+pdq(0,1,2)+PDQ(1,0,2)),
        arima013102=ARIMA(AllAdmissions ~ 1+pdq(0,1,3)+PDQ(1,0,2)),
        search=ARIMA(AllAdmissions, stepwise = FALSE))
asthma_mdl
glance(asthma_mdl)

# Examine residuals
asthma_mdl |>
  select(arima012102) |>
  gg_tsresiduals()

# Test if residuals follows a white noise process
augment(asthma_mdl) |>
  filter(.model == "arima012102") |>
  features(.innov, ljung_box, lag = 36, dof = 5)

# Report the values of the best ARIMA model
report(asthma_mdl |> select(arima012102))

###

# Forecast the next 7 days using best ARIMA model
ci.level=0.95
hilo.name <- as.name(paste(100*ci.level, "%", sep=""))
ts_next <- forecast(asthma_mdl |> select(arima012102), h=7) |>
  hilo(level=100*ci.level) |>
  rename(CI=all_of(hilo.name)) |>
  mutate(Lower=CI$lower, Upper=CI$upper) |>
  rename(Dist=AllAdmissions, Predict=.mean) |>
  select(Day, Predict, Lower, Upper)
ts_next

# Function to Predict Admissions ------------------------------------------
source("ARIMA Functions/asthma_fit.R")

# Split into train and test data
asthma_train <- asthma %>%
  filter(Day<"2022-01-01")
asthma_test <- asthma %>%
  filter(Day>="2022-01-01")

#####
# Test the function using a specified ARIMA model
my_mdl <- asthma_train |>
  model(arima102200=ARIMA(AllAdmissions ~ pdq(1,0,2)+PDQ(2,0,0)))
report(my_mdl)
asthma_fit(ts_data = asthma_train, lookahead = 5,
           p=1, d=0, q=2, P=2, D=0, Q=0)

# Test the function using a different, specified ARIMA model
my_mdl2 <- asthma_train |>
  model(arima200=ARIMA(AllAdmissions ~ pdq(2,0,0)))
report(my_mdl2)
asthma_fit(ts_data = asthma_train, lookahead = 5,
           p=2, d=0, q=0, P=0, D=0, Q=0)

# Test the function using the best ARIMA model above
my_mdl3 <- asthma_train |>
  model(arima012102=ARIMA(AllAdmissions ~ pdq(0,1,2)+PDQ(1,0,2)))
report(my_mdl3)
asthma_fit(ts_data = asthma_train, lookahead = 7,
           p=0, d=1, q=2, P=1, D=0, Q=2)
#####

# Test the function by forecasting next 7 days
asthma_fit(ts_data = asthma_train, lookahead = 7, ci.level = 0.90)


# Forecast Test Data (Lookahead) ------------------------------------------

# Number of days to forecast in test data
nrow(asthma_test)

# Forecast using ARIMA function at end of 2021
future_ts <- asthma_fit(ts_data=asthma_train, lookahead = nrow(asthma_test))

# Merge with observed admissions
asthma_m <- inner_join(asthma, future_ts, by="Day") %>%
  select(Day, AllAdmissions, Predict, Lower, Upper)
asthma_m

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
# saveRDS(asthma_m, paste(dir, "asthma_m.rds", sep=""))

# Read back in rolling predictions
asthma_m <- readRDS(paste(dir, "asthma_m.rds", sep="")) %>%
  tsibble(index=Day)

# Calculate MAPE
median(abs(asthma_m$AllAdmissions-asthma_m$Predict)/asthma_m$AllAdmissions)

# Calculate MSE
sum((asthma_m$AllAdmissions - asthma_m$Predict)^2)/nrow(asthma_m)

# Predict Roll Function ---------------------------------------------------
source("ARIMA Functions/asthma_fit.R")
source("ARIMA Functions/roll_predict.R")

# Test the function by forecasting next 7 days
roll_predict(ts_data = asthma, start_date = "2022-01-01", iter = 7, ci.level = 0.90)


# Forecast Test Data (Rolling) --------------------------------------------

# Number of days to forecast in test data
nrow(asthma_test)

# Forecast by updating ARIMA model each day (365-day window)
future_ts2 <- roll_predict(ts_data=asthma, start_date = "2022-01-01", iter = nrow(asthma_test))

# Merge with observed admissions
asthma_m2 <- inner_join(asthma, future_ts2, by="Day") %>%
  select(Day, AllAdmissions, Predict, Lower, Upper)
asthma_m2
tail(asthma_m2)

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
# saveRDS(asthma_m2, paste(dir, "asthma_m2.rds", sep=""))

# Read back in rolling predictions
asthma_m2 <- readRDS(paste(dir, "asthma_m2.rds", sep="")) %>%
  tsibble(index=Day) %>%
  fill_gaps()
asthma_m2

# Calculate MAPE
median(abs(asthma_m2$AllAdmissions-asthma_m2$Predict)/asthma_m2$AllAdmissions, na.rm=TRUE)

