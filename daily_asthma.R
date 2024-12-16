##### Asthma Forecasting Algorithms #####
### Last Update: 12/16/2024

# Load packages
library(readxl)
library(prophet)
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(fable)
library(feasts)
library(ggplot2)
library(pROC)

# Load Data ---------------------------------------------------------------

# Read in and format data
asthma <- read_excel("data-raw/asthma.xlsx") |>
  tibble() |>
  mutate(WEEK_NAME=wday(Day, label=TRUE, abbr=TRUE),
         WEEK_NUMBER=epiweek(Day),
         WeekDate=floor_date(Day, unit="week", week_start=7),
         Quarter=quarter(Day, fiscal_start = 7, type="date_first")) |>
  relocate(Quarter, .before=MONTH_NUMBER) |>
  relocate(WEEK_NUMBER, .after=MONTH_NUMBER) |>
  relocate(WEEK_NAME, .after=MONTH_NAME) |>
  relocate(WeekDate, .after=MonthDate)
print(asthma, n=15)

# All asthma admissions
asthma_ts <- asthma |>
  dplyr::select(Day, AllAdmissions) |>
  mutate(Day=ymd(Day)) |>
  rename(ds=Day, y=AllAdmissions) |>
  as_tsibble(index = ds)
asthma_ts

#####

# Distribution of days with admissions: 1/1/2016 to 12/31/2023 (n=2922 days)
asthma_ts |>
  tidyr::drop_na(y) |>
  count(y) |>
  mutate(Prop=n/sum(n), Sum=sum(n))
asthma_ts |>
  tidyr::drop_na(y) |>
  filter(y>=4) |>
  count(y) |>
  mutate(Sum=sum(n))
451/2922
asthma_ts |>
  as_tibble() |>
  tidyr::drop_na(y) |>
  summarise(Min=min(y), Q1=quantile(y, probs=0.25), Med=median(y),
            Q3=quantile(y, probs=0.75), max=max(y), IQR=Q3-Q1)

# Plot the time series for all admissions
asthma_ts |>
  autoplot(y, color="#00b5d1", lwd=0.8)+
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
          subtitle="Jan 1, 2016 - December 31, 2023")

# Calendar heat map
cal <- asthma |>
  group_by(MonthDate) |>
  mutate(WEEK_MONTH=(5+day(Day) + wday(floor_date(Day, 'month'))) %/% 7,
         WEEK_NAME=factor(WEEK_NAME, labels=c("Su","M","Tu","W","Th","F","Sa")),
         MONTH_NAME=factor(MONTH_NAME,
                           levels=c("January","February","March",
                                    "April","May","June","July",
                                    "August","September","October",
                                    "November","December"))) |>
  #filter(year(Day)<2022) |>
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=AllAdmissions))+
  geom_tile(color="black")+
  facet_grid(year(Day) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_gradient(low="white", high="blue", na.value="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "grey85"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey30", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=11, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "none")+
  xlab("")+ylab("")+
  ggtitle("Daily Asthma Hospitalizations")+
  labs(fill="Admissions")
cal


# 1. ARIMA ----------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<"2022-01-01")
asthma_test <- asthma_ts |>
  filter(ds>="2022-01-01")


# 1.1 ARIMA Model Formulation ---------------------------------------------

# Examine time plot, ACF and PACF of original series
asthma_train |>
  gg_tsdisplay(y, plot_type = 'partial')

# Test if seasonal differencing is needed
asthma_train |>
  features(y, unitroot_nsdiffs) # no seasonal difference needed

# Test if differencing is needed
asthma_train |>
  features(y, unitroot_ndiffs) # differencing required

# Examine time plot, ACF and PACF of differenced series
asthma_train |>
  gg_tsdisplay(difference(y), plot_type = 'partial')

# Fit ARIMA models
asthma_mdl <- asthma_train |>
  model(arima012002=ARIMA(y ~ 1+pdq(0,1,2)+PDQ(0,0,2)),
        arima013002=ARIMA(y ~ 1+pdq(0,1,3)+PDQ(0,0,2)),
        arima012001=ARIMA(y ~ 1+pdq(0,1,2)+PDQ(0,0,1)),
        arima012=ARIMA(y ~ 1+pdq(0,1,2)),
        search=ARIMA(y, stepwise = FALSE))
asthma_mdl
glance(asthma_mdl)

# Examine residuals
asthma_mdl |>
  select(arima012) |>
  gg_tsresiduals()

# Test if residuals follows a white noise process
augment(asthma_mdl) |>
  filter(.model == "arima012") |>
  features(.innov, ljung_box, lag = 36, dof = 3)

# Report the values of the best ARIMA model
report(asthma_mdl |> select(arima012))



# 1.2 ARIMA Prediction ----------------------------------------------------

# Load in functions to do rolling predictions
source("ARIMA Functions/arima_fit.R")
source("ARIMA Functions/roll_arima.R")

# Test the prediction function
test <- arima_fit(ts_data = asthma_train, lookahead = 7, p=0, d=1, q=2, P=0, D=0, Q=0)
print(test, n=7)

# Test the rolling prediction function
test2 <- roll_arima(ts_data = asthma_ts, start_date = "2022-01-01", iter = 7, ci.level = 0.90)
print(test2, n=7)

#####

# Forecast admissions for 2022 and 2023 through rolling prediction
future_ts <- roll_arima(ts_data = asthma_ts, start_date = "2022-01-01", iter = nrow(asthma_test), ci.level = 0.90)

# Merge with observed admissions
arima_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  select(ds, y, Method, Predict, Lower, Upper)
arima_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
saveRDS(arima_predict, paste(dir, "daily_arima_predict.rds", sep=""))

# Read back in rolling predictions
arima_predict <- readRDS(paste(dir, "daily_arima_predict.rds", sep="")) |>
  tsibble(index=ds)


# 2. ETS ------------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<"2022-01-01")
asthma_test <- asthma_ts |>
  filter(ds>="2022-01-01")


# 2.1 ETS Model Formulation -----------------------------------------------

# Fit ETS models
asthma_mdl <- asthma_train |>
  mutate(y=replace_na(y, 0)) |>
  model(SES = ETS(y ~ error("A")+trend("N")+season("N")),
        Holt = ETS(y ~ error("A")+trend("A")+season("N")),
        Damped = ETS(y ~ error("A")+trend("Ad")+season("N")),
        search = ETS(y))
asthma_mdl
glance(asthma_mdl)

# Test accuracy of the ETS models
asthma_mdl |>
  forecast(h = nrow(asthma_test)) |>
  accuracy(asthma_test)

# Estimated parameters of ANA model
ana_mdl <- asthma_train |>
  mutate(y=replace_na(y, 0)) |>
  model(ana = ETS(y ~ error("A")+trend("N")+season("A")))
report(ana_mdl)


# 2.2 ETS Prediction ------------------------------------------------------

# Load in functions to do rolling predictions
source("ETS Functions/ets_fit.R")
source("ETS Functions/roll_ets.R")

# Test the prediction function
test <- ets_fit(ts_data = asthma_train, lookahead = 7, Error = "A", Trend = "N", Season = "A")
print(test, n=7)

# Test the rolling prediction function 
test2 <- roll_ets(ts_data = asthma_ts, start_date = "2022-01-01", iter = 7, ci.level = 0.90)
print(test, n=7)

#####

# Forecast admissions for 2022 and 2023 through rolling prediction
future_ts <- roll_ets(ts_data = asthma_ts, start_date = "2022-01-01", iter = nrow(asthma_test), ci.level = 0.90)

# Merge with observed admissions
ets_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  select(ds, y, Method, Predict, Lower, Upper)
ets_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
saveRDS(ets_predict, paste(dir, "daily_ets_predict.rds", sep=""))

# Read back in rolling predictions
ets_predict <- readRDS(paste(dir, "daily_ets_predict.rds", sep="")) |>
  tsibble(index=ds)



# 3. Prophet --------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<"2022-01-01")
asthma_test <- asthma_ts |>
  filter(ds>="2022-01-01")

# Load in functions to do rolling predictions
source("Prophet Functions/prophet_fit.R")
source("Prophet Functions/roll_prophet.R")

# Test the prediction function
test <- prophet_fit(ts_data = asthma_train, lookahead = 7)
print(test, n=7)

# Test the rolling prediction function
test2 <- roll_prophet(ts_data = asthma_ts, start_date = "2022-01-01", iter = 7, ci.level = 0.90)
print(test2, n=7)

#####

# Forecast admissions for 2022 and 2023 through rolling prediction
future_ts <- roll_prophet(ts_data = asthma_ts, start_date = "2022-01-01", iter = nrow(asthma_test), ci.level = 0.90)

# Merge with observed admissions
prophet_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  select(ds, y, Method, Predict, Lower, Upper)
prophet_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
saveRDS(prophet_predict, paste(dir, "daily_prophet_predict.rds", sep=""))

# Read back in rolling predictions
prophet_predict <- readRDS(paste(dir, "daily_prophet_predict.rds", sep="")) |>
  tsibble(index=ds)


# 4. Cross-Validation -----------------------------------------------------

# Directory where predictions are saved
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"

# Predictions from ARIMA model
arima_predict <- readRDS(paste(dir, "daily_arima_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps() |>
  rename(arima.yhat=Predict, arima.yhat_lower=Lower, arima.yhat_upper=Upper)

# Predictions from ETS model
ets_predict <- readRDS(paste(dir, "daily_ets_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps() |>
  rename(ets.yhat=Predict, ets.yhat_lower=Lower, ets.yhat_upper=Upper)

# Predictions from Prophet model
prophet_predict <- readRDS(paste(dir, "daily_prophet_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps() |>
  rename(prophet.yhat=Predict, prophet.yhat_lower=Lower, prophet.yhat_upper=Upper)

# Merge into one dataframe
asthma_predict <- arima_predict |>
  select(-Method) |>
  inner_join(ets_predict |> select(-Method, -y), by="ds") |>
  inner_join(prophet_predict |> select(-Method, -y), by="ds")
asthma_predict


# 5. Summary Statistics ---------------------------------------------------

# Calculate Absolute Percentage Error for each day and then calculate IQR
# Get the 25th and 75th percentile of percentage error
# Get 95% CI coverage of the predictions (y) - LOWER/COVER/HIGHER
# Change the lookback window - 365, 2*365, 3*365

### Median Absolute Percentage Error

# ARIMA
median(abs(asthma_predict$y - asthma_predict$arima.yhat)/asthma_predict$y, na.rm=TRUE)

# ETS
median(abs(asthma_predict$y - asthma_predict$ets.yhat)/asthma_predict$y, na.rm=TRUE)

# Prophet
median(abs(asthma_predict$y - asthma_predict$prophet.yhat)/asthma_predict$y, na.rm=TRUE)


### Mean Squared Error

# ARIMA
sum((asthma_predict$y-asthma_predict$arima.yhat)^2, na.rm=TRUE)/nrow(asthma_predict)

# ETS
sum((asthma_predict$y-asthma_predict$ets.yhat)^2, na.rm=TRUE)/nrow(asthma_predict)

# Prophet
sum((asthma_predict$y-asthma_predict$prophet.yhat)^2, na.rm=TRUE)/nrow(asthma_predict)


# 6. Thresholds -----------------------------------------------------------

# Set risk threshold
p <- 0.05

# Get number of admissions that meet or exceed the risk threshold
arima.top <- asthma_predict |>
  arrange(desc(arima.yhat_upper)) |>
  top_frac(n=p) |>
  select(ds, y, arima.yhat, arima.yhat_upper)
ets.top <- asthma_predict |>
  arrange(desc(ets.yhat_upper)) |>
  top_frac(n=p) |>
  select(ds, y, ets.yhat, ets.yhat_upper)
prophet.top <- asthma_predict |>
  arrange(desc(prophet.yhat_upper)) |>
  top_frac(n=p) |>
  select(ds, y, prophet.yhat, prophet.yhat_upper)

# Set benchmark to classify HIGH admission days
arima.thres <- min(arima.top$arima.yhat_upper, na.rm=TRUE)
ets.thres <- min(ets.top$ets.yhat_upper, na.rm=TRUE)
prophet.thres <- min(prophet.top$prophet.yhat_upper, na.rm=TRUE)
actual.thres <- 5

arima.thres
ets.thres
prophet.thres

# Classify whether the day was HIGH or NORMAL
asthma_status <- asthma_predict |>
  mutate(arima.status = ifelse(arima.yhat_upper > arima.thres, "HIGH", "NORMAL"),
         ets.status = ifelse(ets.yhat_upper > ets.thres, "HIGH", "NORMAL"),
         prophet.status = ifelse(prophet.yhat_upper > prophet.thres, "HIGH", "NORMAL"),
         actual.status = ifelse(y > actual.thres, "HIGH", "NORMAL")) |>
  select(ds, y, arima.yhat_upper, ets.yhat_upper, prophet.yhat_upper, arima.status, ets.status, prophet.status, actual.status)
asthma_status

### Proportion of days where actual hospitalizations were HIGH
# Actual
asthma_status |>
  count(actual.status) |>
  mutate(pct=n/sum(n))

# ARIMA
asthma_status |>
  count(arima.status, actual.status) |>
  arrange(actual.status)

# ETS
asthma_status |>
  count(ets.status, actual.status) |>
  arrange(actual.status)

# Prophet
asthma_status |>
  count(prophet.status, actual.status) |>
  arrange(actual.status)

# Compare outcomes
asthma_status |>
  count(arima.status, ets.status, prophet.status, actual.status) |>
  mutate(pct=n/sum(n)) |>
  relocate(actual.status) |>
  arrange(desc(n))

# Calculate misclassifications
asthma.c <- asthma_status %>%
  mutate(arima.c=ifelse(actual.status==arima.status, "CORRECT", "INCORRECT"),
         ets.c=ifelse(actual.status==ets.status, "CORRECT", "INCORRECT"),
         prophet.c=ifelse(actual.status==prophet.status, "CORRECT", "INCORRECT"))
asthma.c %>%
  count(arima.c, ets.c, prophet.c)

#### Misclassification rates
# ARIMA
asthma.c %>%
  count(arima.c) %>%
  mutate(pct=n/sum(n))

# ETS
asthma.c %>%
  count(ets.c) %>%
  mutate(pct=n/sum(n))

# Prophet
asthma.c %>%
  count(prophet.c) %>%
  mutate(pct=n/sum(n))

### PPV
# ARIMA
true.pos <- sum(asthma_status$arima.status=="HIGH" & asthma_status$actual.status=="HIGH")
pos <- sum(asthma_status$arima.status=="HIGH")
arima.ppv <- true.pos/pos

# ETS
true.pos <- sum(asthma_status$ets.status=="HIGH" & asthma_status$actual.status=="HIGH")
pos <- sum(asthma_status$ets.status=="HIGH")
ets.ppv <- true.pos/pos

# Prophet
true.pos <- sum(asthma_status$prophet.status=="HIGH" & asthma_status$actual.status=="HIGH")
pos <- sum(asthma_status$prophet.status=="HIGH")
prophet.ppv <- true.pos/pos

c(ARIMA=arima.ppv, ETS=ets.ppv, Prophet=prophet.ppv)

### NPV
# ARIMA
true.neg <- sum(asthma_status$arima.status=="NORMAL" & asthma_status$actual.status=="NORMAL")
neg <- sum(asthma_status$arima.status=="NORMAL")
arima.npv <- true.neg/neg

# ETS
true.neg <- sum(asthma_status$ets.status=="NORMAL" & asthma_status$actual.status=="NORMAL")
neg <- sum(asthma_status$ets.status=="NORMAL")
ets.npv <- true.neg/neg

# Prophet
true.neg <- sum(asthma_status$prophet.status=="NORMAL" & asthma_status$actual.status=="NORMAL")
neg <- sum(asthma_status$prophet.status=="NORMAL")
prophet.npv <- true.neg/neg

c(ARIMA=arima.npv, ETS=ets.npv, Prophet=prophet.npv)


# 7. ROC Curve Analysis ---------------------------------------------------

### ARIMA

# Fit ROC curve
arima.roc <- roc(response=asthma_status$actual.status,
                 predictor=asthma_status$arima.yhat_upper)
arima_roc.stat <- data.frame(Sensitivity=arima.roc$sensitivities,
                             Specificity=arima.roc$specificities,
                             Cutoffs=arima.roc$thresholds) %>%
  mutate(DistSquared=(Sensitivity-1)^2+(Specificity-1)^2) %>%
  tibble()

# Which row has the smallest distance?
arima_roc.stat %>%
  slice_min(DistSquared)

# Plot the ROC curve for the 3-months prediction
plot.roc(arima.roc, col="red", lwd=2.5, print.thres=TRUE,
         identity=TRUE, identity.lwd=1.5,
         identity.lty="dashed", identity.col="black",
         print.auc=TRUE, auc.polygon = TRUE, 
         main="ROC from ARIMA(0,1,2)(1,0,2)[7] (HIGH=5)")

### ETS

# Fit ROC curve
ets.roc <- roc(response=asthma_status$actual.status,
               predictor=asthma_status$ets.yhat_upper)
ets_roc.stat <- data.frame(Sensitivity=ets.roc$sensitivities,
                           Specificity=ets.roc$specificities,
                           Cutoffs=ets.roc$thresholds) %>%
  mutate(DistSquared=(Sensitivity-1)^2+(Specificity-1)^2) %>%
  tibble()

# Which row has the smallest distance?
ets_roc.stat %>%
  slice_min(DistSquared)

# Plot the ROC curve for the 3-months prediction
plot.roc(ets.roc, col="red", lwd=2.5, print.thres=TRUE,
         identity=TRUE, identity.lwd=1.5,
         identity.lty="dashed", identity.col="black",
         print.auc=TRUE, auc.polygon = TRUE, 
         main="ROC from ETS(A,N,A) (HIGH=5)")

### Prophet
prophet.roc <- roc(response=asthma_status$actual.status,
                 predictor=asthma_status$prophet.yhat_upper)
prophet_roc.stat <- data.frame(Sensitivity=prophet.roc$sensitivities,
                             Specificity=prophet.roc$specificities,
                             Cutoffs=prophet.roc$thresholds) %>%
  mutate(DistSquared=(Sensitivity-1)^2+(Specificity-1)^2) %>%
  tibble()

# Which row has the smallest distance?
prophet_roc.stat %>%
  slice_min(DistSquared)

# Plot the ROC curve for the 3-months prediction
plot.roc(prophet.roc, col="red", lwd=2.5, print.thres=TRUE,
         identity=TRUE, identity.lwd=1.5,
         identity.lty="dashed", identity.col="black",
         print.auc=TRUE, auc.polygon = TRUE, 
         main="ROC from Prophet (HIGH=5)")


### Print AUC and 95% CI and SE
# ARIMA
arima.roc$auc
ci.auc(arima.roc)

# ETS
ets.roc$auc
ci.auc(ets.roc)

# Prophet
prophet.roc$auc
ci.auc(prophet.roc)
