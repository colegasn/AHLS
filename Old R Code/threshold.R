##### THRESHOLD COMPARISONS #####
### Last Update: 8/22/2024

# Load packages
library(readxl)
library(prophet)
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(fable)
library(ggplot2)
library(tidyr)


# Directory where predictions are saved
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"

# Predictions from ARIMA model
arima_pred <- readRDS(paste(dir, "asthma_m.rds", sep="")) %>%
  tsibble(index=Day) %>%
  fill_gaps() %>%
  rename(y=AllAdmissions, arima.yhat=Predict, arima.yhat_lower=Lower, arima.yhat_upper=Upper)

# Predictions from SES model
arima_ets <- readRDS(paste(dir, "asthma_ets.rds", sep="")) %>%
  tsibble(index=Day) %>%
  fill_gaps() %>%
  rename(y=AllAdmissions, ets.yhat=Predict, ets.yhat_lower=Lower, ets.yhat_upper=Upper)

# Predictions from Prophet model
dir2 <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Prophet Functions/"
proph_pred <- readRDS(paste(dir2, "prophet_pred.rds", sep="")) %>%
  select(ds, y, yhat, yhat_lower, yhat_upper) %>%
  rename(proph.yhat=yhat, proph.yhat_lower=yhat_lower, proph.yhat_upper=yhat_upper)

# Merge into one dataframe
asthma_predict <- inner_join(arima_pred, arima_ets, by="Day") %>%
  inner_join(proph_pred, by=c("Day"="ds")) %>%
  select(-y.x, -y.y) %>%
  relocate(Day, y)
asthma_predict
tail(asthma_predict)


# Round number of admissions by predictions
asthma_p <- asthma_predict %>%
  mutate(across(where(is.numeric), round))
asthma_p

# Prediction Accuracy Comparisons -----------------------------------------

### Median Absolute Percentage Error (MAPE)
# ARIMA
median(abs(asthma_predict$y - asthma_predict$arima.yhat)/asthma_predict$y, na.rm=TRUE)

# ETS
median(abs(asthma_predict$y - asthma_predict$ets.yhat)/asthma_predict$y, na.rm=TRUE)

# Prophet
median(abs(asthma_predict$y - asthma_predict$proph.yhat)/asthma_predict$y, na.rm=TRUE)


### Mean Square Error (MSE)
# ARIMA
sum((asthma_predict$y - asthma_predict$arima.yhat)^2, na.rm = TRUE)/nrow(asthma_predict)

# ETS
sum((asthma_predict$y - asthma_predict$ets.yhat)^2, na.rm = TRUE)/nrow(asthma_predict)

# Prophet
sum((asthma_predict$y - asthma_predict$proph.yhat)^2, na.rm = TRUE)/nrow(asthma_predict)


# Thresholds --------------------------------------------------------------

# Identify top percentile of admissions
thres <- 5
asthma_thres <- asthma_predict %>%
  mutate(arima.status=factor(ifelse(arima.yhat_upper >= thres, "HIGH", "NORMAL"),
                             levels=c("NORMAL","HIGH")),
         ets.status=factor(ifelse(ets.yhat_upper >= thres, "HIGH", "NORMAL"),
                             levels=c("NORMAL","HIGH")),
         proph.status=factor(ifelse(proph.yhat_upper >= thres, "HIGH", "NORMAL"),
                             levels=c("NORMAL","HIGH")),
         actual.stats=factor(ifelse(y >= thres, "HIGH", "NORMAL")))

# How many days is the top percentile reached?
asthma_thres %>%
  count(arima.status, name="Count")
asthma_thres %>%
  count(ets.status, name="Count")
asthma_thres %>%
  count(proph.status, name="Count")


# Proportion of HIGH Days -------------------------------------------------

# Set risk threshold
p <- 0.05

# Get number of admissions that meet or exceed the risk threshold
arima.top <- asthma_predict %>%
  arrange(desc(arima.yhat_upper)) %>%
  top_frac(n=p) %>%
  select(Day, y, arima.yhat, arima.yhat_upper)
ets.top <- asthma_predict %>%
  arrange(desc(ets.yhat_upper)) %>%
  top_frac(n=p) %>%
  select(Day, y, ets.yhat, ets.yhat_upper)
proph.top <- asthma_predict %>%
  arrange(desc(proph.yhat_upper)) %>%
  top_frac(n=p) %>%
  select(Day, y, proph.yhat, proph.yhat_upper)

# Lowest benchmark for each model
tail(arima.top)
tail(ets.top)
tail(proph.top)

# Set benchmark to classify HIGH admission days
arima.thres <- min(arima.top$arima.yhat_upper)
ets.thres <- min(ets.top$ets.yhat_upper)
proph.thres <- min(proph.top$proph.yhat_upper)
actual.thres <- 5

arima.thres
ets.thres
proph.thres
actual.thres

# Classify whether the day was HIGH or NORMAL
asthma_status <- asthma_predict %>%
  mutate(arima.status = ifelse(arima.yhat_upper > arima.thres, "HIGH", "NORMAL"),
         ets.status = ifelse(ets.yhat_upper > ets.thres, "HIGH", "NORMAL"),
         proph.status = ifelse(proph.yhat_upper > proph.thres, "HIGH", "NORMAL"),
         actual.status = ifelse(y > actual.thres, "HIGH", "NORMAL")) %>%
  select(Day, y, arima.status, ets.status, proph.status, actual.status)
asthma_status

# Proportion of days where actual hospitalizations were HIGH
asthma_status %>%
  count(actual.status) %>%
  mutate(pct=n/sum(n))

# ARIMA
asthma_status %>%
  count(arima.status, actual.status) %>%
  arrange(actual.status)

# ETS
asthma_status %>%
  count(ets.status, actual.status) %>%
  arrange(actual.status)

# Prophet
asthma_status %>%
  count(proph.status, actual.status) %>%
  arrange(actual.status)

# Compare outcomes
asthma_status %>%
  count(arima.status, ets.status, proph.status, actual.status) %>%
  mutate(pct=n/sum(n)) %>%
  relocate(actual.status) %>%
  arrange(desc(n))

# Calculate misclassifications
asthma.c <- asthma_status %>%
  mutate(arima.c=ifelse(actual.status==arima.status, "CORRECT", "INCORRECT"),
         ets.c=ifelse(actual.status==ets.status, "CORRECT", "INCORRECT"),
         proph.c=ifelse(actual.status==proph.status, "CORRECT", "INCORRECT"))
asthma.c %>%
  count(arima.c, ets.c, proph.c)

# Misclassification rates
asthma.c %>%
  count(arima.c) %>%
  mutate(pct=n/sum(n))

asthma.c %>%
  count(ets.c) %>%
  mutate(pct=n/sum(n))

asthma.c %>%
  count(proph.c) %>%
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
true.pos <- sum(asthma_status$proph.status=="HIGH" & asthma_status$actual.status=="HIGH")
pos <- sum(asthma_status$proph.status=="HIGH")
proph.ppv <- true.pos/pos

c(ARIMA=arima.ppv, ETS=ets.ppv, Prophet=proph.ppv)

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
true.neg <- sum(asthma_status$proph.status=="NORMAL" & asthma_status$actual.status=="NORMAL")
neg <- sum(asthma_status$proph.status=="NORMAL")
proph.npv <- true.neg/neg

c(ARIMA=arima.npv, ETS=ets.npv, Prophet=proph.npv)
