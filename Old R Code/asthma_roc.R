##### PROC ANALYSIS OF ASTHMA PREDICTIONS #####
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
library(tidyr)
library(pROC)

# Data Setup --------------------------------------------------------------

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


# Threshold Selection -----------------------------------------------------

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

# Set benchmark to classify HIGH admission days
arima.thres <- min(arima.top$arima.yhat_upper)
ets.thres <- min(ets.top$ets.yhat_upper)
proph.thres <- min(proph.top$proph.yhat_upper)
actual.thres <- 5

arima.thres
ets.thres
proph.thres

# Classify whether the prediction was HIGH or 0
asthma_status <- asthma_predict %>%
  mutate(arima.status = ifelse(arima.yhat_upper > arima.thres, 1, 0),
         ets.status = ifelse(ets.yhat_upper > ets.thres, 1, 0),
         proph.status = ifelse(proph.yhat_upper > proph.thres, 1, 0),
         actual.status = ifelse(y > actual.thres, 1, 0)) %>%
  select(Day, y, actual.status, arima.yhat_upper, arima.status,
         ets.yhat_upper, ets.status, proph.yhat_upper, proph.status)
asthma_status


# pROC --------------------------------------------------------------------

# ARIMA
#####
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

# Print AUC and 95% CI and SE
arima.roc$auc
ci.auc(arima.roc)

# ETS
#####
ets.roc <- roc(response=asthma_status$actual.status,
               predictor=asthma_status$ets.yhat_upper)
ets_roc.stat <- data.frame(Sensitivity=ets.roc$sensitivities,
                             Specificity=ets.roc$specificities,
                             Cutoffs=arima.roc$thresholds) %>%
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

# Print AUC and 95% CI and SE
ets.roc$auc
ci.auc(ets.roc)

# Prophet
#####
proph.roc <- roc(response=asthma_status$actual.status,
                 predictor=asthma_status$proph.yhat_upper)
proph_roc.stat <- data.frame(Sensitivity=proph.roc$sensitivities,
                             Specificity=proph.roc$specificities,
                             Cutoffs=proph.roc$thresholds) %>%
  mutate(DistSquared=(Sensitivity-1)^2+(Specificity-1)^2) %>%
  tibble()

# Which row has the smallest distance?
proph_roc.stat %>%
  slice_min(DistSquared)

# Plot the ROC curve for the 3-months prediction
plot.roc(proph.roc, col="red", lwd=2.5, print.thres=TRUE,
         identity=TRUE, identity.lwd=1.5,
         identity.lty="dashed", identity.col="black",
         print.auc=TRUE, auc.polygon = TRUE, 
         main="ROC from Prophet (HIGH=5)")

# Print AUC and 95% CI and SE
proph.roc$auc
ci.auc(proph.roc)
