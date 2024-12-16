##### INTRODUCTION TO LSTM (RNN) MODELS ####
### Last Update: 2/6/2024

# Set up tensorflow and keras
# devtools::install_github("rstudio/tensorflow")
# devtools::install_github("rstudio/keras")

# tensorflow::install_tensorflow()
# tensorflow::tf_config()

# Load packages
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(ggplot2)
library(keras)
library(tensorflow)

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
  select(WeekDate, AllAdmissions) %>%
  rename(ds=WeekDate, y=AllAdmissions)
print(asthma_ts, n=15)


# Data Preparation --------------------------------------------------------

# Lag the series
asthma_lag <- asthma_ts %>%
  mutate(y_lag=lag(y,n=1)) %>%
  select(ds, y_lag, y)
asthma_lag

# Split to training and test sets
asthma_train <- asthma_lag %>%
  filter(year(ds) < "2022")
asthma_test <- asthma_lag %>%
  filter(year(ds) >= "2022")

# Check date ranges of training and test sets
c(min(asthma_train$ds), max(asthma_train$ds))
c(min(asthma_test$ds), max(asthma_test$ds))

# Normalize training and test set
asthma_train <- asthma_train %>%
  mutate(y_std=((y - min(y))/(max(y)-min(y))))
asthma_test <- asthma_test %>%
  mutate(y_std=((y - min(asthma_train$y))/(max(asthma_train$y)-min(asthma_train$y))))

# Scaled training and test set
fr_min <- -1; fr_max <- 1
asthma_train <- asthma_train %>%
  mutate(y_scale=y_std*(fr_max-fr_min)+fr_min)
asthma_test <- asthma_test %>%
  mutate(y_scale=y_std*(fr_max-fr_min)+fr_min)

# Examine
head(asthma_train)
tail(asthma_train)
head(asthma_test)
tail(asthma_test)


# Modeling ----------------------------------------------------------------

# Reshape input data to three dimensions
asthma_input <- asthma_train
dim(asthma_input) <- c(length(asthma_input), 1, 1)
asthma_input

m <- keras_model_sequential()
m %>%
  layer_lstm(units=1, batch_input_shape=c(1,asthma_input[2],1), stateful = TRUE) %>%
  layer_dense(units=1)
