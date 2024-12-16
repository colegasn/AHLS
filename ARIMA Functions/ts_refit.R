ts_refit <- function(ts_train, ts_test, reestimate = TRUE, ts_keep=FALSE){
  # Load package if not loaded
  library(dplyr)
  library(fpp3)
  library(feasts)
  library(seasonal)
  
  #####
  
  # Find an ARIMA model for the training data
  ts_mdl <- ts_train |>
    model(search=ARIMA(AllAdmissions, stepwise = FALSE))
  
  # Refit the model based on test data
  ts_refit <- refit(ts_mdl, new_data = ts_test,
                    reestimate = reestimate)
  
  # Obtain predictions based on new test data
  ts_next <- fitted(ts_refit)
  ts_next
  
  # Merge with test data
  ts_predict <- left_join(ts_next, ts_test, by="WeekDate") %>%
    rename(Predict=.fitted) %>%
    select(-.model) %>%
    relocate(WeekDate, Predict)
  ts_predict
  
  # Optionally merge with train data
  if(ts_keep==TRUE){
    ts_predict <-  bind_rows(ts_train, ts_predict)
  }
  
  # Return the iterated time series
  return(ts_predict)
}