prophet_roll <- function(ts_train, ts_test, n=nrow(ts_test)){
  
  # Check that 'n' is less than test set
  if(n>nrow(ts_test)){
    stop("'n' specified is larger than rows in test set.")
  }
  
  # Rolling Predictions
  i <- 1
  while(i<=n){
    # Current state of train and test sets
    if(i==1){
      
      # Initial train set
      ts_train_i <- ts_train
      ts_train_i
      tail(ts_train_i)
      
    } else{
      
      # Add weeks from test set to train set based on iteration
      ts_train_i <- ts_train %>%
        add_row(slice(ts_test, 1:(i-1)))
      ts_train_i
      tail(ts_train_i)
      
    }
    
    # Set the test set
    ts_test_i <- ts_test %>%
      slice(i)
    ts_test_i
    
    # Print message of date to predict
    message(paste("Predicting ", ts_test_i$ds, " ", i,"/",n),
            " (",round(100*i/n),"%)", sep="")
    
    # Call prophet() to fit the model
    m <- suppressMessages(prophet(ts_train_i))
    
    # Dataframe of historical + periods to forecast
    future_m <- make_future_dataframe(m, periods = 1, freq = "day")
    future_m
    
    # Make forecasts on future timepoints
    forecast_m <- predict(m, future_m) %>%
      mutate(ds=ymd(ds)) %>%
      as_tsibble(index="ds")
    
    # Merge with observed admissions
    forecast_ts <- forecast_m %>%
      select(ds, yhat, yhat_lower, yhat_upper) %>%
      inner_join(ts_test_i, by="ds") %>%
      relocate(ds, y, yhat, yhat_lower, yhat_upper)
    forecast_ts
    
    # Save prediction and move on to next week
    if(i==1){
      future_ts <- forecast_ts
    } else{
      future_ts <- bind_rows(future_ts, forecast_ts)
    }
    i <- i+1
  }
  # Return final results
  return(future_ts)
}