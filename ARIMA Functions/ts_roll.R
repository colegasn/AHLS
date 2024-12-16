ts_roll <- function(ts_train, ts_test, lookahead=NULL, ts_keep=FALSE){
  # Load package if not loaded
  library(dplyr)
  library(fpp3)
  library(feasts)
  library(seasonal)
  
  # Source 'ts_fit.R' file
  source("ts_fit.R")
  
  #####
  
  # Check 'lookahead' value
  if(is.null(lookahead)){
    
    # Predict every observation of test set if 'lookahead' not given
    lookahead <- nrow(ts_test)
    
  } else if(lookahead > n_row(ts_test)){
    
    # Set 'lookahead' to max number of weeks in test data if exceeded
    warning("Value 'lookahead' excess maximum number of weeks in test data.")
    lookahead <- nrow(ts_test)
    
  } else if(lookahead <=0){
    
    # Stop if 'lookahead' is not a positive integer
    stop("Value 'lookahead' must be an integer between 1 and length of test data.")
  }
  
  # Set iteration number
  i <- 1
  
  #####
  
  # Repeat the forecasts for 'iter' weeks
  while(i <= lookahead){
    
    if(i==1){
      
      # Initial train set
      ts_train_i <- ts_train
      ts_test_i <- ts_test
      
      ts_train_i
      tail(ts_train_i)
      ts_test_i
      
    } else{
      
      # Add weeks from test set to train set based on iteration
      ts_train_i <- ts_train %>%
        add_row(slice(ts_test, 1:(i-1)))
      ts_train_i
      tail(ts_train_i)
      
    }
    
    # Select week from test set to make prediction
    ts_test_i <- ts_test %>%
      slice(i)
    ts_test_i
    
    # Get next week's prediction
    ts_new <- ts_fit(ts_train = ts_train_i,
                     ts_test = ts_test_i)
    ts_new
    
    # Save prediction at each iteration
    if(i==1){
      ts_predict <- ts_new
    } else{
      ts_predict <- bind_rows(ts_predict, ts_new)
    }
    
    # Remove for next iteration
    rm(ts_train_i, ts_test_i, ts_new)
    
    # Go to the next iteration
    i <- i+1
  }
  
  # Merge predictions with initial test data
  ts_predict <- left_join(ts_test, ts_predict, by="WeekDate")
  
  # If training set not needed, filter to only predictions
  if(ts_keep==TRUE){
    
  }
  
  # Return the iterated time series
  return(ts_predict)
}