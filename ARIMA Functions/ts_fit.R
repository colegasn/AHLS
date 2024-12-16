ts_fit <- function(ts_train, ts_test=NULL, lookahead=NULL, level=NULL, ts_keep=FALSE){
  # Load package if not loaded
  library(dplyr)
  library(fpp3)
  library(feasts)
  library(seasonal)
  
  # Fit a default ARIMA model if no model is specified
  ts_mdl <- ts_train |>
    model(search=ARIMA(AllAdmissions, stepwise = FALSE))

  # Forecast on new data
  if(!is.null(ts_test)){
    
    # Make predictions based on provided test set
    if(is.null(level)){
      # If confidence intervals are not needed
      ts_next <- forecast(ts_mdl, new_data=ts_test) |>
        rename(Dist=AllAdmissions, Predict=.mean) %>%
        select(-.model) %>%
        relocate(WeekDate, Predict, Dist) 
    } else{
      # If confidence intervals are requested
      ts_next <- forecast(ts_mdl, new_data=ts_test)
      ts_next <- hilo(ts_next, level=level) |>
        rename(Dist=AllAdmissions, Predict=.mean) %>%
        select(-.model) %>%
        relocate(WeekDate, Predict, Dist) 
    }
    
    # Only keep 'lookahead' weeks if given
    if(!is.null(lookahead)){
      if(lookahead>0 & lookahead<=nrow(ts_test)){
        ts_next <- ts_next %>%
          slice(1:lookahead)
      } else{
        # Check 'lookahead' value
        stop("Value 'lookahead' must be an integer between 1 and length of test data.")
      }
    }
    
  } else if(!is.null(lookahead) & lookahead <=0){
    
    # Check 'lookahead' value
    stop("Value 'lookahead' must be an integer between 1 and length of test data.")
    
  } else if(is.null(ts_test) & !is.null(lookahead)){
    
    # Make predictions based on 'lookahead' value if no test set is given
    if(is.null(level)){
      # If confidence intervals are not requested
      ts_next <- forecast(ts_mdl, h=lookahead) |>
        rename(Dist=AllAdmissions, Predict=.mean) %>%
        select(-.model) %>%
        relocate(WeekDate, Predict, Dist)
    } else{
      # If confidence intervals are requested
      ts_next <- forecast(ts_mdl, h=lookahead)
      ts_next <- hilo(ts_next, level=level) |>
        rename(Dist=AllAdmissions, Predict=.mean) %>%
        select(-.model) %>%
        relocate(WeekDate, Predict, Dist)
    }
    
  } else{
    # Stop if no test set or 'lookahead' value is given
    stop("A test set or a 'lookahead' value must be specified.")
  }
  
  # Rename output
  ts_predict <- ts_next
  
  # If training set not needed, filter to only predictions
  if(ts_keep==TRUE){
    ts_predict <- bind_rows(ts_train, ts_predict) %>%
      relocate(WeekDate, Predict, Dist)
  }
  
  # Clean up workspace
  rm(ts_mdl, ts_next)
  
  # Return results
  return(ts_predict)
}