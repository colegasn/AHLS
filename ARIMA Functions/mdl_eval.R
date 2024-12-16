mdl_eval <- function(ts.pred){
  # Deviance: actual - predicted
  ts.pred$deviance <- ts.pred$AllAdmissions-ts.pred$Predict
  
  # SSE: sum(deviance)^2
  SSE <- sum(ts.pred$deviance^2)
  
  # MSE: (1/n)sum(deviance)^2
  MSE <- (1/nrow(ts.pred))*SSE
  
  # SAD: sum(abs(deviance))
  SAD <- sum(abs(ts.pred$deviance))
  
  # MAE: Median Absolute Error
  MAE <- median(abs(ts.pred$deviance))
  
  # MAPE: Median Absolute Percentage Error
  MAPE <- (100/nrow(ts.pred))*sum(abs(ts.pred$AllAdmissions-ts.pred$Predict)/ts.pred$AllAdmissions)
  
  # Return calculated values
  return(list(SSE=SSE, MSE=MSE, SAD=SAD, MAE=MAE, MAPE=MAPE,
              Deviance=ts.pred$deviance))
}