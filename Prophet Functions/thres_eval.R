thres_eval <- function(future_ts, thres1, thres2){
  # Add high and low indicator based on thresholds
  future_ts <- future_ts %>%
    mutate(Status.Act=ifelse(y <= thres1, "NORMAL",
                             ifelse(y > thres1 & y <= thres2, "ELEVATED", "HIGH")),
           Status.Pred=ifelse(yhat <= thres1, "NORMAL",
                              ifelse(y > thres1 & y <= thres2, "ELEVATED", "HIGH")))
  
  # Matrix
  prophet_matrix <- future_ts %>%
    count(Status.Act, Status.Pred, name="Count")
  
  # Accuracy rate
  rate.correct <- prophet_matrix %>%
    filter(Status.Act==Status.Pred) %>%
    summarise(Sum.correct=sum(Count), Prop=sum(Count)/sum(prophet_matrix$Count))
  
  # Return accuracy rate
  return(list(rate.correct, prophet_matrix))
}