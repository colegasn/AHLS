##### AHLS TIME SERIES - PROPHET MODEL: CLINICAL LOADS #####
### Last Update: 8/14/2024

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

# Get weekly admissions data
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

# Aggregate
asthma_wk <- asthma %>%
  group_by(WeekDate) %>%
  summarise(AllAdmissions=sum(AllAdmissions)) %>%
  mutate(WEEK_NUMBER=epiweek(WeekDate),
         ROW_NUMBER=row_number()) %>%
  as_tsibble(index = ROW_NUMBER) %>%
  relocate(ROW_NUMBER, WEEK_NUMBER, WeekDate, AllAdmissions) %>%
  select(WeekDate, AllAdmissions) %>%
  rename(ds=WeekDate, y=AllAdmissions)
print(asthma_wk, n=15)


# Calendar Plot -----------------------------------------------------------

# Calendar heat map
cal <- asthma %>%
  group_by(MonthDate) %>%
  mutate(WEEK_MONTH=(5+day(Day) + wday(floor_date(Day, 'month'))) %/% 7,
         WEEK_NAME=factor(WEEK_NAME, labels=c("Su","M","Tu","W","Th","F","Sa")),
         MONTH_NAME=factor(MONTH_NAME,
                           levels=c("January","February","March",
                                    "April","May","June","July",
                                    "August","September","October",
                                    "November","December"))) %>%
  #filter(year(Day)<2022) %>%
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=AllAdmissions))+
  geom_tile(color="black")+
  facet_grid(year(Day) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_gradient(low="white", high="blue", na.value="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "grey90"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey50", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=9, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "bottom")+
  xlab("")+ylab("")+
  ggtitle("Asthma Admissions - All")+
  labs(fill="Admissions")
cal

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(cal, filename=paste(savepath, "asthma_calendar.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

####

# Plot density of daily admissions
ggplot(asthma, aes(x=AllAdmissions))+
  geom_line(stat="density")

# Bar plot of the number of admissions each day
asthma_n <- asthma %>%
  mutate(Group=ifelse(AllAdmissions<=5, as.character(AllAdmissions), "6+")) %>%
  count(Group, name = "Count")
asthma_n

ggplot(asthma_n, aes(x=Group, y=Count))+
  geom_bar(stat="identity", color="black", fill="#00b5d1")+
  geom_text(aes(label=Count), vjust=-0.4, size=4.5)+
  theme_bw()+
  scale_x_discrete(name="Hospitalizations")+
  scale_y_continuous(limits = c(0,900))+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=11),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))+
  ggtitle("Daily Asthma Hospitalizations",
          subtitle = "January 1, 2016 - June 30, 2023")

# Percentile of daily admissions
quantile(asthma$AllAdmissions, probs = c(0.50, 0.75, 0.90, 0.95))

# Identify top percentile of admissions
thres <- 5
asthma_thres <- asthma %>%
  mutate(Status=factor(ifelse(AllAdmissions >= thres, "HIGH", "NORMAL"),
                       levels=c("NORMAL","HIGH")))

# How many days is the top percentile reached?
asthma_thres %>%
  group_by(Status) %>%
  summarise(N=n(), Pct=n()/nrow(asthma_thres))
  
# Calendar heat map by HIGH/NORMAL days
cal_thres <- asthma_thres %>%
  group_by(MonthDate) %>%
  mutate(WEEK_MONTH=(5+day(Day) + wday(floor_date(Day, 'month'))) %/% 7,
         WEEK_NAME=factor(WEEK_NAME, labels=c("Su","M","Tu","W","Th","F","Sa")),
         MONTH_NAME=factor(MONTH_NAME,
                           levels=c("January","February","March",
                                    "April","May","June","July",
                                    "August","September","October",
                                    "November","December"))) %>%
  #filter(year(Day)<2022) %>%
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=Status))+
  geom_tile(color="black")+
  facet_grid(year(Day) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_manual(values=c("grey40","firebrick1"),
                    labels=c("Less than 5","At Least 5"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "grey90"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey50", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=9, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "bottom")+
  xlab("")+ylab("")+
  ggtitle("Asthma Admissions - All")+
  labs(fill="Admissions")
cal_thres

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(cal_thres, filename=paste(savepath, "asthma_thres_calendar.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

# Number of days in the month where admissions were HIGH
asthma_thres %>%
  group_by(MONTH_NAME) %>%
  filter(Status=="HIGH") %>%
  summarise(N=n()) %>%
  arrange(desc(N))

# Days of the week where admissions were HIGH
asthma_thres %>%
  group_by(WEEK_NAME) %>%
  filter(Status=="HIGH") %>%
  summarise(N=n()) %>%
  arrange(desc(N))

# Prophet Modelling  ------------------------------------------------------

# Split to training and test sets
asthma_train <- asthma_wk %>%
  filter(year(ds) < "2022")
asthma_test <- asthma_wk %>%
  filter(year(ds) >= "2022")

# Check date ranges of training and test sets
c(min(asthma_train$ds), max(asthma_train$ds))
c(min(asthma_test$ds), max(asthma_test$ds))

# Call prophet() to fit the model
m <- asthma_train %>%
  prophet()

# Dataframe of historical + periods to forecast
future_m <- make_future_dataframe(m, periods = nrow(asthma_test), freq = "week")
c(head(future_m, n=10), tail(future_m, n=10))

# Make forecasts on future timepoints
forecast_m <- predict(m, future_m) %>%
  mutate(ds=ymd(ds)) %>%
  as_tsibble(index="ds")

# Merge with observed admissions
future_ts <- forecast_m %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  inner_join(asthma_test, by="ds") %>%
  relocate(ds, y, yhat, yhat_lower, yhat_upper)
future_ts

# Apply rolling predictions to the prophet model
source("Prophet Functions/prophet_roll.R")
future_ts2 <- prophet_roll(ts_train=asthma_train, ts_test=asthma_test)
future_ts2

# Backup
future_ts2.backup <- future_ts2


# Prophet Plots -----------------------------------------------------------

# Get a five-number summary of weekly asthma admissions
summary(asthma_train$y)

# Bar plot of total admissions by week
asthma_wk %>%
  count(y, name="Count") %>%
  mutate(Prop=Count/sum(Count)) %>%
  ggplot(aes(x=y, y=Count))+
  geom_col(fill="#00b5d1")+
  theme_bw()+
  xlab("Admissions per Week")+ylab("Count")+
  ggtitle("Weekly Asthma Admissions per Week - All")+
  geom_vline(xintercept = 12, color="black", linewidth=1.2)+
  geom_vline(xintercept = 15.5, color="blue", linewidth=0.8)

# Set thresholds (NORMAL-ELEVATED; ELEVATED-HIGH)
thres1 <- 18
thres2 <- 30

# Add high and low indicator based on thresholds to train and test set predictions
asthma_ts2 <- full_join(asthma_train, future_ts2, join_by(ds, y, ROW_NUMBER)) %>%
  mutate(Status.Act=ifelse(y <= thres1, "NORMAL",
                           ifelse(y > thres1 & y <= thres2, "ELEVATED", "HIGH")),
         Status.Pred=ifelse(yhat <= thres1, "NORMAL",
                            ifelse(y > thres1 & y <= thres2, "ELEVATED", "HIGH")))
asthma_ts2
tail(asthma_ts2)

# Plot the entire time series and predictions
asthma_ts2 %>%
  ggplot()+
  geom_line(data=filter(asthma_ts2, ds<=date("2021-12-31")),
            aes(x=ds, y=y), color="black", alpha=1.0, lwd=0.8, lty="solid")+
  #geom_point(data=filter(asthma_ts2, ds<=date("2021-12-31")),
  #           aes(x=ds, y=y, fill=Status.Act), size=3, shape=21)+
  geom_ribbon(data=filter(asthma_ts2, ds >=date("2022-01-01")),
                          aes(x=ds, ymin=yhat_lower, ymax=yhat_upper),
              fill="magenta", alpha=0.2)+
  geom_line(data=filter(asthma_ts2, ds>=date("2022-01-01")),
            aes(x=ds, y=y), color="black", alpha=0.7, lwd=0.8, lty="solid")+
  #geom_point(data=filter(asthma_ts2, ds>=date("2022-01-01")),
  #          aes(x=ds, y=y, fill=Status.Act), size=3, shape=21)+
  geom_line(data=filter(asthma_ts2, ds>=date("2022-01-01")),
            aes(x=ds, y=yhat), color="magenta", lwd=0.8)+
  geom_point(data=filter(asthma_ts2, ds>=date("2022-01-01")),
             aes(x=ds, y=yhat, fill=Status.Pred), size=3, shape=22)+
  theme_bw()+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  scale_fill_manual(name="Prediction",
                    values=c(NORMAL="green",ELEVATED="orange",HIGH="red"),
                    limits=c("NORMAL","ELEVATED","HIGH"),
                    labels=c("Normal","Elevated","High"))+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=12),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Forecast Patients Admitted for Asthma",
          subtitle="January 1, 2016 - June 30, 2023")

# Plot only the test set predictions
asthma_ts2 %>%
  ggplot()+
  geom_line(data=filter(asthma_ts2, ds>=date("2022-01-01")),
            aes(x=ds, y=y), color="black", alpha=0.7, lwd=0.8, lty="solid")+
  #geom_point(data=filter(asthma_ts2, ds>=date("2022-01-01")),
  #          aes(x=ds, y=y, fill=Status.Act), size=4, shape=21)+
  geom_ribbon(data=filter(asthma_ts2, ds >=date("2022-01-01")),
              aes(x=ds, ymin=yhat_lower, ymax=yhat_upper),
              fill="magenta", alpha=0.2)+
  geom_line(data=filter(asthma_ts2, ds>=date("2022-01-01")),
            aes(x=ds, y=yhat), color="magenta", lwd=0.8)+
  geom_point(data=filter(asthma_ts2, ds>=date("2022-01-01")),
             aes(x=ds, y=yhat, fill=Status.Pred), size=3, shape=22)+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month")+
  scale_fill_manual(name="Prediction",
                    values=c(NORMAL="green",ELEVATED="orange",HIGH="red"),
                    limits=c("NORMAL","ELEVATED","HIGH"),
                    labels=c("Normal","Elevated","High"))+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=12),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Forecast Patients Admitted for Asthma",
          subtitle="January 1, 2022 - June 30, 2023")

# Barebones test predictions plot
asthma_ts2 %>%
  ggplot()+
  geom_line(data=filter(asthma_ts2, ds>=date("2022-01-01")),
            aes(x=ds, y=y, color="Actual"), alpha=0.7, lwd=0.8, lty="solid")+
  geom_ribbon(data=filter(asthma_ts2, ds >=date("2022-01-01")),
              aes(x=ds, ymin=yhat_lower, ymax=yhat_upper),
              fill="magenta", alpha=0.2)+
  geom_line(data=filter(asthma_ts2, ds>=date("2022-01-01")),
            aes(x=ds, y=yhat, color="Predicted"), lwd=0.8)+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month")+
  scale_color_manual(name="", values=c("Actual"="black","Predicted"="magenta"))+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=12),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=12))+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Forecast Patients Admitted for Asthma",
          subtitle="January 1, 2022 - June 30, 2023")


# Thresholds --------------------------------------------------------------

# Set thresholds (NORMAL-ELEVATED; ELEVATED-HIGH)
thres1 <- 18
thres2 <- 30

# Add high and low indicator based on thresholds
future_ts <- future_ts %>%
  mutate(Status.Act=ifelse(y <= thres1, "NORMAL",
                           ifelse(y > thres1 & y <= thres2, "ELEVATED", "HIGH")),
         Status.Pred=ifelse(yhat <= thres1, "NORMAL",
                            ifelse(y > thres1 & y <= thres2, "ELEVATED", "HIGH")))
future_ts

# Matrix
prophet_matrix <- future_ts %>%
  count(Status.Act, Status.Pred, name="Count")
prophet_matrix

# Accuracy rate
prophet_matrix %>%
  filter(Status.Act==Status.Pred) %>%
  mutate(Prop=Count/sum(Count)) %>%
  summarise(Sum.correct=sum(Count), Prop=sum(Count)/sum(prophet_matrix$Count))

# Add high and low indicator based on thresholds
future_ts2 <- future_ts2 %>%
  mutate(Status.Act=ifelse(y <= thres1, "NORMAL",
                           ifelse(y > thres1 & y <= thres2, "ELEVATED", "HIGH")),
         Status.Pred=ifelse(yhat <= thres1, "NORMAL",
                            ifelse(y > thres1 & y <= thres2, "ELEVATED", "HIGH")))
future_ts2

# Matrix
prophet_matrix2 <- future_ts2 %>%
  count(Status.Act, Status.Pred, name="Count")
prophet_matrix2

# Accuracy rate
prophet_matrix2 %>%
  filter(Status.Act==Status.Pred) %>%
  summarise(Sum.correct=sum(Count), Prop=sum(Count)/sum(prophet_matrix2$Count))

#####

# Function that pulls accuracy rate
source("Prophet Functions/thres_eval.R")
thres_result <- thres_eval(future_ts = future_ts, thres1 = 18, thres2 = 30)
thres_result

#####
# Set range of thresholds to try (thres1 < thres2)
thres1.min <- 10; thres1.max <- 25
thres2.min <- 16; thres2.max <- 35

# Get the combination of thresholds
thres <- expand_grid(thres1=thres1.min:thres1.max, thres2=thres2.min:thres2.max) %>%
  filter(thres1<thres2)
thres

# Loop that uses function to calculate accuracy rate from specified thresholds
i <- 1
while(i<=nrow(thres)){
  # Evaluate correction accuracy between two thresholds
  thres_result <- thres_eval(future_ts,
                             thres1 = thres$thres1[i],
                             thres2 = thres$thres2[i])
  
  # Save # correct predictions/percentage and the correction matrix
  if(i==1){
    correction.matrix1 <- thres_result[[1]] %>%
      mutate(thres1=thres$thres1[i], thres2=thres$thres2[i])
    correction.matrix2 <- thres_result[[2]] %>%
      mutate(thres1=thres$thres1[i], thres2=thres$thres2[i])
  } else{
    correction.row1 <- thres_result[[1]] %>%
      mutate(thres1=thres$thres1[i], thres2=thres$thres2[i])
    correction.matrix1 <- bind_rows(correction.matrix1, correction.row1)
    correction.row2 <- thres_result[[2]] %>%
      mutate(thres1=thres$thres1[i], thres2=thres$thres2[i])
    correction.matrix2 <- bind_rows(correction.matrix2, correction.row2)
  }
  rm(thres_result)
  i <- i+1
}

# Examine the performance of prophet model with thresholds
correction.matrix <- correction.matrix2 %>%
  pivot_wider(id_cols=c(thres1, thres2),
              names_from = c(Status.Act, Status.Pred),
              values_from = Count) %>%
  inner_join(correction.matrix1, by=c("thres1","thres2")) %>%
  relocate(thres1, thres2, Sum.correct, Prop) %>%
  arrange(desc(Prop))
correction.matrix %>%
  print(n=40)

# Filter rows where NA's are included
correction.matrix %>%
  filter(complete.cases(correction.matrix)==TRUE) %>%
  print(n=40)
