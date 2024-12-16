##### CALENDAR PLOTS #####
### Last Update: 2/29/2024

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


# Data Setup --------------------------------------------------------------

# Get daily admissions data
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


# Prophet Modeling --------------------------------------------------------

# Rename variables for prophet model
asthma <- asthma %>%
  rename(ds=Day, y=AllAdmissions)

# Call prophet() to fit the model
m <- asthma %>%
  prophet()

# Dataframe of historical + periods to forecast
future_m <- make_future_dataframe(m, periods = nrow(asthma), freq = "day")

# Make forecasts on future timepoints
forecast_m <- predict(m, future_m) %>%
  mutate(ds=ymd(ds)) %>%
  as_tsibble(index="ds")

# Select the point estimate + 95% CI
future_ts <- forecast_m %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  relocate(ds, yhat, yhat_lower, yhat_upper)
future_ts

# Merge with observed admissions
asthma_m <- inner_join(asthma, future_ts, by="ds") %>%
  select(ds, y, yhat, yhat_lower, yhat_upper)
asthma_m

# Threshold Selection -----------------------------------------------------

# Set a HIGH threshold for actual admissions
act.thres <- 5

# Identify top percentile of actual admissions
asthma_m <- asthma_m %>%
  mutate(Act_Status=factor(ifelse(y >= act.thres, "HIGH", "NORMAL"),
                           levels=c("NORMAL","HIGH")))

# Percentile of actual admissions reached
asthma_m %>%
  group_by(Act_Status) %>%
  summarise(N=n(), Pct=n()/nrow(asthma_m))

#####

# Set a HIGH threshold for predicted admissions
pred.thres <- 3

# Identify top percentile of predicted admissions
asthma_m <- asthma_m %>%
  mutate(Pred_Status=factor(ifelse(yhat >= pred.thres, "HIGH", "NORMAL"),
                            levels=c("NORMAL","HIGH")))

# Percentile of predicted admissions reached
asthma_m %>%
  group_by(Pred_Status) %>%
  summarise(N=n(), Pct=n()/nrow(asthma_m))

#####

# Add high and low indicator based on thresholds to train and test set predictions
asthma_m <- asthma_m %>%
  mutate(Accuracy=ifelse(Act_Status=="NORMAL" & Pred_Status=="NORMAL","TRUE-",
                         ifelse(Act_Status=="NORMAL" & Pred_Status=="HIGH","FALSE+",
                                ifelse(Act_Status=="HIGH" & Pred_Status=="NORMAL","FALSE-",
                                       ifelse(Act_Status=="HIGH" & Pred_Status=="HIGH","TRUE+",NA)))),
         Accuracy=factor(Accuracy, levels=c("TRUE-","TRUE+","FALSE+","FALSE-")))
asthma_m
tail(asthma_m)

# Classification of predictions
asthma_m %>%
  count(Accuracy)

# Accuracy rate
asthma_m %>%
  count(Accuracy) %>%
  filter(Accuracy=="TRUE+"|Accuracy=="TRUE-") %>%
  summarise(n=sum(n), pct=n/nrow(asthma_m))


# Calendar Plot -----------------------------------------------------------

# Merge the prediction classifications to asthma data
asthma_thres <- inner_join(asthma, asthma_m, by="ds") %>%
  rename(Day=ds)
asthma_thres

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
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=Accuracy))+
  geom_tile(color="black")+
  facet_grid(year(Day) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_manual(values=c("grey40","green","blue","firebrick1"),
                    labels=c("True Negative","True Positive","False Negative","False Positive"))+
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
  guides(fill = guide_legend(nrow = 2))+
  xlab("")+ylab("")+
  ggtitle("Prophet Model Accuracy")+
  labs(fill="Evaluation",
       title = "Prophet Model Accuracy",
       subtitle = "Asthma Admissions - All",
       caption = paste("High Actual Threshold:", act.thres, "\n",
                       "High Predicted Threshold:", pred.thres, sep=" "))
cal_thres

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(cal_thres, filename=paste(savepath, "asthma_calendar_accuracy", ".pdf", sep=""),
                                 device=cairo_pdf,
       width=14, height=9, units="in")

