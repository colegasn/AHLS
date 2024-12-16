##### AHLS TIME SERIES - PROPHET MODEL: CLINICAL LOADS 2 #####
### Last Update: 2/22/2024

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
asthma_train <- asthma %>%
  rename(ds=Day, y=AllAdmissions) %>%
  filter(year(ds) < "2022")
asthma_test <- asthma %>%
  rename(ds=Day, y=AllAdmissions) %>%
  filter(year(ds) >= "2022")

# Check date ranges of training and test sets
c(min(asthma_train$ds), max(asthma_train$ds))
c(min(asthma_test$ds), max(asthma_test$ds))

# Call prophet() to fit the model
m <- asthma_train %>%
  prophet()

# Dataframe of historical + periods to forecast
future_m <- make_future_dataframe(m, periods = nrow(asthma_test), freq = "day")
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
source("Prophet Functions/prophet_roll2.R")
future_ts2 <- prophet_roll(ts_train=asthma_train, ts_test=asthma_test)
future_ts2

# Backup
future_ts2.backup <- future_ts2

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Prophet Functions/"
# saveRDS(future_ts2, file=paste(dir, "prophet_pred.rds", sep=""))
future_ts2 <- readRDS(paste(dir, "prophet_pred.rds", sep=""))
future_ts2


# Accuracy Framework ------------------------------------------------------

# Read in the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Prophet Functions/"
future_ts2 <- readRDS(paste(dir, "prophet_pred.rds", sep="")) %>%
  select(ds, y, yhat, yhat_lower, yhat_upper)
future_ts2

# Set a threshold
thres1 <- 5

# Add high and low indicator based on thresholds to train and test set predictions
asthma_ts <- left_join(future_ts2, asthma_train, join_by(ds, y)) %>%
  select(ds, y, yhat, yhat_lower, yhat_upper) %>%
  mutate(Status.Act=ifelse(y >= thres1, "HIGH", "NORMAL"),
         Status.Pred=ifelse(yhat_upper >= thres1, "HIGH", "NORMAL"))
asthma_ts
tail(asthma_ts)

# Matrix
prophet_matrix <- asthma_ts %>%
  count(Status.Act, Status.Pred, name="Count")
prophet_matrix

# Accuracy rate
prophet_matrix %>%
  filter(Status.Act==Status.Pred) %>%
  mutate(Prop=Count/sum(Count)) %>%
  summarise(Sum.correct=sum(Count), Prop=sum(Count)/sum(prophet_matrix$Count))

# Plot only the test set predictions
asthma_ts %>%
  ggplot()+
  geom_line(data=filter(asthma_ts, ds>=date("2022-01-01")),
            aes(x=ds, y=y), color="black", alpha=0.7, lwd=0.8, lty="solid")+
  #geom_point(data=filter(asthma_ts, ds>=date("2022-01-01")),
  #          aes(x=ds, y=y, fill=Status.Act), size=4, shape=21)+
  geom_ribbon(data=filter(asthma_ts, ds >=date("2022-01-01")),
              aes(x=ds, ymin=yhat_lower, ymax=yhat_upper),
              fill="magenta", alpha=0.2)+
  geom_line(data=filter(asthma_ts, ds>=date("2022-01-01")),
            aes(x=ds, y=yhat), color="magenta", lwd=0.8)+
  geom_point(data=filter(asthma_ts, ds>=date("2022-01-01")),
             aes(x=ds, y=yhat, fill=Status.Pred), size=3, shape=21)+
  theme_bw()+
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month")+
  scale_fill_manual(name="Prediction",
                    values=c(NORMAL="green",HIGH="red"),
                    limits=c("NORMAL","HIGH"),
                    labels=c("Normal","High"))+
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

# Calculate MAE and MAPE
future_ts2 %>%
  as_tibble() %>%
  summarise(MAE=median(abs(y-yhat)),
            MAPE=(1/nrow(future_ts2))*sum(abs((y-yhat)/y)) ) # when y=0 -> Inf
