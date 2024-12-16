##### AHLS TIME SERIES - PROPHET MODEL: HIGH/LOW LOADS #####
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


# Calendar Plot -----------------------------------------------------------

# Calendar heat map
asthma %>%
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


# Prophet Modelling  ------------------------------------------------------

# Split to training and test sets
asthma_train <- asthma_ts %>%
  filter(year(ds) < "2022")
asthma_test <- asthma_ts %>%
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

# HIGH/LOW ----------------------------------------------------------------

# Indicator whether prediction next week will be HIGH/LOW than current week
future_ts <- future_ts %>%
  mutate(next.wk=ifelse(lead(yhat,n=1) < y, "LOW",
                        ifelse(lead(yhat,n=1) > y, "HIGH", "SAME"))) %>%
  relocate(ds, y, next.wk, yhat)

# Indicator whether next week was actually HIGH/LOW than current week
future_ts <- future_ts %>%
  mutate(last.wk=ifelse(y < lag(y,n=1), "LOW",
                        ifelse(y > lag(y,n=1), "HIGH", "SAME"))) %>%
  relocate(ds, y, last.wk, next.wk, yhat)

# Indicator of previous week's prediction
future_ts <- future_ts %>%
  mutate(prev.pred=lag(next.wk,n=1))

# Check
future_ts
tail(future_ts)

# Matrix
prophet_matrix <- future_ts %>%
  filter(!is.na(last.wk) & !is.na(prev.pred)) %>%
  count(last.wk, prev.pred, name="Count") %>%
  mutate(Prop=Count/sum(Count))
prophet_matrix

# Accuracy rate
prophet_matrix %>%
  filter(last.wk==prev.pred) %>%
  summarise(Sum.correct=sum(Count), Prop=sum(Count)/sum(prophet_matrix$Count))


#####

# Indicator whether prediction next week will be HIGH/LOW than current week
future_ts2 <- future_ts2 %>%
  mutate(next.wk=ifelse(lead(yhat,n=1) < y, "LOW",
                        ifelse(lead(yhat,n=1) > y, "HIGH", "SAME"))) %>%
  relocate(ds, y, next.wk, yhat)

# Indicator whether next week was actually HIGH/LOW than current week
future_ts2 <- future_ts2 %>%
  mutate(last.wk=ifelse(y < lag(y,n=1), "LOW",
                        ifelse(y > lag(y,n=1), "HIGH", "SAME"))) %>%
  relocate(ds, y, last.wk, next.wk, yhat)

# Indicator of previous week's prediction
future_ts2 <- future_ts2 %>%
  mutate(prev.pred=lag(next.wk,n=1))

# Check
future_ts2
tail(future_ts2)

# Matrix
prophet_matrix2 <- future_ts2 %>%
  filter(!is.na(last.wk) & !is.na(prev.pred)) %>%
  count(last.wk, prev.pred, name="Count") %>%
  mutate(Prop=Count/sum(Count))
prophet_matrix2

# Accuracy rate
prophet_matrix2 %>%
  filter(last.wk==prev.pred) %>%
  summarise(Sum.correct=sum(Count), Prop=sum(Count)/sum(prophet_matrix2$Count))
