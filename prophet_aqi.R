##### PROPHET MODEL - AQI ANALYSIS #####
### Last Update: 10/31/2024

# Load packages
library(readxl)
library(prophet)
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(fable)
library(ggplot2)
library(ggpubr)

# Data Import ---------------------------------------------

# Daily admissions
aqi <- readRDS("asthma.rds") %>%
  tibble() %>%
  mutate(WEEK_NAME=wday(Day, label=TRUE, abbr=TRUE),
         WEEK_NUMBER=epiweek(Day),
         WeekDate=floor_date(Day, unit="week", week_start=7),
         Quarter=quarter(Day, fiscal_start = 7, type="date_first")) %>%
  relocate(Quarter, .before=MONTH_NUMBER) %>%
  relocate(WEEK_NUMBER, .after=MONTH_NUMBER) %>%
  relocate(WEEK_NAME, .after=MONTH_NAME) %>%
  relocate(WeekDate, .after=MonthDate) %>%
  rename(AQI=aqi)
print(aqi, n=15)


# AQI Summary -------------------------------------------------------------

# Convert to tsibble object
aqi <- as_tsibble(aqi, index=Day)

# 5-number summary
summary(aqi$AQI)

# Plot AQI
aqi_plot <- ggplot(aqi, aes(x=Day, y=AQI))+
  geom_line(color="purple", lwd=0.4)+
  scale_x_date(name="Date", date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2016-01-01"), as.Date("2023-06-30")),
               expand = c(0,0))+
  scale_y_continuous(name="AQI", breaks = seq(0,200,by=20))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))+
  ggtitle("Daily Asthma Hospitalizations",
          subtitle = "January 1, 2016 - June 30, 2023")
aqi_plot

# Categorize AQI
aqi <- aqi %>%
  mutate(health=ifelse(AQI<=50, "Good",
                ifelse(AQI>50 & AQI<=100, "Moderate",
                ifelse(AQI>100 & AQI<=150, "Sensitive",
                ifelse(AQI>150 & AQI<=200, "Unhealthy",
                ifelse(AQI>200 & AQI<=300, "Very Unhealthy",
                ifelse(AQI>300, "Hazardous", NA))))))
         )
aqi

# Barplot of days with high AQI
aqi_n <- aqi %>%
  count(health, name = "Count") %>%
  mutate(Prop=Count/sum(Count))
aqi_n

ggplot(aqi_n, aes(x=health, y=Count))+
  geom_bar(stat="identity", width = 0.7, color="black", fill="purple")+
  geom_text(aes(label=Count), vjust=-0.4, size=6)+
  theme_bw()+
  scale_x_discrete(name="Hospitalizations Per Day")+
  scale_y_continuous(limits = c(0,1500))+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=14))+
  ggtitle("Daily AQI",
          subtitle = "January 1, 2016 - June 30, 2023")

# Calendar heat map of AQI
cal <- aqi %>%
  group_by(MonthDate) %>%
  mutate(WEEK_MONTH=(5+day(Day) + wday(floor_date(Day, 'month'))) %/% 7,
         WEEK_NAME=factor(WEEK_NAME, labels=c("Su","M","Tu","W","Th","F","Sa")),
         MONTH_NAME=factor(MONTH_NAME,
                           levels=c("January","February","March",
                                    "April","May","June","July",
                                    "August","September","October",
                                    "November","December"))) %>%
  #filter(year(Day)<2022) %>%
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=health))+
  geom_tile(color="black")+
  facet_grid(year(Day) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_manual(values=c("green","yellow","orange","red",""),
                    labels=c("Good","Moderate","Sensitive","Unhealthy"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "grey85"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey30", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=11, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "none")+
  xlab("")+ylab("")+
  labs(title="Daily AQI",
       subtitle="January 1, 2016 - June 30, 2023",
       fill="AQI")
cal


# Prophet Modeling of AQI -------------------------------------------------

# Call prophet() to fit the model - note the renaming of variables!
m_aqi <- aqi %>%
  rename(ds=Day, y=AQI) %>%
  prophet()

# Dataframe of historical + periods to forecast
future_aqi <- make_future_dataframe(m_aqi, periods = 365)
head(future_aqi, n=10)  
tail(future_aqi, n=10)

# Make forecasts on future timepoints
forecast_aqi <- predict(m_aqi, future_aqi)

# Explore the forecast
names(forecast_aqi)

# Display some forecast values
forecast_aqi %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

# Plot the forecast
plot(m_aqi, forecast_aqi)+
  theme_bw()+
  labs(x="Date", y="# of Admissions")+
  ggtitle("Daily AQI Forecast")

# Break the forecast down by trend, seasonality, and random terms
prophet_plot_components(m_aqi, forecast_aqi)

# Interactive plot to explore the data
dyplot.prophet(m_aqi, forecast_aqi)


# Prophet Modeling of Admissions ------------------------------------------

# Call prophet() to fit the model - note the renaming of variables!
m_asthma <- aqi %>%
  rename(ds=Day, y=AllAdmissions) %>%
  prophet()

# Dataframe of historical + periods to forecast
future_asthma <- make_future_dataframe(m_asthma, periods = 365)
head(future_asthma, n=10)  
tail(future_asthma, n=10)

# Make forecasts on future timepoints
forecast_asthma <- predict(m_asthma, future_asthma)

# Explore the forecast
names(forecast_asthma)

# Display some forecast values
forecast_asthma %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(n=30)

# Plot the forecast
plot(m_asthma, forecast_asthma)+
  theme_bw()+
  labs(x="Date", y="# of Admissions")+
  ggtitle("All Asthma Admissions - Daily")

# Break the forecast down by trend, seasonality, and random terms
prophet_plot_components(m_asthma, forecast_asthma)

# Interactive plot to explore the data
dyplot.prophet(m_asthma, forecast_asthma)


# Comparisons -------------------------------------------------------------

# Obtain the residuals of the Prophet model fit for admissions
e_asthma <- inner_join(forecast_asthma, m_asthma$history, by="ds") %>%
  select(ds, yhat, y) %>%
  mutate(residuals=y-yhat,
         Day=as.Date(ds)) %>%
  rename(yhat.asthma=yhat, y.asthma=y)
e_asthma

# Convert to tsibble object
e_asthma <- as_tsibble(e_asthma, index = Day)

# Plot the residuals of Prophet model for admissions
e_asthma.plot <- ggplot(e_asthma, aes(x=Day, y=residuals))+
  geom_line(color="black", lwd=0.4)+
  geom_hline(yintercept=0, linetype="solid", color="red", linewidth=1)+
  scale_x_date(name="Date", date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2016-01-01"), as.Date("2023-06-30")),
               expand = c(0,0))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))+
  ggtitle("Daily Asthma Hospitalizations",
          subtitle = "January 1, 2016 - June 30, 2023")
e_asthma.plot

# Add Prophet predictions of AQI
e_aqi <- inner_join(e_asthma, m_aqi$history, by="ds") %>%
  inner_join(forecast_aqi, by="ds") %>%
  rename(y.aqi=y, yhat.aqi=yhat) %>%
  select(Day, y.aqi, yhat.aqi, y.asthma, yhat.asthma, residuals)

# Convert to tsibble object
e_aqi <- as_tsibble(e_aqi, index=Day) 
e_aqi

# Plot residuals of Prophet for admissions vs. Prophet AQI predictions
e_aqi.plot <- ggplot(e_aqi, aes(x=y.aqi, y=residuals))+
  geom_point(color="black")+
  scale_x_continuous(name="AQI", breaks=seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))+
  ggtitle("Residuals vs. AQI",
          subtitle = "January 1, 2016 - June 30, 2023")
e_aqi.plot

# Correlation test between actual AQI and residuals of Prophet model
cor.test(e_aqi$y.aqi, e_aqi$residuals, method = "kendall")


# Lagged AQI Exposure -----------------------------------------------------

# Create Lag 0, Lag 1, Lag 2, Lag 3, Lag 4, and Lag 5 exposures
lag.aqi <- aqi %>%
  select(Day, AllAdmissions, AQI) %>%
  mutate(lag0_AQI=lag(AQI, n=0),
         lag1_AQI=lag(AQI, n=1),
         lag2_AQI=lag(AQI, n=2),
         lag3_AQI=lag(AQI, n=3),
         lag4_AQI=lag(AQI, n=4),
         lag5_AQI=lag(AQI, n=5))
lag.aqi

# Fit prophet() model to lag AQI 
m_lag0_aqi <- lag.aqi %>%
  rename(ds=Day, y=lag0_AQI) %>%
  prophet()
m_lag1_aqi <- lag.aqi %>%
  rename(ds=Day, y=lag1_AQI) %>%
  prophet()
m_lag2_aqi <- lag.aqi %>%
  rename(ds=Day, y=lag2_AQI) %>%
  prophet()
m_lag3_aqi <- lag.aqi %>%
  rename(ds=Day, y=lag3_AQI) %>%
  prophet()
m_lag4_aqi <- lag.aqi %>%
  rename(ds=Day, y=lag4_AQI) %>%
  prophet()
m_lag5_aqi <- lag.aqi %>%
  rename(ds=Day, y=lag5_AQI) %>%
  prophet()

# Dataframe of historical + periods to forecast
future_lag0_aqi <- make_future_dataframe(m_lag0_aqi, periods = 365)
future_lag1_aqi <- make_future_dataframe(m_lag1_aqi, periods = 365)
future_lag2_aqi <- make_future_dataframe(m_lag2_aqi, periods = 365)
future_lag3_aqi <- make_future_dataframe(m_lag3_aqi, periods = 365)
future_lag4_aqi <- make_future_dataframe(m_lag4_aqi, periods = 365)
future_lag5_aqi <- make_future_dataframe(m_lag5_aqi, periods = 365)

# Make forecasts on future time points
forecast_lag0_aqi <- predict(m_lag0_aqi, future_lag0_aqi) %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(lag0.yhat=yhat, lag0.yhat_lower=yhat_lower, lag0.yhat_upper=yhat_upper)
forecast_lag1_aqi <- predict(m_lag1_aqi, future_lag1_aqi) %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(lag1.yhat=yhat, lag1.yhat_lower=yhat_lower, lag1.yhat_upper=yhat_upper)
forecast_lag2_aqi <- predict(m_lag2_aqi, future_lag2_aqi) %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(lag2.yhat=yhat, lag2.yhat_lower=yhat_lower, lag2.yhat_upper=yhat_upper)
forecast_lag3_aqi <- predict(m_lag3_aqi, future_lag3_aqi) %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(lag3.yhat=yhat, lag3.yhat_lower=yhat_lower, lag3.yhat_upper=yhat_upper)
forecast_lag4_aqi <- predict(m_lag4_aqi, future_lag4_aqi) %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(lag4.yhat=yhat, lag4.yhat_lower=yhat_lower, lag4.yhat_upper=yhat_upper)
forecast_lag5_aqi <- predict(m_lag5_aqi, future_lag5_aqi) %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  rename(lag5.yhat=yhat, lag5.yhat_lower=yhat_lower, lag5.yhat_upper=yhat_upper)

# Combine into one dataframe by date
forecast_lag_aqi <- inner_join(forecast_lag0_aqi, forecast_lag1_aqi, by="ds") %>%
  inner_join(forecast_lag2_aqi, by="ds") %>%
  inner_join(forecast_lag3_aqi, by="ds") %>%
  inner_join(forecast_lag4_aqi, by="ds") %>%
  inner_join(forecast_lag5_aqi, by="ds")

# Add actual AQI observations to forecasted AQI
lag_aqi_forecast <- m_lag0_aqi$history %>%
  select(ds, AQI) %>%
  right_join(forecast_lag_aqi, by="ds") %>%
  rename(y.AQI=AQI)

# Display some forecast values
lag_aqi_forecast %>%
  select(ds, y.AQI, lag0.yhat, lag1.yhat, lag2.yhat, lag3.yhat, lag4.yhat, lag5.yhat) %>%
  head(n=21)


# Lagged AQI to Admissions Residuals --------------------------------------

# Add Prophet predictions of AQI
e_aqi <- inner_join(e_asthma, lag_aqi_forecast, by="ds")
e_aqi

# Convert to tsibble object
e_aqi <- as_tsibble(e_aqi, index=Day) 
e_aqi

# Plot residuals of Prophet for admissions vs. Prophet AQI predictions
e_aqi.plot <- ggplot(e_aqi, aes(x=y.AQI, y=residuals))+
  geom_point(color="black")+
  scale_x_continuous(name="AQI", breaks=seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))+
  ggtitle("Residuals vs. AQI",
          subtitle = "January 1, 2016 - June 30, 2023")
e_aqi.plot

# Correlation test between actual AQI and residuals of Prophet model
cor.test(e_aqi$y.AQI, e_aqi$residuals, method = "kendall")


# Correlation of Lag AQI and Residuals ------------------------------------

# Add residuals of hospitalization predictions to lag AQI values
e_lag.aqi <- lag.aqi %>%
  inner_join(e_asthma, by="Day") %>%
  select(Day, y.asthma, yhat.asthma, residuals,
         lag0_AQI, lag1_AQI, lag2_AQI, lag3_AQI, lag4_AQI, lag5_AQI)
e_lag.aqi

# LAG 0 AQI vs. RESIDUALS
e_lag0_aqi.plot <- ggplot(e_lag.aqi, aes(x=lag0_AQI, y=residuals))+
  geom_point(color="black", alpha=0.2)+
  scale_x_continuous(name="AQI", breaks=seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  geom_hline(yintercept = 0, col="red", lwd=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))

# LAG 1 AQI vs. RESIDUALS
e_lag1_aqi.plot <- ggplot(e_lag.aqi, aes(x=lag1_AQI, y=residuals))+
  geom_point(color="black", alpha=0.2)+
  scale_x_continuous(name="AQI", breaks=seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  geom_hline(yintercept = 0, col="red", lwd=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))

# LAG 2 AQI vs. RESIDUALS
e_lag2_aqi.plot <- ggplot(e_lag.aqi, aes(x=lag2_AQI, y=residuals))+
  geom_point(color="black", alpha=0.2)+
  scale_x_continuous(name="AQI", breaks=seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  geom_hline(yintercept = 0, col="red", lwd=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))

# LAG 3 AQI vs. RESIDUALS
e_lag3_aqi.plot <- ggplot(e_lag.aqi, aes(x=lag3_AQI, y=residuals))+
  geom_point(color="black", alpha=0.2)+
  scale_x_continuous(name="AQI", breaks=seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  geom_hline(yintercept = 0, col="red", lwd=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))

# LAG 4 AQI vs. RESIDUALS
e_lag4_aqi.plot <- ggplot(e_lag.aqi, aes(x=lag4_AQI, y=residuals))+
  geom_point(color="black", alpha=0.2)+
  scale_x_continuous(name="AQI", breaks=seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  geom_hline(yintercept = 0, col="red", lwd=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))

# LAG 5 AQI vs. RESIDUALS
e_lag5_aqi.plot <- ggplot(e_lag.aqi, aes(x=lag5_AQI, y=residuals))+
  geom_point(color="black", alpha=0.2)+
  scale_x_continuous(name="AQI", breaks=seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  geom_hline(yintercept = 0, col="red", lwd=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))

# Facet plot of Lag AQI to Residuals of asthma hospitalizations
e_lag_aqi.plot <- ggarrange(e_lag0_aqi.plot, e_lag1_aqi.plot, e_lag2_aqi.plot,
                            e_lag3_aqi.plot, e_lag4_aqi.plot, e_lag5_aqi.plot,
                            labels = c("Lag-0", "Lag-1", "Lag-2","Lag-3","Lag-4","Lag-5"),
                            ncol = 3, nrow = 2, hjust = -8.8, vjust = 1.7)
e_lag_aqi.plot

#####

# Check the correlation between lag AQI and residuals
cor.test(e_lag.aqi$lag0_AQI, e_aqi$residuals, method = "kendall")
cor.test(e_lag.aqi$lag1_AQI, e_aqi$residuals, method = "kendall")
cor.test(e_lag.aqi$lag2_AQI, e_aqi$residuals, method = "kendall")
cor.test(e_lag.aqi$lag3_AQI, e_aqi$residuals, method = "kendall")
cor.test(e_lag.aqi$lag4_AQI, e_aqi$residuals, method = "kendall") # borderline
cor.test(e_lag.aqi$lag5_AQI, e_aqi$residuals, method = "kendall")
