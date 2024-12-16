##### Asthma Plots #####
### Last Update: 10/30/2024

# Load packages
library(tibble)
library(lubridate)
library(readxl)
library(distr)
library(dplyr)
library(fpp3)
library(feasts)
library(seasonal)
library(ggplot2)

# Read in time series with daily asthma admissions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
asthma <- readRDS(paste(dir, "asthma.rds", sep="")) %>%
  tsibble(index=Day) %>%
  fill_gaps()
asthma

# Setup for weekly admissions
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

# Daily Asthma Hospitalizations -------------------------------------------

# Bar plot of the number of admissions each day
asthma_n <- asthma %>%
  mutate(AllAdmissions=replace_na(AllAdmissions, 0)) %>%
  mutate(Group=ifelse(AllAdmissions<=5, as.character(AllAdmissions), "6+")) %>%
  count(Group, name = "Count") %>%
  mutate(Prop=Count/sum(Count))
asthma_n

ggplot(asthma_n, aes(x=Group, y=Count))+
  geom_bar(stat="identity", width = 0.7, color="black", fill="#00b5d1")+
  geom_text(aes(label=Count), vjust=-0.4, size=6)+
  theme_bw()+
  scale_x_discrete(name="Hospitalizations Per Day")+
  scale_y_continuous(limits = c(0,900))+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=14))+
  ggtitle("Daily Asthma Hospitalizations",
          subtitle = "January 1, 2016 - June 30, 2023")


# Time Series Plots -------------------------------------------------------

# Plot daily time series
asthma <- as_tsibble(asthma, index=Day)
ts_day <- ggplot(asthma, aes(x=Day, y=AllAdmissions))+
  geom_line(color="#00b5d1", lwd=0.4)+
  geom_smooth(se=FALSE, color="red", lwd=0.5)+
  scale_x_date(name="Date", date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2016-01-01"), as.Date("2023-06-30")),
               expand = c(0,0))+
  scale_y_continuous(name="Hospitalizations", breaks = 0:14)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                    vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))+
  ggtitle("Daily Asthma Hospitalizations",
          subtitle = "January 1, 2016 - June 30, 2023")
ts_day

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(ts_day, filename=paste(savepath, "daily_time_series.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

# Plot weekly time series
asthma_wk <- as_tsibble(asthma_wk, index=ds)
ts_wk <- ggplot(asthma_wk, aes(x=ds, y=y))+
  #geom_smooth(se=FALSE, color="red", lwd=0.5)+
  geom_line(color="#00b5d1", lwd=0.8)+
  scale_x_date(name="Date", date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2016-01-01"), as.Date("2023-07-01")),
               expand = c(0,0))+
  scale_y_continuous(name="Hospitalizations", breaks=seq(0,50,by=5))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=14))+
  ggtitle("Weekly Asthma Hospitalizations",
          subtitle = "January 1, 2016 - June 30, 2023")
ts_wk

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(ts_wk, filename=paste(savepath, "weekly_time_series.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")


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
  ggtitle("Daily Asthma Hospitalizations")+
  labs(fill="Admissions")
cal

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(cal, filename=paste(savepath, "asthma_calendar.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

# STL Decomposition -------------------------------------------------------

# Decompose daily time series into components
asthma <- as_tsibble(asthma, index=Day)
asthma %>%
  fill_gaps() %>%
  mutate(AllAdmissions=replace_na(AllAdmissions, 0)) %>%
  model(STL(AllAdmissions ~ trend(window = 7)+season(window="periodic"),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

# Decompose weekly time series into components
asthma_wk <- as_tsibble(asthma_wk, index=ds)
asthma_wk %>%
  model(STL(y ~ trend(window = 7)+season(window="periodic"),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

# Plot yearly asthma admissions - one line for each year
asthma_wk %>%
  gg_season(y, labels = "both",
            pal = c("red","orange3","gold","pink","green2","blue","purple"),
            lwd=0.7)+
  theme_bw()+
  labs(y="Weekly Admissions",
       title = "Yearly Asthma Hospitalizations")
