##### RISEUP PIPELINE #####
### Last Update: 1/11/2024

# Loading R Files ---------------------------------------------------------

# Download 'daily_aqi.R' file from GitHub
download.file("https://raw.githubusercontent.com/geomarker-io/riseup_geomarker_pipeline/main/data-raw/daily_aqi.R", destfile = "daily_aqi.R")

# Download 'daily_pollen_mold.rds' file from GitHub
download.file("https://github.com/geomarker-io/riseup_geomarker_pipeline/raw/main/data-raw/daily_aqi.rds", destfile = "data-raw/daily_aqi.rds")

#####

# Download 'daily_pollen_mold.R' file from GitHub
download.file("https://raw.githubusercontent.com/geomarker-io/riseup_geomarker_pipeline/main/data-raw/daily_pollen_mold.R", destfile = "daily_pollen_mold.R")

# Download 'daily_pollen_mold.rds' file from GitHub
download.file("https://github.com/geomarker-io/riseup_geomarker_pipeline/raw/main/data-raw/daily_pollen_mold.rds", destfile = "data-raw/daily_pollen_mold.rds")

#####

# Download 'daily_weather.R' file from GitHub
download.file("https://raw.githubusercontent.com/geomarker-io/riseup_geomarker_pipeline/main/data-raw/daily_weather.R", destfile = "daily_weather.R")

# Download 'daily_weather.rds' file from GitHub
download.file("https://github.com/geomarker-io/riseup_geomarker_pipeline/raw/main/data-raw/daily_weather.rds", destfile = "data-raw/daily_weather.rds")


# Reading in Data ---------------------------------------------------------

# Loading R scripts pulls data and saves as .RDS file
source("daily_aqi.R")
# source("daily_pollen_mold.R")    # does not work
source("daily_weather.R")

# Backdoor solution (in case above does not work)
out2 <- readRDS("data-raw/daily_aqi.rds")
pollen2 <- readRDS("data-raw/daily_pollen_mold.rds")
weather2 <- readRDS("data-raw/daily_weather.rds")

# Explore Data ------------------------------------------------------------

# Look at AQI Values
print(out, n=20)

# look at pollen and outdoor mold data
print(pollen2, n=20)

# Look at weather data
print(weather, n=20)


# Join to Time Series Data ------------------------------------------------

# Set working directory
setwd("C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS")

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(tsibble)

# Load the asthma data
asthma <- read_excel("asthma.xlsx") %>%
  tibble() %>%
  mutate(Day=as.Date(Day))
asthma

# Convert to time series object
asthma2 <- asthma %>%
  as_tsibble(index = Day)
asthma2

# Join AQI to time series
asthma2 <- inner_join(asthma2, out, join_by("Day"=="date"))
asthma2

# Join pollen and mold data to time series (skip due to lots of NA)
# asthma_ts2 <- inner_join(asthma_ts, pollen2, join_by("WeekDate"=="date"))
# asthma_ts2

# Join weather data to time series
asthma2 <- inner_join(asthma2, weather, join_by("Day"=="date"))
asthma2

# Save the weekly time series data for analysis
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/"
saveRDS(asthma2, file=paste(dir, "asthma.rds", sep=""))

# Read in time series
asthma <- readRDS(paste(dir, "asthma.rds", sep="")) %>%
  select(Day, AllAdmissions, aqi, wind_speed, wind_direction, outdoor_temp, relative_humidity)
asthma

#####

# Identify day of the week (week starts on Sunday)
asthma <- asthma %>%
  mutate(WEEK_NAME=wday(Day, label=TRUE, abbr=TRUE),
         WEEK_NUMBER=epiweek(Day),
         WeekDate=as.Date(floor_date(Day, unit="week", week_start=7))) %>%
  relocate(WEEK_NUMBER, .after=MONTH_NUMBER) %>%
  relocate(WEEK_NAME, .after=MONTH_NAME) %>%
  relocate(WeekDate, .after=MonthDate)
print(asthma, n=15)

# Summarize all admissions by week
asthma_week <- asthma %>%
  group_by(WeekDate) %>%
  summarise(AllAdmissions=sum(AllAdmissions)) %>%
  mutate(WEEK_NUMBER=epiweek(WeekDate),
         ROW_NUMBER=row_number(),
         YEAR_NUMBER=year(WeekDate))
asthma_week

# Convert to time series object
asthma_ts <- asthma_week %>%
  as_tsibble(index = WeekDate)
asthma_ts

# Join AQI to time series
asthma_ts <- inner_join(asthma_ts, out, join_by("WeekDate"=="date"))
asthma_ts

# Join pollen and mold data to time series (skip due to lots of NA)
# asthma_ts2 <- inner_join(asthma_ts, pollen2, join_by("WeekDate"=="date"))
# asthma_ts2

# Join weather data to time series
asthma_ts <- inner_join(asthma_ts, weather, join_by("WeekDate"=="date"))
asthma_ts

# Save the weekly time series data for analysis
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS"
saveRDS(asthma_ts, file=paste(dir, "asthma_ts.rds"))

# Read in time series
asthma_ts <- readRDS(paste(dir, "asthma_ts.rds")) %>%
  select(WeekDate, AllAdmissions, aqi, wind_speed, wind_direction, outdoor_temp, relative_humidity)
asthma_ts
