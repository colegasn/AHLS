##### airnow Tutorial
### Last Update: 1/23/2024

# Load necessary packages
library(airnow)
library(dplyr)

# Creating an AirNow account
set_airnow_token()

# Set API Key
source("airnow_API_KEY.R")
set_airnow_token(token=API_KEY)
# get_airnow_token()    - verify API Key


# Pulling AirNow Data -----------------------------------------------------

# Get current conditions by ZIP code
?get_airnow_conditions
air45013 <- get_airnow_conditions(zip="45013")
air45013

# Get current conditions by latitude and longitude
air.myhouse <- get_airnow_conditions(latitude = 39.36243581389768,
                                     longitude = -84.65074958213444)
air.myhouse


# Find the site with highest PM2.5 AQI within a bounding box
?get_airnow_area
# FORMAT: c(minLong, minLat, maxLong, maxLat) (bottom-left to upper-right)
air.box <- get_airnow_area(box=c(-85.30197702364258, 38.51549770992655,
                      -83.22134192892264, 39.78787948109574),
                      parameters = "pm25", verbose = TRUE)
air.box 

# Get forecast for future dates
# ?get_airnow_forecast
forecast45013 <- get_airnow_forecast(zip="45013")
forecast45013

