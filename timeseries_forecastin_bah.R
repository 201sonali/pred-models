rm(list=ls())

# load libraries
library(forecast)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(astsa)
library(xts)
library(lubridate)

# Import data
data <- read.csv("~/MSDS/predictive models/SupplyChainData.csv", header=FALSE)

# Add headers
names(data) <- c("date", "index")

# Reformat date to use in forecasting
data$date <- as.Date(data$date, format = "%d-%b-%Y")
# Unsure why this is deleted but add back in
data$date[1]="1998-01-31"

# Create a time series object with the predictor variable in column 2
ts_data <- ts(data$index, start = c(year(data$date[1]), month(data$date[1])), frequency = 12)

# Forecast 12 periods into the future
forecast_data <- forecast(ts_data, h = 12)

# Plot 
ggplot() + 
  geom_line(aes(x = time(ts_data), y = ts_data), color = "seagreen") +
  geom_ribbon(aes(x = time(forecast_data$mean), ymax = forecast_data$lower[,1]*-1, ymin = forecast_data$upper[,1]*-1), alpha = 0.3, fill = "lightpink2") +
  geom_line(aes(x = time(forecast_data$mean), y = forecast_data$mean*-1), color = "lightpink4") +
  labs(title = "GSCPI Forecasting Results:\nAll Time", x = "Time", y = "Global Supply Chain Pressure Index\n(GSCPI)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))
  

upper_bound = min(forecast_data$lower)*-1
lower_bound = max(forecast_data$upper)*-1
# Could index any mean, should be the same
average = forecast_data$mean[1]*-1 

upper_bound
lower_bound
average
# Cross checking average is the same
(upper_bound+lower_bound)/2 

# Truncate data to last 48 months
recent_data <- tail(data, 48)

# Create a time series object with the predictor variable in column 2
ts_data <- ts(recent_data$index, start = c(year(recent_data$date[1]), month(recent_data$date[1])), frequency = 12)

# Forecast 12 periods into the future
forecast_data <- forecast(ts_data, h = 12)

# Plot 
ggplot() + 
  geom_line(aes(x = time(ts_data), y = ts_data), color = "seagreen") +
  geom_ribbon(aes(x = time(forecast_data$mean), ymax = forecast_data$lower[,1]*-1, ymin = forecast_data$upper[,1]*-1), alpha = 0.3, fill = "lightpink2") +
  geom_line(aes(x = time(forecast_data$mean), y = forecast_data$mean*-1), color = "lightpink4") +
  labs(title = "GSCPI Forecasting Results:\n4 Years", x = "Time", y = "Global Supply Chain Pressure Index\n(GSCPI)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

# Pull data
upper_bound = min(forecast_data$lower)*-1
lower_bound = max(forecast_data$upper)*-1
# Could index any mean, should be the same
average = forecast_data$mean[1]*-1 

upper_bound
lower_bound
average
# Cross checking average is the same
(upper_bound+lower_bound)/2 

