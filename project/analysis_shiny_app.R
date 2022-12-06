library(shiny)
library(shinyWidgets)
library(RSQLite)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(treemap)
library(plotly)
library(glue)
library(stringr)

mapbox_token <- 'pk.eyJ1Ijoid29lc3RtYW5uIiwiYSI6ImNsYjBxeDQ3NTB1YzEzc21saGx2c3hqMTEifQ.Szpy3fIYLgIWNZkdFU5PHg'
Sys.setenv('MAPBOX_TOKEN' = mapbox_token)

# LOAD OBSERVATION DATA TO DO QUALITY CONTROL ----------------------------------
observationConn <- dbConnect(SQLite(), "data/20221201_bike_observations.db")
observations <- dbGetQuery(observationConn,
                           "SELECT id,
                           Timestamp as timestamp,
                           bikeNumber as bike_number
                           FROM BikeObservations
                           WHERE id % 3 = 0
                           AND Timestamp > '2022-11-15 15:00:00'
                           AND Timestamp < '2022-11-30 00:00:00'")
# loading times
unique_bikes <- dbGetQuery(observationConn, "SELECT COUNT(DISTINCT bikeNumber) FROM BikeObservations")
unique_bikes <- unique_bikes[[1]]

observations$timestamp <- as.POSIXct(observations$timestamp,
                                     format = "%Y-%m-%d %H:%M:%S")

# LOAD STATIC STATION DATA ----------------------------------------------------
stations <- read.table('data/ljubljana_station_data_static.csv',
                       sep = ',',
                       header = T)
stations <- stations[, -3] # Remove address clolumn
colnames(stations) <- c('number', 'name', 'lat', 'lon')
# LOAD JOURNEY DATA -----------------------------------------------------------
# When plotting our data we can see that there is a massive spike of journeys around 15.11 9:00 am
# We saw that while looking at the quality of our data. Therefore we will remove data from the start till 15.11 12am
journeyConn <- dbConnect(SQLite(), "data/20221201_journey.db")
journeys <- dbGetQuery(journeyConn,
                       "SELECT *
                        FROM Journeys
                        WHERE timestampStart > '2022-11-15 15:00:00'
                        AND timestampEnd > '2022-11-15 15:00:00'
                        AND timestampStart < '2022-11-30 00:00:00'")

colnames(journeys) <- c('id', 'timestamp_start', 'timestamp_end',
                        'bike_number', 'station_start', 'station_end',
                        'location_start_lat', 'location_start_lon',
                        'location_end_lat', 'location_end_lon',
                        'distance_meters', 'time_minutes')

journeys$timestamp_start <- as.POSIXct(journeys$timestamp_start,
                                       format = "%Y-%m-%d %H:%M:%S")
journeys$timestamp_end <- as.POSIXct(journeys$timestamp_end,
                                     format = "%Y-%m-%d %H:%M:%S")
journeys$weekday <- wday(journeys$timestamp_start, label = TRUE)
journeys$is_weekend <- journeys$weekday %in% c("Sat", "Sun")
# LOAD WEATHER DATA -----------------------------------------------------------
weather_data <- read.table('data/weather_data_ljubljana.csv', sep = ',',
                           header = T)
colnames(weather_data) <- c('timestamp', 'avg_temperature_celsisus')

weather_data$timestamp <- as.POSIXct(weather_data$timestamp, format =
  "%Y-%m-%d %H:%M")


# ADD WEATHER DATA TO JOURNEYS ------------------------------------------------
# We are creating journeys$timestampe as a dummy variable to easier merge two
# dataframes
# Then we are creating an empty column to hold the tempretrue data
# setDT... combines the dataframes, roll=nearest matches the timestamp to the
# nearest timestamp in the weather data

journeys$timestamp <- journeys$timestamp_start
journeys[, 'avg_temperature_celsisus'] <- NA
setDT(journeys)[, avg_temperature_celsisus := setDT(weather_data)[journeys,
                                                                  avg_temperature_celsisus,
                                                                  on = "timestamp",
                                                                  roll = "nearest"]]
# Delete the dummy timestamp column
journeys <- journeys[, -15]


ui <- fluidPage(
  titlePanel("BicikeLJ"),
  mainPanel(
  ),
  width = 12
)


server <- function(input, output) { }


shinyApp(ui, server)