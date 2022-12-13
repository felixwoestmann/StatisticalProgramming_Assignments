# Load Data functions ----------------------------------------------------------
loadStaticStationData <- function() {
  # LOAD STATIC STATION DATA ----------------------------------------------------
  stations <- read.table('data/ljubljana_station_data_static.csv',
                         sep = ',',
                         header = T)
  stations <- stations[, -3] # Remove address clolumn
  colnames(stations) <- c('number', 'name', 'lat', 'lon')
  return(stations)
}

loadJourneyData <- function() {
  # LOAD JOURNEY DATA -----------------------------------------------------------
  # When plotting our data we can see that there is a massive spike of journeys around 15.11 9:00 am
  # We saw that while looking at the quality of our data. Therefore we will remove data from the start till 15.11 12am
  journeyConn <- dbConnect(SQLite(), "data/20221207_journey.db")
  journeys <- dbGetQuery(journeyConn,
                         "SELECT *
                          FROM Journeys
                          WHERE timestampStart > '2022-11-15 15:00:00'
                          AND timestampEnd > '2022-11-15 15:00:00'
                          AND timestampStart < '2022-12-06 00:00:00'")

  colnames(journeys) <- c('id', 'timestamp_start', 'timestamp_end',
                          'bike_number', 'station_start', 'station_end',
                          'location_start_lat', 'location_start_lon',
                          'location_end_lat', 'location_end_lon',
                          'distance_meters', 'time_minutes')

  dbDisconnect(journeyConn)

  journeys$timestamp_start <- as.POSIXct(journeys$timestamp_start,
                                         format = "%Y-%m-%d %H:%M:%S")
  journeys$timestamp_end <- as.POSIXct(journeys$timestamp_end,
                                       format = "%Y-%m-%d %H:%M:%S")
  journeys$weekday <- wday(journeys$timestamp_start, label = TRUE)
  journeys$is_weekend <- journeys$weekday %in% c("Sat", "Sun")

  journeys$hour <- hour(journeys$timestamp_start)

  return(journeys)
}

loadWeatherData <- function() {
  # LOAD WEATHER DATA -----------------------------------------------------------
  weather_data <- read.table('data/weather_data_ljubljana.csv',
                             sep = ',',
                             header = T)
  colnames(weather_data) <- c('timestamp', 'avg_temperature_celsisus', 'precipitation_mm')

  weather_data$timestamp <- as.POSIXct(weather_data$timestamp,
                                       format = "%Y-%m-%d %H:%M")

  # Interpolate NA values in precipitation
  weather_data$precipitation_mm <- na.approx(weather_data$precipitation_mm, na.rm = FALSE)

  return(weather_data)
}

combineWeatherAndJourneyData <- function(journeys, weather_data) {
  # ADD WEATHER DATA TO JOURNEYS ------------------------------------------------
  # We are creating journeys$timestampe as a dummy variable to easier merge two
  # dataframes
  # Then we are creating an empty column to hold the tempretrue data
  # setDT... combines the dataframes, roll=nearest matches the timestamp to the
  # nearest timestamp in the weather data

  journeys$timestamp <- journeys$timestamp_start
  journeys[, 'avg_temperature_celsisus'] <- NA
  journeys[, 'precipitation_mm'] <- NA
  setDT(journeys)[, avg_temperature_celsisus := setDT(weather_data)[journeys,
                                                                    avg_temperature_celsisus,
                                                                    on = "timestamp",
                                                                    roll = "nearest"]]

  setDT(journeys)[, precipitation_mm := setDT(weather_data)[journeys,
                                                            precipitation_mm,
                                                            on = "timestamp",
                                                            roll = "nearest"]]

  # Delete the dummy timestamp column
  journeys <- journeys[, -16]

  return(journeys)
}

journeysGroupedByTime <- function(journeys, breaks) {
  journeys_by_temperature <- journeys
  journeys_by_temperature$chunks <- cut(journeys_by_temperature$timestamp_start, breaks = breaks)
  journeys_by_temperature$chunks <- as.POSIXct(journeys_by_temperature$chunks,
                                               format = "%Y-%m-%d %H:%M:%S")
  journeys_grouped <- journeys_by_temperature %>%
    group_by(chunks) %>%
    summarise(mean_temperature = mean(avg_temperature_celsisus),
              mean_precipitation = mean(precipitation_mm),
              n = n())

  return(journeys_grouped)
}