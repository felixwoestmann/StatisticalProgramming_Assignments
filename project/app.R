library(shiny)
library(RSQLite)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(treemap)

# LOAD OBSERVATION DATA TO DO QUALITY CONTROL ----------------------------------
observationConn <- dbConnect(SQLite(), "data/20221127_bike_observations.db")
observations <- dbGetQuery(observationConn,
                           "SELECT id,
                           Timestamp as timestamp,
                           bikeNumber as bike_number
                           FROM BikeObservations
                           WHERE id % 3 = 0") # only use every third observation as a means of sampling and to aid loading times

observations$timestamp <- as.POSIXct(observations$timestamp, format =
  "%Y-%m-%d %H:%M:%S")

# LOAD STATIC STATION DATA ----------------------------------------------------
stations <- read.table('data/ljubljana_station_data_static.csv', sep = ',',
                       header = T)
stations <- stations[, -3] # Remove address clolumn
colnames(stations) <- c('number', 'name', 'lat', 'lon')
# LOAD JOURNEY DATA -----------------------------------------------------------
journeyConn <- dbConnect(SQLite(), "data/20221127_journey.db")
journeys <- dbGetQuery(journeyConn, "SELECT * FROM Journeys")

colnames(journeys) <- c('id', 'timestamp_start', 'timestamp_end',
                        'bike_number', 'station_start', 'station_end',
                        'location_start_lat', 'location_start_lon',
                        'location_end_lat', 'location_end_lon',
                        'distance_meters', 'time_minutes')

journeys$timestamp_start <- as.POSIXct(journeys$timestamp_start, format =
  "%Y-%m-%d %H:%M:%S")
journeys$timestamp_end <- as.POSIXct(journeys$timestamp_end, format =
  "%Y-%m-%d %H:%M:%S")
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

# START SHINY CODE ------------------------------------------------------------

seven_value_color_palette <- c("#F94144", "#F3722C", "#F9C74F",
                               "#90BE6D", "#43AA8B", "#577590", "#277DA1")

overViewTabPanel <- function() {
  return(
    tabPanel('Overview Jpurney Table',
             tableOutput('overviewTable'),

    )
  )
}

timeBlocksTabPanel <- function() {
  return(
    tabPanel('Data Quality Control',
             p('We are grouping our observations and journeys into 30 minute blocks to control if there are any
             anomalies in the data.'),
             p('It can be seen that in the Observations we are missing some data at the start around the 14th and
             15th of November 2022. This anomaly shows up in our journey data again and leads to a spike in the
             journeys. To combat this issue we should probably chnage the scope of the analysis and leave this data
             out.'),
             p('Beside this the data looks totally fine, from our server logs which documented failing scraping
             requests we know that one request failed around every 2 hours. Considering we were doing 120 Minutes / 3
              Minutes between requests for 84 station = 3360 in two hours this is a percentage of 0.02% which is
              totally fine.'),
             plotOutput('timeBlocksObservations', width = '100%', height = '400px'),
             plotOutput('timeBlocksJourneys', width = '100%', height = '400px'),
    )
  )
}

popularStationsPanel <- function() {
  return(
    tabPanel('Popular Stations',
             plotOutput('popularStationsBarPlot'),
             plotOutput('popularStationsMosaicPlot'),
             plotOutput('popularStationsMap'),)
  )
}

journeyByWeekdayPanel <- function() {
  return(
    tabPanel('Journeys by Weekday',
             plotOutput('journeyByWeekday'),
             plotOutput('journeyByWeekend'),
    )
  )
}

ui <- fluidPage(
  titlePanel("Titlepanel"),
  mainPanel(
    tabsetPanel(
      overViewTabPanel(),
      timeBlocksTabPanel(),
      journeyByWeekdayPanel(),
      popularStationsPanel(),
    ),
    width = 12
  ),
)

# write shiny app
server <- function(input, output) {
  output$overviewTable <- renderTable({
    head(journeys)
  })

  output$timeBlocksJourneys <- renderPlot({

    journeys$chunks <- cut(journeys$timestamp_start, breaks = "30 min")
    counted_chunks <- count(journeys, , chunks)
    counted_chunks$chunks <- as.POSIXct(counted_chunks$chunks, format =
      "%Y-%m-%d %H:%M:%S")

    color_vector <- wday(counted_chunks$chunks, label = TRUE)
    color_vector <- as.character(color_vector)
    color_vector[color_vector == "Mon"] <- seven_value_color_palette[1]
    color_vector[color_vector == "Tue"] <- seven_value_color_palette[2]
    color_vector[color_vector == "Wed"] <- seven_value_color_palette[3]
    color_vector[color_vector == "Thu"] <- seven_value_color_palette[4]
    color_vector[color_vector == "Fri"] <- seven_value_color_palette[5]
    color_vector[color_vector == "Sat"] <- seven_value_color_palette[6]
    color_vector[color_vector == "Sun"] <- seven_value_color_palette[7]

    barplot(counted_chunks$n,
            names.arg = substring(counted_chunks$chunks, 1, 16),
            ylab = "Number of journeys",
            main = "Number of journeys per half hour block",
            las = 2,
            border = NA,
            col = color_vector,
            density = 100,
            # TODO add legend
    )
  })

  output$timeBlocksObservations <- renderPlot({
    observations$chunks <- cut(observations$timestamp, breaks = "30 min")
    counted_chunks <- count(observations, , chunks)
    counted_chunks$chunks <- as.POSIXct(counted_chunks$chunks, format =
      "%Y-%m-%d %H:%M:%S")

    barplot(counted_chunks$n,
            names.arg = substring(counted_chunks$chunks, 1, 16),
            ylab = "Number of observations",
            main = "Number of observations per half hour block",
            las = 2,
    )

  })
  output$journeyByWeekday <- renderPlot({
    journeys_by_weekday <- journeys %>%
      group_by(weekday) %>%
      summarise(n = n())
    barplot(journeys_by_weekday$n,
            names.arg = journeys_by_weekday$weekday,
            ylab = "Number of journeys",
            main = "Number of journeys per weekday",
            las = 2,
    )
  })

  output$journeyByWeekend <- renderPlot({
    journeys_by_weekend <- journeys %>%
      group_by(is_weekend) %>%
      summarise(n = n())
    journeys_by_weekend$is_weekend <- as.factor(journeys_by_weekend$is_weekend)
    journeys_by_weekend$is_weekend <- ifelse(journeys_by_weekend$is_weekend == TRUE,
                                             "Weekend", "Weekday")
    barplot(journeys_by_weekend$n,
            names.arg = journeys_by_weekend$is_weekend,
            ylab = "Number of journeys",
            main = "Number of journeys by weekend / weekday",
            las = 2,
    )
  })


  output$popularStationsBarPlot <- renderPlot({
    # Group journeys by station_start and count those
    popular_stations <- journeys %>%
      group_by(station_start) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(10)

    # merge popular_stations together with stations to obtain the name
    popular_stations <- merge(popular_stations, stations[, c('number', 'name')], by.x = "station_start", by.y =
      "number")

    barplot(popular_stations$n,
            names.arg = popular_stations$name,
            ylab = "Number of journeys",
            main = "Number of journeys per station",
            las = 2,
    )
  })

  output$popularStationsMosaicPlot <- renderPlot({
    # Group journeys by station_start and count those
    popular_stations <- journeys %>%
      group_by(station_start) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(10)

    # merge popular_stations together with stations to obtain the name
    popular_stations <- merge(popular_stations, stations[, c('number', 'name')], by.x = "station_start", by.y =
      "number")

    treemap(popular_stations,
            title = "Treemap of the most popular stations",
            index = "name",
            vSize = "n",
            type = "index"
    )

  })

}


shinyApp(ui, server)