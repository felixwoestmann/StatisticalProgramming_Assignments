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

# LOAD OBSERVATION DATA TO DO QUALITY CONTROL ------------------------------
observationConn <- dbConnect(SQLite(), "data/20221207_bike_observations.db")
observations <- dbGetQuery(observationConn,
                           "SELECT id,
                           Timestamp as timestamp,
                           bikeNumber as bike_number
                           FROM BikeObservations
                           WHERE id % 3 = 0
                           AND Timestamp > '2022-11-15 15:00:00'
                           AND Timestamp < '2022-12-06 00:00:00'")
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

# START SHINY CODE ------------------------------------------------------------

seven_value_color_palette <- c("#F94144", "#F3722C", "#F9C74F",
                               "#90BE6D", "#43AA8B", "#577590", "#277DA1")

overViewTabPanel <- function() {
  return(
    tabPanel('Overview Journey Table',
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
             plotOutput('weatherPlot', width = '100%', height = '400px')
    )
  )
}

popularStationsPanel <- function() {
  return(
    tabPanel('Popular Stations',
             splitLayout(
               cellWidths = c("50%", "50%"),
               plotOutput('popularStationsBarPlot'),
               plotOutput('popularStationsTreePlot')),
             plotlyOutput('popularStationsMapPlot'),)
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

journeyTemperaturePanel <- function() {
  return(
    tabPanel('Journey Temperature',
             sidebarLayout(
               sidebarPanel(
                 sliderTextInput('journeyTemperatureSlider',
                                 'Select Time Bucket',
                                 choices = c('1 hour',
                                             '2 hour',
                                             '4 hour',
                                             '8 hour',
                                             '12 hour',
                                             '24 hour'),
                 ),
               ),
               mainPanel(
                 plotOutput('journeyTemperaturePlot')
               )
             )
    )
  )
}

journeyTimeOfDayPanel <- function() {
  return(
    tabPanel('Journey Time of Day',
             plotOutput('journeyTimeOfDay'),
    )
  )
}

popularJourneysPanel <- function() {
  return(
    tabPanel('Popular Journeys',
             plotlyOutput('popularJourneysPanel'),
    )
  )
}

ui <- fluidPage(
  titlePanel("BicikeLJ"),
  mainPanel(
    tabsetPanel(
      overViewTabPanel(),
      timeBlocksTabPanel(),
      journeyByWeekdayPanel(),
      popularStationsPanel(),
      journeyTemperaturePanel(),
      journeyTimeOfDayPanel(),
      popularJourneysPanel()
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

    counted_chunks$weekday <- wday(counted_chunks$chunks, label = TRUE)

    barplot(counted_chunks$n,
            names.arg = substring(counted_chunks$chunks, 1, 16),
            ylab = "Number of journeys",
            main = "Number of journeys per half hour block",
            las = 2,
            border = NA,
            col = counted_chunks$weekday,
            density = 100,
            legend = TRUE,

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

  output$weatherPlot <- renderPlot({
    ggplot(weather_data, aes(x = timestamp, y = avg_temperature_celsisus,)) +
      ggtitle("Average air temperatur Ljubljana") +
      xlab("Time") +
      ylab("Temperature in °C") +
      geom_line()
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

  output$popularStationsTreePlot <- renderPlot({
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

  output$popularStationsMapPlot <- renderPlotly({
    # Group journeys by station_start and count those
    popular_stations <- journeys %>%
      group_by(station_start) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(10)

    # merge popular_stations together with stations to obtain the name
    popular_stations <- merge(popular_stations,
                              stations[, c('number', 'name', 'lat', 'lon')],
                              by.x = "station_start",
                              by.y = "number")

    plot_mapbox(popular_stations) %>%
      add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
      layout(
        mapbox = list(style = "dark", zoom = 12,
                      center = list(lon = 14.5, lat = 46.05))) %>%
      add_markers(
        x = ~lon,
        y = ~lat,
        size = ~n,
        colors = "Accent",
        text = ~name,
        hoverinfo = "text",
        showlegend = FALSE,
      )

  })


  output$journeyTemperaturePlot <- renderPlot({
    binSize <- input$journeyTemperatureSlider
    journeys_by_temperature <- journeys
    journeys_by_temperature$chunks <- cut(journeys_by_temperature$timestamp_start, breaks = binSize)
    journeys_by_temperature$chunks <- as.POSIXct(journeys_by_temperature$chunks,
                                                 format = "%Y-%m-%d %H:%M:%S")
    journeys_by_daytime <- journeys_by_temperature %>%
      group_by(chunks) %>%
      summarise(mean_temperature = mean(avg_temperature_celsisus),
                n = n())

    # add a column with the hour of the day
    journeys_by_daytime$hour <- as.numeric(format(journeys_by_daytime$chunks, "%H"))
    ## add morning, afternoon, evening and night
    ## 7am sunrise on 14th of November
    journeys_by_daytime$daytime <- ifelse(journeys_by_daytime$hour >= 6 & journeys_by_daytime$hour < 18,
                                          "day", "night")

    ggplot(journeys_by_daytime, aes(x = mean_temperature, y = n, color = daytime)) +
      geom_point(size = 2, shape = 23)
    # Add a 'ceiling' to the plot which represent the theoretical number of journeys possible at one point in time
    # This number is likely an overestimate, since it would count bikes which are replaced since they were broken
    # for example
  })

  output$journeyTimeOfDay <- renderPlot({
    journeys_by_daytime <- journeys
    journeys_by_daytime$timestamp_start <- as.POSIXct(journeys_by_daytime$timestamp_start,
                                                      format = "%Y-%m-%d %H:%M:%S")
    journeys_by_daytime$hour <- as.numeric(format(journeys_by_daytime$timestamp_start, "%H"))
    journeys_by_daytime$day <- as.numeric(format(journeys_by_daytime$timestamp_start, "%d"))

    journeys_by_daytime$daytime <- ifelse(journeys_by_daytime$hour >= 7 & journeys_by_daytime$hour < 12,
                                          "morning",
                                          ifelse(journeys_by_daytime$hour >= 12 &
                                                   journeys_by_daytime$hour < 18,
                                                 "afternoon",
                                                 ifelse(journeys_by_daytime$hour >= 18 &
                                                          journeys_by_daytime$hour < 24,
                                                        "evening",
                                                        "night")))

    journeys_by_daytime <- journeys_by_daytime %>%
      group_by(day, daytime) %>%
      summarise(n = n())

    ggplot(journeys_by_daytime, aes(fill = daytime, y = n, x = day)) +
      geom_bar(position = "stack", stat = "identity") +
      labs(y = "Number of journeys", x = "Day of November") +
      scale_x_continuous(breaks = round(seq(min(journeys_by_daytime$day), max(journeys_by_daytime$day), by = 1), 0)) +
      scale_fill_manual(values = seven_value_color_palette)
  }

  )

  output$popularJourneysPanel <- renderPlotly({
    # group dataframe so that it doesnt matter if a station was start or end
    popular_journeys <- journeys %>%
      group_by(station_one = pmin(station_start, station_end), station_two = pmax(station_start, station_end)) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      mutate(
        station_start_name = stations[match(station_one, stations$number), "name"],
        station_end_name = stations[match(station_two, stations$number), "name"],
        latStart = stations[match(station_one, stations$number), "lat"],
        lonStart = stations[match(station_one, stations$number), "lon"],
        latEnd = stations[match(station_two, stations$number), "lat"],
        lonEnd = stations[match(station_two, stations$number), "lon"]
      )

    plot_mapbox(popular_journeys) %>%
      add_segments(x = -100,
                   xend = -50,
                   y = 50,
                   yend = 75) %>%
      layout(
        mapbox = list(style = "dark",
                      zoom = 12,
                      center = list(lon = 14.5, lat = 46.05))) %>%
      add_segments(
        x = ~lonStart, xend = ~lonEnd,
        y = ~latStart, yend = ~latEnd,
        colors = "Accent",
        text = ~n,
        hoverinfo = "text") %>%
      add_markers(
        x = ~lonStart,
        y = ~latStart,
        colors = "green",
        hoverinfo = "text",
        showlegend = FALSE,) %>%
      add_markers(
        x = ~lonEnd,
        y = ~latEnd,
        colors = "green",
        hoverinfo = "text",
        showlegend = FALSE,) %>%
      config(mapboxAccessToken = mapbox_token)
  })

}


shinyApp(ui, server)