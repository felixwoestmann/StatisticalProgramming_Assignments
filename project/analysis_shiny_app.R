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
library(zoo)

mapbox_token <- 'pk.eyJ1Ijoid29lc3RtYW5uIiwiYSI6ImNsYjBxeDQ3NTB1YzEzc21saGx2c3hqMTEifQ.Szpy3fIYLgIWNZkdFU5PHg'
Sys.setenv('MAPBOX_TOKEN' = mapbox_token)

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
weather_data <- read.table('data/weather_data_ljubljana.csv',
                           sep = ',',
                           header = T)
colnames(weather_data) <- c('timestamp', 'avg_temperature_celsisus', 'precipitation_mm')

weather_data$timestamp <- as.POSIXct(weather_data$timestamp,
                                     format = "%Y-%m-%d %H:%M")

# Interpolate NA values in precipitation
weather_data$precipitation_mm <- na.approx(weather_data$precipitation_mm, na.rm = FALSE)

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
journeys <- journeys[, -15]

journeysGroupedByTime <- function(breaks) {
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

# SHINY APP -----------------------------------------------------------------

# USER INTERFACE ------------

ui <- fluidPage(
  titlePanel("BicikeLJ"),
  mainPanel(
    tabsetPanel(
      tabPanel('1. Popular Stations',
               sliderInput('numberOfStations', label = "", min = 1, max = 10, value = 5),
               plotOutput('popStationsHistogram'),
      ),
      tabPanel('2. Weather and Journeys',

               mainPanel(sidebarLayout(sidebarPanel(
                 h3("Rain level"),
                 checkboxInput('showRain', label = 'Show Rain', value = TRUE),
                 checkboxInput('showNoRain', label = 'Show No Rain', value = TRUE)),
                                       mainPanel(
                                         plotOutput('weatherAndJourneys')),),),
               mainPanel(sidebarLayout(sidebarPanel(
                 h3("Daytype"),
                 checkboxInput('showWeekday', label = 'Show Weekday', value = TRUE,),
                 checkboxInput('showWeekend', label = 'Show Weekend', value = TRUE,)),
                                       mainPanel(
                                         plotOutput('weatherAndJourneysAndDayType'),))),
               mainPanel(sidebarLayout(sidebarPanel(
                 h3("Daytime"),
                 checkboxInput('showMorning', label = 'Show Morning', value = TRUE,),
                 checkboxInput('showAfternoon', label = 'Show Afternoon', value = TRUE,),
                 checkboxInput('showEvening', label = 'Show Evening', value = TRUE,),
                 checkboxInput('showNight', label = 'Show Night', value = TRUE,)),
                                       mainPanel(
                                         plotOutput('weatherAndDayTime'),))),
               mainPanel(sidebarLayout(sidebarPanel(
                 h3("Combined view of selected vars from above"),),
                                       mainPanel(
                                         plotOutput('combinedDayTimeAndDaytype'),))),
      
               fluidRow(
                 column(4, h3("Rain level"),
                        checkboxInput('showRain', label = 'Show Rain', value = TRUE),
                        checkboxInput('showNoRain', label = 'Show No Rain', value = TRUE),
                        plotOutput('weatherAndJourneys')),
                 column(4, h3("Daytype"),
                        checkboxInput('showWeekday', label = 'Show Weekday', value = TRUE,),
                        checkboxInput('showWeekend', label = 'Show Weekend', value = TRUE,),
                        plotOutput('weatherAndJourneysAndDayType')),
                 column(4, h3("Daytime"),
                        checkboxInput('showMorning', label = 'Show Morning', value = TRUE,),
                        checkboxInput('showAfternoon', label = 'Show Afternoon', value = TRUE,),
                        checkboxInput('showEvening', label = 'Show Evening', value = TRUE,),
                        checkboxInput('showNight', label = 'Show Night', value = TRUE,),
                        plotOutput('weatherAndDayTime'),
                 )),
               hr(),
               plotOutput('combinedDayTimeAndDaytype'),
      )
    ),
      
      tabPanel('3. Journey Distance',
              sliderInput('dayoftheweek', label = "", min = 1, max = 7, value = 1),
              checkboxInput('showGoodweather', label = 'Show Good Weather', value = TRUE),
              checkboxInput('showBadweather', label = 'Show Bad Weather', value = TRUE),
              plotOutput('DistanceHistogram'))
               
    ),
      width = 12
  )


# SERVER ------------

server <- function(input, output) {
  
  output$popStationsHistogram <- renderPlot({
    numberOfStations <- input$numberOfStations
  })

  output$weatherAndJourneys <- renderPlot({
    showRain <- TRUE
    showNoRain <- TRUE
    showRain <- input$showRain
    showNoRain <- input$showNoRain

    journeys_grouped <- journeysGroupedByTime('20 min')

    journeys_grouped$rainLevel <- ifelse(journeys_grouped$mean_precipitation > 0, "Rain", "No Rain")
    journeys_grouped$rainLevel <- factor(journeys_grouped$rainLevel, levels = c("Rain", "No Rain"))
    # Filter based on Checkbox
    journeys_grouped <- journeys_grouped %>%
      filter(rainLevel == "Rain" & showRain == TRUE |
               rainLevel == "No Rain" & showNoRain == TRUE)
    ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = rainLevel)) +
      geom_point(size = 2, shape = 23)
  })
  output$weatherAndJourneysAndDayType <- renderPlot({
    showWeekday <- TRUE
    showWeekend <- TRUE
    showWeekday <- input$showWeekday
    showWeekend <- input$showWeekend

    journeys_grouped <- journeysGroupedByTime('20 min')

    # Calc limits before filtering so its not affected by selected vars
    max_temp <- max(journeys_grouped$mean_temperature)
    min_temp <- min(journeys_grouped$mean_temperature)
    max_n <- max(journeys_grouped$n)
    min_n <- min(journeys_grouped$n)

    journeys_grouped$daytype <- ifelse(wday(journeys_grouped$chunks, label = TRUE) %in% c("Sat", "Sun"), "Weekend",
                                       "Weekday")

    journeys_grouped$daytype <- factor(journeys_grouped$daytype, levels = c("Weekday", "Weekend"))
    # Filter based on Checkbox
    journeys_grouped <- journeys_grouped %>%
      filter(daytype == "Weekday" & showWeekday == TRUE |
               daytype == "Weekend" & showWeekend == TRUE)


    ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = daytype)) +
      geom_point(size = 2, shape = 23) +
      lims(x = c(min_temp, max_temp), y = c(min_n, max_n))
  })

  output$weatherAndDayTime <- renderPlot({
    showMorning <- TRUE
    showAfternoon <- TRUE
    showEvening <- TRUE
    showNight <- TRUE
    showMorning <- input$showMorning
    showAfternoon <- input$showAfternoon
    showEvening <- input$showEvening
    showNight <- input$showNight

    journeys_grouped <- journeysGroupedByTime('20 min')

    # Calc limits before filtering so its not affected by selected vars
    max_temp <- max(journeys_grouped$mean_temperature)
    min_temp <- min(journeys_grouped$mean_temperature)
    max_n <- max(journeys_grouped$n)
    min_n <- min(journeys_grouped$n)

    journeys_grouped$daytime <- ifelse(hour(journeys_grouped$chunks) %in% 6:11, "Morning",
                                       ifelse(hour(journeys_grouped$chunks) %in% 12:17, "Afternoon",
                                              ifelse(hour(journeys_grouped$chunks) %in% 18:23, "Evening",
                                                     ifelse(hour(journeys_grouped$chunks) %in% 0:5, "Night", NA))))

    journeys_grouped$daytime <- factor(journeys_grouped$daytime, levels = c("Morning", "Afternoon", "Evening", "Night"))
    # Filter based on Checkbox
    journeys_grouped <- journeys_grouped %>%
      filter(daytime == "Morning" & showMorning == TRUE |
               daytime == "Afternoon" & showAfternoon == TRUE |
               daytime == "Evening" & showEvening == TRUE |
               daytime == "Night" & showNight == TRUE)


    ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = daytime)) +
      geom_point(size = 2, shape = 23) +
      lims(x = c(min_temp, max_temp), y = c(min_n, max_n))
  })

  output$combinedDayTimeAndDaytype <- renderPlot({
    showWeekday <- TRUE
    showWeekend <- TRUE
    showWeekday <- input$showWeekday
    showWeekend <- input$showWeekend

    showMorning <- TRUE
    showAfternoon <- TRUE
    showEvening <- TRUE
    showNight <- TRUE
    showMorning <- input$showMorning
    showAfternoon <- input$showAfternoon
    showEvening <- input$showEvening
    showNight <- input$showNight

    showRain <- TRUE
    showNoRain <- TRUE
    showRain <- input$showRain
    showNoRain <- input$showNoRain

    journeys_grouped <- journeysGroupedByTime('20 min')
    # Calc limits before filtering so its not affected by selected vars
    max_temp <- max(journeys_grouped$mean_temperature)
    min_temp <- min(journeys_grouped$mean_temperature)
    max_n <- max(journeys_grouped$n)
    min_n <- min(journeys_grouped$n)

    journeys_grouped$daytime <- ifelse(hour(journeys_grouped$chunks) %in% 6:11, "Morning",
                                       ifelse(hour(journeys_grouped$chunks) %in% 12:17, "Afternoon",
                                              ifelse(hour(journeys_grouped$chunks) %in% 18:23, "Evening",
                                                     ifelse(hour(journeys_grouped$chunks) %in% 0:5, "Night", NA))))

    journeys_grouped$daytime <- factor(journeys_grouped$daytime, levels = c("Morning", "Afternoon", "Evening", "Night"))

    journeys_grouped$daytype <- ifelse(wday(journeys_grouped$chunks, label = TRUE) %in% c("Sat", "Sun"), "Weekend",
                                       "Weekday")

    journeys_grouped$daytype <- factor(journeys_grouped$daytype, levels = c("Weekday", "Weekend"))

    journeys_grouped$rainLevel <- ifelse(journeys_grouped$mean_precipitation > 0, "Rain", "No Rain")
    journeys_grouped$rainLevel <- factor(journeys_grouped$rainLevel, levels = c("Rain", "No Rain"))

    # Combine daytime and daytype
    journeys_grouped$daytimeAndDaytypeAndRain <- paste(journeys_grouped$daytime, journeys_grouped$daytype,
                                                       journeys_grouped$rainLevel, sep = "-")

    journeys_grouped <- journeys_grouped %>%
      filter(daytype == "Weekday" & showWeekday == TRUE |
               daytype == "Weekend" & showWeekend == TRUE) %>%
      filter(daytime == "Morning" & showMorning == TRUE |
               daytime == "Afternoon" & showAfternoon == TRUE |
               daytime == "Evening" & showEvening == TRUE |
               daytime == "Night" & showNight == TRUE) %>%
      filter(rainLevel == "Rain" & showRain == TRUE |
               rainLevel == "No Rain" & showNoRain == TRUE)

    ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = daytimeAndDaytypeAndRain)) +
      geom_point(size = 2, shape = 23) +
      lims(x = c(min_temp, max_temp), y = c(min_n, max_n))

  })
  
  output$DistanceHistogram <- renderPlot({
    ggplot()
  })
  
}

# CALL

shinyApp(ui, server)