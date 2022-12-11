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

options(shiny.autoreload = TRUE)

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

## RENEE is doing stuff here ######################

# goal: barplot with 24 bins for every hour showing average distance that was biked (in data time period)
# - set to day of the week
# - set to good or bad weather (blue is hour is bad, orange if hour is good) / OR MAYBE color shows good or bad
# weather but 4 colors for 4 different weather types/combinations

# to do
# 1. make variable for hours
# 2. make variable goodweather, TRUE for good weather hour (= 0 percep and 5> celsisus), FALSE if not
# variable for rain TRUE or FALSE
# variable for 5> celsisus TRUE or FALSE
# combine in to goodweather
# 3. make a barplot that shows average distance per hour
# 4. put the barplot in shiny app
# 5. make it change according to checkbox (good weather/bad weather/double check)
# 6. make it change according to the slider (1=monday)
## sliderInput('dayoftheweek', label = "", min = 1, max = 7, value = 1)
# check felix's exploratory shiny to tackle this barplot

disdata <- subset(journeys, select = c("timestamp_start", "distance_meters", "weekday", "avg_temperature_celsisus",
                                       "precipitation_mm"))

disdata$hour <- as.factor(substr(disdata$timestamp,
                                 start = 12, stop = 13))

disdata$rain <- disdata$precipitation_mm > 0
disdata$cold <- disdata$avg_temperature_celsisus < 5
disdata$goodweather <- disdata$rain == FALSE & disdata$cold == FALSE

# output$DistanceBarplot <- renderPlot({
#   showGoodweather <- input$showGoodweather
#   showBadweather <- input$showBadweather
#
#   # Grouped distance
#
#   average_distance_hour <- disdata %>%
#     group_by(hour) %>%
#     summarise(meandistance = mean(distance_meters))
#
#   average_distance_hour$weather <- ifelse(disdata$goodweather == TRUE, "Good", "Bad")
#   average_distance_hour$weather <- factor(average_distance_hour$weather, levels = c("Good", "Bad"))
#
#
#   # Filter based on Checkbox
#   average_distance_hour <- average_distance_hour %>%
#     filter(average_distance_hour == "Bad" & showBadweather == TRUE |
#              average_distance_hour == "Good" & showGoodweather == TRUE)
#
#   ggplot()+
#     geom_bar(average_distance_hour, aes(y = meandistance, x = hour),
#              stat= "identity")
# })


## END of renees stuff for now ############################

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
               fluidRow(
                 column(4, h3("Rain or No Rain"),
                        plotOutput('weatherRain'),
                        sidebarPanel(
                          h4("Select rain level"),
                          checkboxInput('showRain', label = 'Show Rain', value = TRUE),
                          checkboxInput('showNoRain', label = 'Show No Rain', value = TRUE),
                          width = 12)),
                 column(4, h3("Weekday or Weekend"),
                        plotOutput('weatherWeekdayWeekend'),
                        sidebarPanel(
                          h4("Select type of day"),
                          checkboxInput('showWeekday', label = 'Show Weekday', value = TRUE,),
                          checkboxInput('showWeekend', label = 'Show Weekend', value = TRUE,),
                          width = 12)),
                 column(4, h3("Time of day"),
                        plotOutput('weatherTimeOfDay'),
                        sidebarPanel(
                          h4("Select time of day"),
                          fluidRow(
                            column(6,
                                   checkboxInput('showMorning', label = 'Show Morning', value = TRUE,),
                                   checkboxInput('showAfternoon', label = 'Show Afternoon', value = TRUE,)),
                            column(6,
                                   checkboxInput('showEvening', label = 'Show Evening', value = TRUE,),
                                   checkboxInput('showNight', label = 'Show Night', value = TRUE,))
                          ),
                          width = 12))
               ),
               hr(),
               h1("▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼"),
               plotOutput('weatherCombined'),
      ),

      tabPanel('3. Journey Distance',
               mainPanel(sidebarLayout(sidebarPanel(
                 h3("Weather type"),
                 checkboxInput('showGoodweather', label = 'Good Weather', value = TRUE),
                 checkboxInput('showBadweather', label = 'Bad Weather', value = TRUE)),
                                       mainPanel(
                                         plotOutput('DistanceBarplot'))))),

    ),
    width = 12
  ))


# SERVER ------------

server <- function(input, output) {
  # Import render functions for tab 2 ------------------------------------------------------------------------
  # This is helpful so we don't fuck up the other ones code whil collaborating
  source('tab_2.R', local = TRUE)


  output$popStationsHistogram <- renderPlot({
    numberOfStations <- input$numberOfStations
  })


  output$DistanceBarplot <- renderPlot({
    showGoodweather <- input$showGoodweather
    showBadweather <- input$showBadweather

    disdata$goodweather <- as.factor(disdata$goodweather)

    disdata <- disdata %>%
      filter(disdata$goodweather == "FALSE" & showBadweather == TRUE |
               disdata$goodweather == "TRUE" & showGoodweather == TRUE)

    ggplot() +
      geom_bar(data = disdata %>%
        group_by(hour) %>%
        summarise(meandistance = mean(distance_meters)),
               aes(y = meandistance, x = hour),
               stat = "identity")
  })

}

# CALL

shinyApp(ui, server)

