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
library(wesanderson)


source('load_data.R', local = TRUE)
source('tabContents/tab1_ui_declaration.R', local = TRUE)
source('tabContents/tab2_ui_declaration.R', local = TRUE)
source('tabContents/tab3_ui_declaration.R', local = TRUE)

mapbox_token <- 'pk.eyJ1Ijoid29lc3RtYW5uIiwiYSI6ImNsYjBxeDQ3NTB1YzEzc21saGx2c3hqMTEifQ.Szpy3fIYLgIWNZkdFU5PHg'
Sys.setenv('MAPBOX_TOKEN' = mapbox_token)

stations <- loadStaticStationData()
weather_data <- loadWeatherData()
journeys <- loadJourneyData()
journeys <- combineWeatherAndJourneyData(journeys, weather_data)

### ADDITIONAL DATA MANIPULATION ###

disdata <- subset(journeys, select = c("timestamp_start", "distance_meters", "weekday", "avg_temperature_celsisus",
                                       "precipitation_mm"))

disdata$hour <- as.factor(substr(disdata$timestamp,
                                 start = 12, stop = 13))
disdata$weekday <- as.factor(disdata$weekday)
mean(disdata$avg_temperature_celsisus)

disdata$rain <- disdata$precipitation_mm > 0
disdata$cold <- disdata$avg_temperature_celsisus < 5.3
disdata$goodweather <- disdata$rain == FALSE & disdata$cold == FALSE

weekdays <- c("Monday", "Tuesday", "Wednesday",
              "Thursday", "Friday", "Saturday", "Sunday")

####################################


# SHINY APP -----------------------------------------------------------------

# USER INTERFACE


ui <- fluidPage(
  titlePanel("BicikeLJ"),
  mainPanel(
    tabsetPanel(
      tab1(),
      tab2(),
      tab3(),
    ),
    width = 12)
)

# SERVER

server <- function(input, output) {

  source('tabContents/tab_1_renderFunctions.R', local = TRUE)
  source('tabContents/tab2_renderFunctions.R', local = TRUE)
  source('tabContents/tab3_renderFunctions.R', local = TRUE)

}

# CALL

shinyApp(ui, server)

