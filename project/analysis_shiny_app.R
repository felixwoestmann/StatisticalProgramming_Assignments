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

source('load_data.R', local = TRUE)
source('tab2_ui_declaration.R', local = TRUE)

mapbox_token <- 'pk.eyJ1Ijoid29lc3RtYW5uIiwiYSI6ImNsYjBxeDQ3NTB1YzEzc21saGx2c3hqMTEifQ.Szpy3fIYLgIWNZkdFU5PHg'
Sys.setenv('MAPBOX_TOKEN' = mapbox_token)

stations <- loadStaticStationData()
weather_data <- loadWeatherData()
journeys <- loadJourneyData()
journeys <- combineWeatherAndJourneyData(journeys, weather_data)

## RENEE is doing stuff here ######################

# goal: barplot with 24 bins for every hour showing average distance that was biked (in data time period)
# - set to day of the week
# - set to good or bad weather (blue is hour is bad, orange if hour is good) / OR MAYBE color shows good or bad
# weather but 4 colors for 4 different weather types/combinations

# 6. make it change according to the slider (1=monday)
# check felix's exploratory shiny to tackle this barplot

disdata <- subset(journeys, select = c("timestamp_start", "distance_meters", "weekday", "avg_temperature_celsisus",
                                       "precipitation_mm"))

disdata$hour <- as.factor(substr(disdata$timestamp,
                                 start = 12, stop = 13))
disdata$weekday <- as.factor(disdata$weekday)

disdata$rain <- disdata$precipitation_mm > 0
disdata$cold <- disdata$avg_temperature_celsisus < 5
disdata$goodweather <- disdata$rain == FALSE & disdata$cold == FALSE

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
               wellPanel(
                 h2('Instructions'),
                 p('This tab allows the user to interactively analyze data about weather and the amount of
               journeys conducted.'),
                 shiny::tags$li('- In the first step the user can filter the data based on some paramters. '),
                 shiny::tags$li('- In the second step the user can see intersection of the sets below.')
               ),
               tab2UITop(),
               hr(),
               h1("▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼"),
               tab2UIBottom(),
      ),
      tabPanel('3. Journey Distance',
               mainPanel(sidebarLayout(
                 
                 sidebarPanel(
                 h3("Weather type"),
                 checkboxInput('showGoodweather', label = 'Good Weather', value = TRUE),
                 checkboxInput('showBadweather', label = 'Bad Weather', value = TRUE)),
                                       mainPanel(
                                         plotOutput('DistanceBarplot')),
             
                sidebarPanel(
                h3("Day of the week"),
                sliderInput('dayoftheweek', 
                            label = "Day of the week", 
                            min = 1, max = 7, value = 1,
                            step = 1)),
                                        mainPanel(
                                          plotOutput('DistanceBarplot2'))

    ))),
    width = 12
  )))



# SERVER ------------

server <- function(input, output) {
  # Import render functions for tab 2 ------------------------------------------------------------------------
  # This is helpful so we don't fuck up the other ones code whil collaborating
  source('tab2_renderFunctions.R', local = TRUE)

# tab1 
  
  output$popStationsHistogram <- renderPlot({
    numberOfStations <- input$numberOfStations
  })


# tab2
  
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

  output$DistanceBarplot2 <- renderPlot({
    
    ggplot()+
      geom_bar(data = disdata %>% 
                 group_by(hour) %>% 
                 summarise(meandistance = mean(distance_meters)),
               aes(y = meandistance, x = hour),
               stat= "identity")
  })
}

# CALL

shinyApp(ui, server)

