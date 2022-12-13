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

mapbox_token <- 'pk.eyJ1Ijoid29lc3RtYW5uIiwiYSI6ImNsYjBxeDQ3NTB1YzEzc21saGx2c3hqMTEifQ.Szpy3fIYLgIWNZkdFU5PHg'
Sys.setenv('MAPBOX_TOKEN' = mapbox_token)

stations <- loadStaticStationData()
weather_data <- loadWeatherData()
journeys <- loadJourneyData()
journeys <- combineWeatherAndJourneyData(journeys, weather_data)

###

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
###


# SHINY APP -----------------------------------------------------------------

# USER INTERFACE ------------


ui <- fluidPage(
  titlePanel("BicikeLJ"),
  mainPanel(
    tabsetPanel(
      tab1(),
      tab2(),
      tabPanel('3. Journey Distance',
               wellPanel(
                 h2('Instructions'),
                 p("This tab allows the user to interactively analyze data about the average distance of bicikelj travels"),
                shiny::tags$li('In the first step the user can filter the data based on good weather (no rain and more than 5.3C temperature)
                               or bad weather (either rain, below 5.3C temperature, or both). '),
                shiny::tags$li('In the second step the user can filter the data based on the day of the week.')),
                 
               sidebarPanel(
                   h3("Weather type"),
                   checkboxInput('showGoodweather', label = 'Good Weather', value = TRUE),
                   checkboxInput('showBadweather', label = 'Bad Weather', value = TRUE),
                   # actionButton('refresh', label = NULL, icon = icon("arrows-rotate"),
                   #              class="btn-xs", style = "float: right;")
                   ),
                 mainPanel(
                   plotOutput('DistanceBarplot')),

                sidebarPanel(
                  h3("Day of the week"),
                  sliderTextInput(inputId = 'dayoftheweek',
                    label = "",
                    choices = weekdays)),
                mainPanel(
                  plotOutput('DistanceBarplot2'))
                )
               ),
    width = 12)
             )

# SERVER ------------

server <- function(input, output) {
  # Import render functions for tab 2 ------------------------------------------------------------------------
  # This is helpful so we don't fuck up the other ones code while collaborating
  
  source('tabContents/tab_1_renderFunctions.R', local = TRUE)
  source('tabContents/tab2_renderFunctions.R', local = TRUE)
  
  # tab3

  output$DistanceBarplot <- renderPlot({
    showGoodweather <- input$showGoodweather
    showBadweather <- input$showBadweather

    disdata$goodweather <- as.factor(disdata$goodweather)
    # daytime <- disdata$hour

    disdata <- disdata %>%
      filter(disdata$goodweather == "FALSE" & showBadweather == TRUE |
               disdata$goodweather == "TRUE" & showGoodweather == TRUE)

    ggplot() +
        geom_bar(data = disdata %>%
        group_by(hour) %>%
        summarise(meandistance = mean(distance_meters)),
               aes(y = meandistance, x = hour),
               stat = "identity")+
        coord_cartesian(xlim = NULL, ylim = c(0, 3000), default = TRUE)
      # scale_color_manual(aesthetics = "fill",
      #                    values = c("Weekday" = wes_palette("Darjeeling1")[1],
      #                               "Weekend" = wes_palette("Darjeeling1")[2]))
  })


  output$DistanceBarplot2 <- renderPlot({
 
    disdata$weekday <- as.factor(disdata$weekday)
    slider_value <- input$dayoftheweek

    disdata <- disdata %>%
      filter(disdata$weekday == "Mon" & slider_value == "Monday" |
               disdata$weekday == "Tue" & slider_value == "Tuesday"|
               disdata$weekday == "Wed" & slider_value == "Wednesday"|
               disdata$weekday == "Thu" & slider_value == "Thursday"|
               disdata$weekday == "Fri" & slider_value == "Friday"|
               disdata$weekday == "Sat" & slider_value == "Saturday"|
               disdata$weekday == "Sun" & slider_value == "Sunday")


    ggplot() +
      geom_bar(data = disdata %>%
        group_by(hour) %>%
        summarise(meandistance = mean(distance_meters)),
               aes(y = meandistance, x = hour),
               stat = "identity")+
      coord_cartesian(xlim = NULL, ylim = c(0, 3000), default = TRUE)
  })
}

# CALL

shinyApp(ui, server)

