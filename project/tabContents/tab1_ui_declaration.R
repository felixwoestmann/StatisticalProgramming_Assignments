tab1 <- function() {
  return(
    tabPanel('1. Popular Stations',
             tabsetPanel(
               popularStationsPanel(),
               overhangPanel(),
               ridgePlotPanel(),
             )
    )
  )
}

popularStationsPanel <- function() {
  return(
    tabPanel('Popular Stations',
             wellPanel(
               h2('Instructions'),
               p("This tab allows the user to interactively analyze data about the most popular stations."),
               p("To analyze the data we counted occurences of each station in the dataset as a start and end station
                and combined those number"),
               p("The user can select the number of stations to be displayed in the barplot and the map. The slider
               has an animation button. The map
                shows the stations as markers and the number of occurences the area of the circle."),
               fluidRow(
                 column(8,
                        sliderInput('popularStattionsNumberOfStations',
                                    label = "Number of stations",
                                    min = 1, max = 84, value = 10,
                                    animate = TRUE),
                 ),
                 column(4,
                        checkboxGroupButtons('popularStationsWeekdayWeekend',
                                             label = "Weekday / Weekend",
                                             choices = list('Show weekend' = 'weekend',
                                                            'Show weekday' = 'weekday'),
                                             selected = c('weekend', 'weekday'),
                                             direction = 'vertical',
                                             individual = TRUE),
                 ),
               ),
             ),
             fluidRow(
               column(6,
                      h3("Overview of most popular stations"),
                      plotOutput("popularStattionsOverviewPlot")),
               column(6,
                      h3("Map of most popular stations"),
                      plotlyOutput("popularStationsMapOverview",
                                   height = "100%",
                                   width = "100%"))
             )
    )
  )
}

overhangPanel <- function() {
  return(tabPanel('Overhang',
                  wellPanel(
                    h2('Instructions'),
                    p("This tab shows the overhang of a station. The overhang is the difference between journeys
                    started at a station and jpurneys ended at a station. If the overhang is positive, more journeys
                    are started then ended at this station."),
                    p("To be easier comparable we report the overhang as a percentage of the total number of journeys
                     started at this station."),
                    p("Only stations are shown where the absolute overhang is larger than 2%."),
                    p("Displayed in the bars is the number of journeys at the station."),
                    br(),
                    p("The map shows the same data as the barplot, the size of the overhang is shown in the area of
                    the circles, the color indicates wether it is positive or negative."),
                    p("Red indicates a positive overhang, green indicates a negative overhang."),
                  ),
                  fluidRow(
                    column(6,
                           h3("Overview of overhang of station usage"),
                           plotOutput("popularStationsOverhangPlot")),
                    column(6,
                           h3("Map of stations with the biggest overhang"),
                           plotlyOutput("popularStationsOverhangMap",
                                        height = "100%",
                                        width = "100%"))
                  ),
  )
  )
}

ridgePlotPanel <- function() {
  return(
    tabPanel("Distribution of station usage",
             sliderInput('ridgeNumberOfStations',
                         label = "Number of stations",
                         min = 1, max = 84, value = 5),
             plotOutput("ridgePlot",
                        height = "650px",
                        width = "100%")
    )
  )
}