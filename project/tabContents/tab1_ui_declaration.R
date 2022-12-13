tab1 <- function() {
  return(
    tabPanel('1. Popular Stations',
             tabsetPanel(
               popularStationsPanel(),
               overhangPanel(),
               tabPanel('Ridgeardson')
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
               h3('Select number of stations to display'),
               sliderInput('popularStattionsNumberOfStations',
                           label = "Number of stations",
                           min = 1, max = 84, value = 10,
                           animate = TRUE),),
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
                    br(),
                    p("Displayed in the bars is the number of journeys at the station."),
                  ),
                  fluidRow(
                    column(6,
                           h3("Overview of overhang of station usage"),
                           plotOutput("popularStationsOverhangPlot")),

                  ),
  )
  )
}