tab1 <- function() {
  return(
    tabPanel('1. Popular Stations',
             sliderInput('numberOfStations',
                         label = "",
                         min = 1,
                         max = 10,
                         value = 5),
             plotOutput('popStationsHistogram'),
    )
  )
}