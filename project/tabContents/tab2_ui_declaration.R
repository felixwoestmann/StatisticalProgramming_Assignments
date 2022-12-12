tab2 <- function() {
  return(tabPanel('2. Weather and Journeys',
                  wellPanel(
                    h2('Instructions'),
                    p('This tab allows the user to interactively analyze data about weather and the amount of
               journeys conducted.'),
                    shiny::tags$li('- In the first step the user can filter the data based on some paramters. '),
                    shiny::tags$li('- In the second step the user can see intersection of the sets below.')
                  ),
                  fluidRow(
                    column(4,
                           h3("Rain or No Rain"),
                           plotOutput('weatherRain'),
                           wellPanel(
                             h4("Select rain level"),
                             checkboxInput('showRain', label = 'Show Rain', value = TRUE),
                             checkboxInput('showNoRain', label = 'Show No Rain', value = TRUE),
                             width = 12)),
                    column(4,
                           h3("Weekday or Weekend"),
                           plotOutput('weatherWeekdayWeekend'),
                           wellPanel(
                             h4("Select type of day"),
                             checkboxInput('showWeekday', label = 'Show Weekday', value = TRUE,),
                             checkboxInput('showWeekend', label = 'Show Weekend', value = TRUE,),
                             width = 12)),
                    column(4,
                           h3("Time of day"),
                           plotOutput('weatherTimeOfDay'),
                           wellPanel(
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
                  h1("▼▼▼▼", style = "text-align:center;"),
                  plotOutput('weatherCombined'),
  ))
}

