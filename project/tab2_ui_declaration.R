tab2UITop <- function() {
  topUi <- fluidRow(
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
  )
  return(topUi)
}

tab2UIBottom <- function() {
  bottomUi <- plotOutput('weatherCombined')
  return(bottomUi)
}