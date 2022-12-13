tab3 <- function() {
  return(
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
  )
}