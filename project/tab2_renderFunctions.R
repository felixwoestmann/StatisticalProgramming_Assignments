# Helper functions for tab 2 ---------------------------------------------------

# Calc limits before filtering so its not affected by selected vars
minMaxTemp <- function(groupedJourneys) {
  max_temp <- max(groupedJourneys$mean_temperature)
  min_temp <- min(groupedJourneys$mean_temperature)
  return(c(min_temp, max_temp))
}

# Calc limits before filtering so its not affected by selected vars
minMaxN <- function(groupedJourneys) {
  max_n <- max(groupedJourneys$n)
  min_n <- min(groupedJourneys$n)
  return(c(min_n, max_n))
}

# Add Celsius unit to number
toCelsius <- function(x) { format(paste0(x, " °C")) }

addRainLevel <- function(x) {
  x$rainLevel <- ifelse(x$mean_precipitation > 0, "Rain", "No Rain")
  x$rainLevel <- factor(x$rainLevel, levels = c("Rain", "No Rain"))
  return(x)
}

addDayType <- function(x) {
  x$daytype <- ifelse(wday(x$chunks, label = TRUE) %in% c("Sat", "Sun"),
                      "Weekend",
                      "Weekday")
  x$daytype <- factor(x$daytype, levels = c("Weekday", "Weekend"))
  return(x)
}

addDayTime <- function(x) {
  x$daytime <- ifelse(hour(x$chunks) %in% 6:11, "Morning",
                      ifelse(hour(x$chunks) %in% 12:17, "Afternoon",
                             ifelse(hour(x$chunks) %in% 18:23, "Evening",
                                    ifelse(hour(x$chunks) %in% 0:5, "Night", NA))))

  x$daytime <- factor(x$daytime, levels = c("Morning", "Afternoon", "Evening", "Night"))
  return(x)
}

# Render functions for tab 2 -------------------------------------------------
output$weatherRain <- renderPlot({
  showRain <- TRUE
  showNoRain <- TRUE
  showRain <- input$showRain
  showNoRain <- input$showNoRain

  journeys_grouped <- journeysGroupedByTime(journeys, '20 min')

  xLims <- minMaxTemp(journeys_grouped)
  yLims <- minMaxN(journeys_grouped)

  journeys_grouped <- addRainLevel(journeys_grouped)
  # Filter based on Checkbox
  journeys_grouped <- journeys_grouped %>%
    filter(rainLevel == "Rain" & showRain == TRUE |
             rainLevel == "No Rain" & showNoRain == TRUE)

  ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = rainLevel)) +
    geom_point(size = 2, shape = 21) +
    lims(x = xLims, y = yLims) +
    scale_color_manual(aesthetics = "fill",
                       values = c("Rain" = wes_palette("Darjeeling1")[1],
                                  "No Rain" = wes_palette("Darjeeling1")[2])) +
    scale_x_continuous(breaks = seq(0, 14, 2),
                       labels = toCelsius(seq(0, 14, 2)),
                       limits = xLims) +
    labs(x = "Mean temperature", y = "Number of Journeys") +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = "Rain level"))
})

output$weatherWeekdayWeekend <- renderPlot({
  showWeekday <- TRUE
  showWeekend <- TRUE
  showWeekday <- input$showWeekday
  showWeekend <- input$showWeekend

  journeys_grouped <- journeysGroupedByTime(journeys, '20 min')

  # Calc limits before filtering so its not affected by selected vars
  xLims <- minMaxTemp(journeys_grouped)
  yLims <- minMaxN(journeys_grouped)

  journeys_grouped <- addDayType(journeys_grouped)
  # Filter based on Checkbox
  journeys_grouped <- journeys_grouped %>%
    filter(daytype == "Weekday" & showWeekday == TRUE |
             daytype == "Weekend" & showWeekend == TRUE)


  ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = daytype)) +
    geom_point(size = 2, shape = 21) +
    lims(x = xLims, y = yLims) +
    scale_color_manual(aesthetics = "fill",
                       values = c("Weekday" = wes_palette("Darjeeling1")[1],
                                  "Weekend" = wes_palette("Darjeeling1")[2])) +
    scale_x_continuous(breaks = seq(0, 14, 2),
                       labels = toCelsius(seq(0, 14, 2)),
                       limits = xLims) +
    labs(x = "Mean temperature", y = "Number of Journeys") +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = "Type of day"))
})

output$weatherTimeOfDay <- renderPlot({
  showMorning <- TRUE
  showAfternoon <- TRUE
  showEvening <- TRUE
  showNight <- TRUE
  showMorning <- input$showMorning
  showAfternoon <- input$showAfternoon
  showEvening <- input$showEvening
  showNight <- input$showNight

  journeys_grouped <- journeysGroupedByTime(journeys, '20 min')

  # Calc limits before filtering so its not affected by selected vars
  xLims <- minMaxTemp(journeys_grouped)
  yLims <- minMaxN(journeys_grouped)

  journeys_grouped <- addDayTime(journeys_grouped)

  # Filter based on Checkbox
  journeys_grouped <- journeys_grouped %>%
    filter(daytime == "Morning" & showMorning == TRUE |
             daytime == "Afternoon" & showAfternoon == TRUE |
             daytime == "Evening" & showEvening == TRUE |
             daytime == "Night" & showNight == TRUE)


  ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = daytime)) +
    geom_point(size = 2, shape = 21) +
    lims(x = xLims, y = yLims) +
    scale_color_manual(aesthetics = "fill",
                       values = c("Morning" = wes_palette("Darjeeling1")[1],
                                  "Afternoon" = wes_palette("Darjeeling1")[2],
                                  "Evening" = wes_palette("Darjeeling1")[3],
                                  "Night" = wes_palette("Darjeeling1")[4])) +
    scale_x_continuous(breaks = seq(0, 14, 2),
                       labels = toCelsius(seq(0, 14, 2)),
                       limits = xLims) +
    labs(x = "Mean temperature", y = "Number of Journeys") +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = "Time of day"))
})

output$weatherCombined <- renderPlot({
  showWeekday <- TRUE
  showWeekend <- TRUE
  showWeekday <- input$showWeekday
  showWeekend <- input$showWeekend

  showMorning <- TRUE
  showAfternoon <- TRUE
  showEvening <- TRUE
  showNight <- TRUE
  showMorning <- input$showMorning
  showAfternoon <- input$showAfternoon
  showEvening <- input$showEvening
  showNight <- input$showNight

  showRain <- TRUE
  showNoRain <- TRUE
  showRain <- input$showRain
  showNoRain <- input$showNoRain

  journeys_grouped <- journeysGroupedByTime(journeys, '20 min')
  # Calc limits before filtering so its not affected by selected vars
  xLims <- minMaxTemp(journeys_grouped)
  yLims <- minMaxN(journeys_grouped)

  journeys_grouped <- addDayTime(journeys_grouped)
  journeys_grouped <- addDayType(journeys_grouped)
  journeys_grouped <- addRainLevel(journeys_grouped)

  # Combine daytime and daytype
  journeys_grouped$daytimeAndDaytypeAndRain <- paste(journeys_grouped$daytime, journeys_grouped$daytype,
                                                     journeys_grouped$rainLevel, sep = "-")

  journeys_grouped <- journeys_grouped %>%
    filter(daytype == "Weekday" & showWeekday == TRUE |
             daytype == "Weekend" & showWeekend == TRUE) %>%
    filter(daytime == "Morning" & showMorning == TRUE |
             daytime == "Afternoon" & showAfternoon == TRUE |
             daytime == "Evening" & showEvening == TRUE |
             daytime == "Night" & showNight == TRUE) %>%
    filter(rainLevel == "Rain" & showRain == TRUE |
             rainLevel == "No Rain" & showNoRain == TRUE)

  ggplot(journeys_grouped, aes(x = mean_temperature, y = n)) +
    geom_point(size = 2, shape = 21, fill = wes_palette('Darjeeling1')[4]) +
    lims(x = xLims, y = yLims) +
    scale_x_continuous(breaks = seq(0, 14, 2),
                       labels = toCelsius(seq(0, 14, 2)),
                       limits = xLims) +
    labs(x = "Mean temperature", y = "Number of Journeys")
})