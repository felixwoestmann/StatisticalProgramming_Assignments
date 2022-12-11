# Render functions for tab 2 -------------------------------------------------
output$weatherRain <- renderPlot({
  showRain <- TRUE
  showNoRain <- TRUE
  showRain <- input$showRain
  showNoRain <- input$showNoRain

  journeys_grouped <- journeysGroupedByTime('20 min')

  journeys_grouped$rainLevel <- ifelse(journeys_grouped$mean_precipitation > 0, "Rain", "No Rain")
  journeys_grouped$rainLevel <- factor(journeys_grouped$rainLevel, levels = c("Rain", "No Rain"))
  # Filter based on Checkbox
  journeys_grouped <- journeys_grouped %>%
    filter(rainLevel == "Rain" & showRain == TRUE |
             rainLevel == "No Rain" & showNoRain == TRUE)
  ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = rainLevel)) +
    geom_point(size = 2, shape = 23) +
    theme(legend.position = "bottom")
})

output$weatherWeekdayWeekend <- renderPlot({
  showWeekday <- TRUE
  showWeekend <- TRUE
  showWeekday <- input$showWeekday
  showWeekend <- input$showWeekend

  journeys_grouped <- journeysGroupedByTime('20 min')

  # Calc limits before filtering so its not affected by selected vars
  max_temp <- max(journeys_grouped$mean_temperature)
  min_temp <- min(journeys_grouped$mean_temperature)
  max_n <- max(journeys_grouped$n)
  min_n <- min(journeys_grouped$n)

  journeys_grouped$daytype <- ifelse(wday(journeys_grouped$chunks, label = TRUE) %in% c("Sat", "Sun"), "Weekend",
                                     "Weekday")

  journeys_grouped$daytype <- factor(journeys_grouped$daytype, levels = c("Weekday", "Weekend"))
  # Filter based on Checkbox
  journeys_grouped <- journeys_grouped %>%
    filter(daytype == "Weekday" & showWeekday == TRUE |
             daytype == "Weekend" & showWeekend == TRUE)


  ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = daytype)) +
    geom_point(size = 2, shape = 23) +
    lims(x = c(min_temp, max_temp), y = c(min_n, max_n)) +
    theme(legend.position = "bottom")
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

  journeys_grouped <- journeysGroupedByTime('20 min')

  # Calc limits before filtering so its not affected by selected vars
  max_temp <- max(journeys_grouped$mean_temperature)
  min_temp <- min(journeys_grouped$mean_temperature)
  max_n <- max(journeys_grouped$n)
  min_n <- min(journeys_grouped$n)

  journeys_grouped$daytime <- ifelse(hour(journeys_grouped$chunks) %in% 6:11, "Morning",
                                     ifelse(hour(journeys_grouped$chunks) %in% 12:17, "Afternoon",
                                            ifelse(hour(journeys_grouped$chunks) %in% 18:23, "Evening",
                                                   ifelse(hour(journeys_grouped$chunks) %in% 0:5, "Night", NA))))

  journeys_grouped$daytime <- factor(journeys_grouped$daytime,
                                     levels = c("Morning", "Afternoon", "Evening", "Night"))

  # Filter based on Checkbox
  journeys_grouped <- journeys_grouped %>%
    filter(daytime == "Morning" & showMorning == TRUE |
             daytime == "Afternoon" & showAfternoon == TRUE |
             daytime == "Evening" & showEvening == TRUE |
             daytime == "Night" & showNight == TRUE)


  ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = daytime)) +
    geom_point(size = 2, shape = 23) +
    lims(x = c(min_temp, max_temp), y = c(min_n, max_n)) +
    theme(legend.position = "bottom")
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

  journeys_grouped <- journeysGroupedByTime('20 min')
  # Calc limits before filtering so its not affected by selected vars
  max_temp <- max(journeys_grouped$mean_temperature)
  min_temp <- min(journeys_grouped$mean_temperature)
  max_n <- max(journeys_grouped$n)
  min_n <- min(journeys_grouped$n)

  journeys_grouped$daytime <- ifelse(hour(journeys_grouped$chunks) %in% 6:11, "Morning",
                                     ifelse(hour(journeys_grouped$chunks) %in% 12:17, "Afternoon",
                                            ifelse(hour(journeys_grouped$chunks) %in% 18:23, "Evening",
                                                   ifelse(hour(journeys_grouped$chunks) %in% 0:5, "Night", NA))))

  journeys_grouped$daytime <- factor(journeys_grouped$daytime, levels = c("Morning", "Afternoon", "Evening", "Night"))

  journeys_grouped$daytype <- ifelse(wday(journeys_grouped$chunks, label = TRUE) %in% c("Sat", "Sun"), "Weekend",
                                     "Weekday")

  journeys_grouped$daytype <- factor(journeys_grouped$daytype, levels = c("Weekday", "Weekend"))

  journeys_grouped$rainLevel <- ifelse(journeys_grouped$mean_precipitation > 0, "Rain", "No Rain")
  journeys_grouped$rainLevel <- factor(journeys_grouped$rainLevel, levels = c("Rain", "No Rain"))

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

  ggplot(journeys_grouped, aes(x = mean_temperature, y = n, fill = daytimeAndDaytypeAndRain)) +
    geom_point(size = 2, shape = 23) +
    lims(x = c(min_temp, max_temp), y = c(min_n, max_n))

})