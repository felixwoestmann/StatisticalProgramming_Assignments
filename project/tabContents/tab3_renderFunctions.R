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
             aes(y = meandistance, x = hour, fill = wes_palette("Darjeeling1")[1]),
             stat = "identity") +
    xlab("Hour of the day") +
    ylab("Average distance in meters") +
    coord_cartesian(xlim = NULL, ylim = c(0, 3000), default = TRUE) +
    theme(legend.position = "none")


  # scale_color_manual(aesthetics = "fill",
  #                    values = c("Morning" = wes_palette("Darjeeling1")[1],
  #                               "Afternoon" = wes_palette("Darjeeling1")[2],
  #                               "Evening" = wes_palette("Darjeeling1")[3],
  #                               "Night" = wes_palette("Darjeeling1")[4]))
  # 
  # scale_color_manual(name = "hour",
  #                    values = c("(00,06]" = wes_palette("Darjeeling1")[4],
  #                               "(06,12]" = wes_palette("Darjeeling1")[1],
  #                               "(12,18]" = wes_palette("Darjeeling1")[2],
  #                               "(18,00]" = wes_palette("Darjeeling1")[3]),
  #                    labels = c("Night","Morning", "Afternoon", "Evening"))+

})

output$DistanceBarplot2 <- renderPlot({

  disdata$weekday <- as.factor(disdata$weekday)
  slider_value <- input$dayoftheweek

  disdata <- disdata %>%
    filter(disdata$weekday == "Mon" & slider_value == "Monday" |
             disdata$weekday == "Tue" & slider_value == "Tuesday" |
             disdata$weekday == "Wed" & slider_value == "Wednesday" |
             disdata$weekday == "Thu" & slider_value == "Thursday" |
             disdata$weekday == "Fri" & slider_value == "Friday" |
             disdata$weekday == "Sat" & slider_value == "Saturday" |
             disdata$weekday == "Sun" & slider_value == "Sunday")


  ggplot() +
    geom_bar(data = disdata %>%
      group_by(hour) %>%
      summarise(meandistance = mean(distance_meters)),
             aes(y = meandistance, x = hour, fill = wes_palette("Darjeeling1")[1]),
             stat = "identity") +
    xlab("Hour of the day") +
    ylab("Average distance in meters") +
    coord_cartesian(xlim = NULL, ylim = c(0, 3000), default = TRUE) +
    theme(legend.position = "none")
})