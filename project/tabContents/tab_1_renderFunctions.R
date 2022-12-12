getPopularStationData <- function(x) {
  popular_start_stations <- x %>%
    group_by(station_start) %>%
    summarise(n = n())

  popular_end_stations <- x %>%
    group_by(station_end) %>%
    summarise(n = n())

  colnames(popular_start_stations) <- c("station", "n")
  colnames(popular_end_stations) <- c("station", "n")

  # Combine counts of start and end stations
  popular_stations <- popular_start_stations %>%
    full_join(popular_end_stations, by = "station") %>%
    mutate(n = n.x + n.y) %>%
    select(station, n)

  # Add station names and coordinates
   popular_stations <- merge(popular_stations,
                              stations[, c('number', 'name', 'lat', 'lon')],
                              by.x = "station",
                              by.y = "number")

  return(popular_stations)
}

output$popularStattionsOverviewPlot <- renderPlot({
  popular_stations <- getPopularStationData(journeys)

  popular_stations <- popular_stations %>%
    arrange(desc(n)) %>%
    head(10)

  ggplot(popular_stations, aes(x = n, y = name)) +
    geom_bar(stat = "identity", fill = rep(wes_palette("Darjeeling1")[1:2], ceiling(length(popular_stations$n) / 2))) +
    theme(axis.text = element_text(size = 10)) +
    labs(x = "# Station was start or end of Journey", y = "Station name")
})

output$popularStationsMapOverview <- renderPlotly({
  popular_stations <- getPopularStationData(journeys)
  ### Prepare Plot for Ljubljana
  plot_mapbox(popular_stations) %>%
    add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
    layout(
      mapbox = list(style = "basic",
                    zoom = 12,
                    center = list(lon = 14.5, lat = 46.05))) %>%
    # Add our data
    add_markers(
      x = ~lon,
      y = ~lat,
      size = ~n,
      color = "red",
      text = ~name,
      hoverinfo = "text",
      showlegend = FALSE,
    ) %>%
     config(displayModeBar = FALSE)
})