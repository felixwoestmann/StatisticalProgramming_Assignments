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

getVectorOfColorsForBarplot <- function(x) {

  if (length(x) %% 2 == 0) {
    return(rep(wes_palette("Darjeeling1")[1:2], ceiling(length(x) / 2)))
  } else {
    length_x <- (length(x) - 1) / 2
    return(c(rep(wes_palette("Darjeeling1")[1:2], ceiling(length_x)), wes_palette("Darjeeling1")[1]))
  }
}

output$popularStattionsOverviewPlot <- renderPlot({
  numberOfStations <- 10
  numberOfStations <- input$popularStattionsNumberOfStations

  popular_stations <- getPopularStationData(journeys) %>%
    arrange(desc(n)) %>%
    head(numberOfStations)

  ggplot(popular_stations, aes(x = n, y = reorder(name, n))) +
    geom_bar(stat = "identity", fill = getVectorOfColorsForBarplot(popular_stations$n)) +
    theme(axis.text = element_text(size = 10)) +
    labs(x = "# Station was start or end of Journey", y = "Station name")
})

output$popularStationsMapOverview <- renderPlotly({
  numberOfStations <- 10
  numberOfStations <- input$popularStattionsNumberOfStations

  popular_stations <- getPopularStationData(journeys) %>%
    arrange(desc(n)) %>%
    head(numberOfStations)

  ### Prepare Plot for Ljubljana
  plot_mapbox(popular_stations) %>%
    add_segments(x = -100,
                 xend = -50,
                 y = 50,
                 yend = 75) %>%
    layout(
      mapbox = list(style = "basic",
                    zoom = 12,
                    center = list(lon = 14.5, lat = 46.05))) %>%
    add_markers(
      x = ~lon,
      y = ~lat,
      color = I(wes_palette("Darjeeling1")[1]),
      size = ~n,
      marker = list(sizemode = 'area',
                    sizeref = 0.3,
                    sizemin = 2),
      text = ~name,
      hoverinfo = "text",
      showlegend = FALSE,
    ) %>%
    config(displayModeBar = FALSE)
})