getPopularStationData <- function(x) {
  start_station_counts <- x %>%
    group_by(station_start) %>%
    summarise(n = n())

  end_station_counts <- x %>%
    group_by(station_end) %>%
    summarise(n = n())

  colnames(start_station_counts) <- c("station", "n")
  colnames(end_station_counts) <- c("station", "n")

  # Combine counts of start and end stations
  popular_stations <- start_station_counts %>%
    full_join(end_station_counts, by = "station") %>%
    mutate(n = n.x + n.y) %>%
    select(station, n)

  # Add station names and coordinates
  popular_stations <- merge(popular_stations,
                            stations[, c('number', 'name', 'lat', 'lon')],
                            by.x = "station",
                            by.y = "number")

  return(popular_stations)
}

mode <- function(codes) { which.max(tabulate(codes)) }

getVectorOfColorsForBarplot <- function(x) {

  if (length(x) %% 2 == 0) {
    return(rep(wes_palette("Darjeeling1")[1:2], ceiling(length(x) / 2)))
  } else {
    length_x <- (length(x) - 1) / 2
    return(c(rep(wes_palette("Darjeeling1")[1:2], ceiling(length_x)), wes_palette("Darjeeling1")[1]))
  }
}

isShowWeekend <- function(vector) {
  showWeekend <- FALSE
  if ("weekend" %in% vector) { showWeekend <- TRUE }
  return(showWeekend)
}

isShowWeekday <- function(vector) {
  showWeekday <- FALSE
  if ("weekday" %in% vector) { showWeekday <- TRUE }
  return(showWeekday)
}

toPercent <- function(x) { format(paste0(x, "%")) }

getOverhangStationData <- function(x) {
  start_station_counts <- x %>%
    group_by(station_start) %>%
    summarise(n = n())

  end_station_counts <- x %>%
    group_by(station_end) %>%
    summarise(n = n())

  colnames(start_station_counts) <- c("station", "n")
  colnames(end_station_counts) <- c("station", "n")

  station_overhang <- start_station_counts %>%
    full_join(end_station_counts, by = "station") %>%
    mutate(n = n.x - n.y) %>%
    select(station, n)

  colnames(station_overhang) <- c("station", "start_overhang")

  # Add station names and coordinates
  station_overhang <- merge(station_overhang,
                            stations[, c('number', 'name', 'lat', 'lon')],
                            by.x = "station",
                            by.y = "number")


  # Calculate overhang as percentage of start_station_counts and filter out stations with absoulute overhang smaller
  # then 10
  station_overhang <- station_overhang %>%
    left_join(start_station_counts, by = "station") %>%
    mutate(overhang = start_overhang / n * 100) %>%
    select(station, name, overhang, n) %>%
    filter(abs(overhang) > 2) %>%
    arrange(desc(overhang))

  return(station_overhang)
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
  journeys_filtered <- journeys %>%
    filter((is_weekend == TRUE & isShowWeekend(input$popularStationsWeekdayWeekend) == TRUE)
             | (is_weekend == FALSE & isShowWeekday(input$popularStationsWeekdayWeekend) == TRUE))

  popular_stations <- getPopularStationData(journeys_filtered) %>%
    arrange(desc(n)) %>%
    head(numberOfStations)

  ### Prepare Plot for Ljubljana
  plot_mapbox(popular_stations) %>%
    add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
    layout(mapbox = list(style = "basic", zoom = 12, center = list(lon = 14.5, lat = 46.05))) %>%
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
    config(displayModeBar = FALSE,
           mapboxAccessToken = mapbox_token)
})

output$popularStationsOverhangPlot <- renderPlot({
  station_overhang <- getOverhangStationData(journeys)


  ggplot(station_overhang, aes(x = overhang, y = reorder(name, overhang))) +
    geom_bar(stat = "identity",
             fill = getVectorOfColorsForBarplot(station_overhang$overhang)) +
    geom_text(aes(label = n),
              nudge_x = ifelse(station_overhang$overhang < 0, 0.9, -0.9),
              fontface = "bold") +
    theme(axis.text = element_text(size = 10)) +
    scale_x_continuous(breaks = seq(-15, 15, 2),
                       labels = toPercent(seq(-15, 15, 2))) +
    labs(x = "Overhang", y = "Station name")
})

output$popularStationsOverhangMap <- renderPlotly({
  station_overhang <- getOverhangStationData(journeys)
  # add lat and lon from station data
  station_overhang <- merge(station_overhang,
                            stations[, c('number', 'lat', 'lon')],
                            by.x = "station",
                            by.y = "number")

  # split overhang info into two columns to more easily disply on map
  station_overhang <- station_overhang %>%
    mutate(overhang_abs = abs(overhang),
           overhang_neg = overhang < 0)

  # split station overhang into two data frames by overhang_neg
  station_overhang_pos <- station_overhang %>%
    filter(overhang_neg == FALSE) %>%
    select(station, name, overhang_abs, lat, lon)

  station_overhang_neg <- station_overhang %>%
    filter(overhang_neg == TRUE) %>%
    select(station, name, overhang_abs, lat, lon)

  ### Prepare Plot for Ljubljana
  plot_mapbox(station_overhang) %>%
    add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
    layout(mapbox = list(style = "basic", zoom = 10, center = list(lon = 14.51, lat = 46.05))) %>%
    add_markers(
      data = station_overhang_pos,
      x = ~lon,
      y = ~lat,
      color = I(wes_palette("Darjeeling1")[1]),
      size = ~overhang_abs,
      marker = list(sizemode = 'area', sizeref = 0.3, sizemin = 2),
      text = ~name,
      hoverinfo = "text",
      showlegend = FALSE,
    ) %>%
    add_markers(
      data = station_overhang_neg,
      x = ~lon,
      y = ~lat,
      color = I(wes_palette("Darjeeling1")[2]),
      size = ~overhang_abs,
      marker = list(sizemode = 'area', sizeref = 0.3, sizemin = 2),
      text = ~name,
      hoverinfo = "text",
      showlegend = FALSE,
    ) %>%
    config(displayModeBar = FALSE,
           mapboxAccessToken = mapbox_token)
})

output$ridgePlot <- renderPlot({
  # only get station_start, hour and timestamp_start from journeys
  journeys_ridge <- journeys %>%
    select(station_start, hour)

  # get median of popular stations by hour
  distribution <- journeys_ridge %>%
    group_by(hour, station_start) %>%
    summarise(n = n())


  if (input$ridgeSortingStat == 1) {
    #median
  } else if (input$ridgeSortingStat == 2) {
    #mean
  } else if (input$ridgeSortingStat == 3) {
    # mode
  }

  distribution <- distribution %>%
    arrange(desc(sort_stat)) %>%
    head(input$ridgeNumberOfStations)

  # add names of stations
  journeys_ridge <- merge(journeys_ridge,
                          stations[, c('number', 'name')],
                          by.x = "station_start",
                          by.y = "number")

  # add mean hour of station
  journeys_ridge <- merge(journeys_ridge,
                          distribution,
                          by.x = "station_start",
                          by.y = "station_start")

  # Convert journeys_ridge name to factor
  journeys_ridge$name <- as.factor(journeys_ridge$name)


  ggplot(journeys_ridge, aes(x = hour, y = name, height = ..density..)) +
    geom_density_ridges(stat = "density", trim = TRUE) +
    scale_x_continuous(breaks = seq(0, 24, 2)) +
    theme(legend.position = "none")
})
