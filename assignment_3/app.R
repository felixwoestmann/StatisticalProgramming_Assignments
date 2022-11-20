movies <-
  read.csv(file = "Top70MoviesonBoxOfficein2022.csv") # soruce: https://www.kaggle.com/datasets/aneesayoub/top-70-movies-on-box-office-in-2022
colnames(movies) <-
  c(
    "title",
    "directors",
    "country",
    "languages",
    "earnings",
    "imdb_rating",
    "description",
    "total_reviews"
  )

# First remove the $ sign by replcing it with '' // gsub("\\$", "", movies$earnings)
# Then remove all commas from number // gsub(",", "", input, fixed = TRUE)
# Then convert it to a numeric column // as.numeric
movies$earningsInDollar <-
  as.numeric(gsub(",", "", gsub("\\$", "", movies$earnings), fixed = TRUE))

numericVariables <- c("Earnings" = "earningsInDollar",
                      "IMDB Rating" = "imdb_rating",
                      "# Reviews" = "total_reviews")

# Make nominal to factor
movies$country <- as.factor(movies$country)
movies$languages <- as.factor(movies$languages)

factorVariables <- c("Country" = "country","Languages"="languages")

readableNamesColumns <-
  c(
    "Title",
    "Directors",
    "Countries",
    "Languages",
    "Earnings",
    "IMDB Rating",
    "Description",
    "Total number of reviews",
    "Earnings in $"
  )

readableNameForColumnName <- function(columnName) {
  indexInColNames <- which(colnames(movies) == columnName)
  return(readableNamesColumns[indexInColNames])
}

showAlertForSelectingSameVariableTwice <- function() {
  return (
    shinyalert(
      title = "X Axis and Y Axis have to be different!",
      text = "",
      size = "m",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "I understand, Maam",
      confirmButtonCol = "#4CAF4F",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  )
}

library(shiny)
library(shinyalert)
library(glue)



histogramSideBarPanel <- function() {
  return (sidebarPanel(
    selectInput("histVariable",
                "Variable:",
                numericVariables),
    sliderInput(
      "plotBins",
      label = h3("Select number of bins"),
      min = 1,
      max = 35,
      value = 10,
      step = 1
    )
  ))
}

frequenciesSideBarPanel <- function() {
  return (sidebarPanel(
    selectInput("frequencyVariable",
                "Variable:",
                factorVariables),
    sliderInput(
      "minimumFrequency",
      label = h3("Minimum Frequency"),
      min = 1,
      max = 5,
      value = 1,
      step = 1
    ),
    checkboxInput("sortFrequencies", "Sort frequencies", value = FALSE)
  ))
}

scatterPlotSideBarPanel <- function() {
  return (
    sidebarPanel(
      sliderInput(
        "movieRange",
        label = h3("Select range of top movies by X Axis"),
        min = 0,
        max = nrow(movies),
        value = c(0, nrow(movies)),
        step = 1
      ),
      selectInput("scatterPlotXAxis",
                  "X Axis:",
                  selected = numericVariables[1],
                  numericVariables),
      actionButton('switchVariables', "Switch", icon = icon("arrows-rotate")),
      selectInput("scatterPlotYAxis",
                  "Y Axis:",
                  selected = numericVariables[2],
                  numericVariables),
      checkboxInput("enableLinearModel", "Show regression line", value = FALSE)
    )
  )
}

ui <- fluidPage(titlePanel("Top 70 grossing movies 2022"),
                mainPanel(tabsetPanel(
                  tabPanel(
                    "Histograms",
                    sidebarLayout(histogramSideBarPanel(), mainPanel(plotOutput("plotHist")))
                  ),
                  tabPanel(
                    "Frequencies",
                    sidebarLayout(frequenciesSideBarPanel(), mainPanel(plotOutput("frequencyPlot")))
                  ),
                  tabPanel(
                    "Scatterplot",
                    sidebarLayout(scatterPlotSideBarPanel(),
                                  mainPanel(plotOutput("scatterPlot")))
                  )
                )))

server <- function(input, output, session) {
  output$plotHist <- renderPlot({
    hist(movies[, input$histVariable],
         breaks = input$plotBins,
         main =glue('Histogram of {readableNameForColumnName(input$histVariable)}'),
         xlab = readableNameForColumnName(input$histVariable))
  })
  # Observe Switch Vars Button event
  observeEvent(input$switchVariables, {
    yAxis <- input$scatterPlotYAxis
    xAxis <- input$scatterPlotXAxis
    updateSelectInput(session,
                      'scatterPlotYAxis',
                      selected = xAxis)
    updateSelectInput(session,
                      'scatterPlotXAxis',
                      selected = yAxis)
  })
  output$frequencyPlot <- renderPlot({
    freq_tab = table(movies[, input$frequencyVariable])
    if (input$sortFrequencies) {
      freq_tab <- sort(freq_tab, decreasing = T)
    }
    barplot(freq_tab[freq_tab>=input$minimumFrequency])
  })
  output$scatterPlot <- renderPlot({
    # Check that you can't choose X axis = Y axis
    if (input$scatterPlotXAxis == input$scatterPlotYAxis) {
      showAlertForSelectingSameVariableTwice()
      # Code below empties the selectVariable Input for YAxis of Scatterplot
      observe({
        updateSelectInput(session,
                          'scatterPlotYAxis',
                          selected = '')
      })
      
    }
    movies_sorted <-
      movies[order(movies[, input$scatterPlotXAxis]), ]
    movies_range <-
      movies_sorted[input$movieRange[1]:input$movieRange[2], ]
    plot(
      movies_range[, input$scatterPlotXAxis],
      movies_range[, input$scatterPlotYAxis],
      main = glue(
        'Scatterplot of {readableNameForColumnName(input$scatterPlotYAxis)} and {readableNameForColumnName(input$scatterPlotXAxis)}'
      ),
      ylab = readableNameForColumnName(input$scatterPlotYAxis),
      xlab = readableNameForColumnName(input$scatterPlotXAxis),
      col = "blue"
    )
    if (input$enableLinearModel) {
      # draw regression line
      linearModel<-lm(movies_range[, input$scatterPlotYAxis] ~ movies_range[, input$scatterPlotXAxis])
      abline(linearModel, col = "red")
      intercept<-coef(linearModel)[1]
      intercept<-round(intercept,2)
      slope<-coef(linearModel)[2]
      slope<-signif(slope,2)
      eq <- paste0(glue('{readableNameForColumnName(input$scatterPlotYAxis)} = '), intercept,
                   ifelse(sign(slope)==1, " + ", " - "), abs(slope), glue(' × {readableNameForColumnName(input$scatterPlotXAxis)} '))
      
      mtext(eq, 3, line=-2)
    }
  })
}

shinyApp(ui, server)
