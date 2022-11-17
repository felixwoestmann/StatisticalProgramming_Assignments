movies <-
  read.csv(file = "Top70MoviesonBoxOfficein2022.csv") # soruce: https://www.kaggle.com/datasets/aneesayoub/top-70-movies-on-box-office-in-2022
colnames(movies) <-
  c(
    "title",
    "directors",
    "country",
    "languages.",
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



optionsScatterplotAxis <- c("Earnings" = "earningsInDollar",
                            "IMDB Rating" = "imdb_rating",
                            "# Reviews" = "total_reviews")

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
      selectInput(
        "scatterPlotXAxis",
        "X Axis:",
        selected = optionsScatterplotAxis[1],
        optionsScatterplotAxis
      ),
      actionButton('switchVariables', "Switch", icon = icon("arrows-rotate")),
      selectInput(
        "scatterPlotYAxis",
        "Y Axis:",
        selected = optionsScatterplotAxis[2],
        optionsScatterplotAxis
      ),
      checkboxInput("enableLinearModel", "Show regression line", value = FALSE)
    )
  )
}

ui <- fluidPage(titlePanel("Top 70 grossing movies 2022"),
                mainPanel(tabsetPanel(
                  tabPanel(
                    "Plot",
                    selectInput(
                      "histVariable",
                      "Variable:",
                      c("Earnings" = "earningsInDollar",
                        "Ratings" = "imdb_rating")
                    ),
                    sliderInput(
                      "plotBins",
                      label = h3("Select number of bins"),
                      min = 1,
                      max = 35,
                      value = 10,
                      step = 1
                    ),
                    plotOutput("plot")
                  ),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel(
                    "Scatterplot",
                    sidebarLayout(scatterPlotSideBarPanel(),
                                  mainPanel(plotOutput("scatterPlot")))
                  )
                )))

server <- function(input, output, session) {
  output$plot <- renderPlot({
    hist(movies[, input$histVariable],
         breaks = input$plotBins,
         xlab = input$histVariable)
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
      movies[order(movies[, input$scatterPlotXAxis]),]
    movies_range <-
      movies_sorted[input$movieRange[1]:input$movieRange[2],]
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
      abline(lm(movies[, input$scatterPlotYAxis] ~ movies[, input$scatterPlotXAxis]), col = "red")
    }
  })
}

shinyApp(ui, server)
