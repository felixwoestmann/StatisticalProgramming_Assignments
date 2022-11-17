movies <- read.csv(file = "Top70MoviesonBoxOfficein2022.csv") # soruce: https://www.kaggle.com/datasets/aneesayoub/top-70-movies-on-box-office-in-2022
head(movies)
colnames(movies) <- c("title", "directors", "country", "languages.", "earnings", "imdb_rating", "description", "total_reviews")

# First remove the $ sign by replcing it with '' // gsub("\\$", "", movies$earnings)
# Then remove all commas from number // gsub(",", "", input, fixed = TRUE)
# Then convert it to a numeric column // as.numeric
movies$earningsInDollar <- as.numeric(gsub(",", "", gsub("\\$", "", movies$earnings), fixed = TRUE))


library(shiny)

ui <- fluidPage(
  titlePanel("Top 70 grossing movies 2022"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "variable", "Variable:",
        c(
          "Earnings" = "earningsInDollar",
          "Ratings" = "imdb_rating"
        )
      ),
      sliderInput("movieRange",
        label = h3("Select range of top earing movies"), min = 0,
        max = nrow(movies), value = c(0, nrow(movies)), step = 1
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 selectInput(
          "histVariable", "Variable:",
          c(
            "Earnings" = "earningsInDollar",
            "Ratings" = "imdb_rating"
          )),
          sliderInput("plotBins",
                      label = h3("Select number of bins"), min = 1,
                      max = 35, value = 10, step = 1
          ),
        
        plotOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Scatterplot", plotOutput("scatterPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    hist(movies[,input$histVariable], breaks=input$plotBins,xlab=input$histVariable)
  })
  output$scatterPlot <- renderPlot({
    movies_sorted <- movies[order(movies$earningsInDollar), ]
    movies_range <- movies_sorted[input$movieRange[1]:input$movieRange[2], ]
    plot(movies_range$earningsInDollar, movies_range$imdb_rating,
      main = "Scatterplot Example",
      ylab = "Rating ", xlab = "Earnings "
    )
  })
}

shinyApp(ui, server)

