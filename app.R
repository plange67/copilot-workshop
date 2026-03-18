library(shiny)

ui <- fluidPage(
  titlePanel("Clustering with PCA"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("columns", "Select Columns for Clustering:", choices = NULL),
      actionButton("run", "Run Clustering"),
      textOutput("validation")
    ),
    mainPanel(
      plotOutput("pcaPlot")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    # Load your dataset here
    # For example, use mtcars
    data <- mtcars
    return(data)
  })

  # Update column choices based on the dataset
  observe({
    data <- dataset()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    updateCheckboxGroupInput(session, "columns", choices = numeric_cols)
  })

  output$validation <- renderText({
    if (length(input$columns) < 2) {
      return("Please select at least 2 numeric columns.")
    } else {
      return("")
    }
  })

  output$pcaPlot <- renderPlot({
    input$run
    isolate({
      validate(need(length(input$columns) >= 2, "Please select at least 2 numeric columns."))
      data <- dataset()
      data <- na.omit(data[input$columns])  # Drop rows with missing values

      if (nrow(data) == 0) {
        return(NULL)
      }

      pca <- prcomp(data, center = TRUE, scale. = TRUE)
      pca_data <- as.data.frame(pca$x)
      plot(pca_data[,1:2], col = 'blue', pch = 19,
           xlab = 'Principal Component 1',
           ylab = 'Principal Component 2',
           main = 'PCA Plot')
    })
  })
}

shinyApp(ui, server)