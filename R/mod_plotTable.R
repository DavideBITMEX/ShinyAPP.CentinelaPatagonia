# Define the UI module
plotTableUI <- function(id) {
  ns <- NS(id) # Namespace for the module
  tagList(
    plotOutput(ns("plot"), brush = ns("plot_brush")),
    DTOutput(ns("table"))
  )
}

# Server logic for the module
plotTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot(
      ggplot(mtcars) +
        geom_point(aes(x = mpg, y = disp))
    )

    output$table <- renderDT({
      brushedPoints(mtcars, input$plot_brush)
    })
  })
}

# plotTableUI("plotTable") # Call the UI module
# plotTableServer("plotTable") # Call the server module
