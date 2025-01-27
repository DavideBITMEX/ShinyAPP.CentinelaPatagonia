#' 2024 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shiny
#' @import ggplot2


library(ggplot2)

library(ggplot2)

mod_2024_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("descriptive_stats")),
    fluidRow(
      column(
        width = 6,
        plotOutput(ns("plot_1"), height = "300px")
      ),
      column(
        width = 6,
        plotOutput(ns("plot_2"), height = "300px")
      )
    )
  )
}

mod_2024_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Descriptive statistics
    output$descriptive_stats <- renderText({
      "Descriptive statistics for 2024"
    })

    # Plot 1
    output$plot_1 <- renderPlot({
      ggplot(mtcars, aes(x = mpg, y = disp)) +
        geom_point() +
        ggtitle("2024: MPG vs Disp")
    })

    # Plot 2
    output$plot_2 <- renderPlot({
      ggplot(mtcars, aes(x = hp, y = wt)) +
        geom_point() +
        ggtitle("2024: HP vs WT")
    })
  })
}





## To be copied in the UI
# mod_2024_ui("2024_1")

## To be copied in the server
# mod_2024_server("2024_1")
