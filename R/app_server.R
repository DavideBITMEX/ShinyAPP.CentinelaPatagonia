#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny leaflet
#' @noRd

library(leaflet)

app_server <- function(input, output, session) {
  # Reactive to handle the selected year
  observeEvent(input$campaign_selector, {
    if (input$campaign_selector == "2024") {
      mod_map_2024_server("map_2024_1")   # Call the map module for 2024
      mod_2024_server("2024_1")          # Call the tab module for 2024
    }
    # Add handling for 2023 if needed
  })

  # Descriptive stats
  output$descriptive_stats <- renderText({
    "Just some space to put the descriptive statistics for 2024"
  })
}




