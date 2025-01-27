#' map_2024 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
library(leaflet)

# UI for the map module
mod_map_2024_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = "500px")
}

# Server for the map module
mod_map_2024_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -74.006, lat = 40.7128, zoom = 10) %>%
        addCircleMarkers(
          lng = c(-74.006, -74.016),
          lat = c(40.7128, 40.7228),
          label = c("Point 1", "Point 2"),
          color = "blue"
        )
    })
  })
}




## To be copied in the UI
# mod_map_2024_ui("map_2024_1")

## To be copied in the server
# mod_map_2024_server("map_2024_1")
