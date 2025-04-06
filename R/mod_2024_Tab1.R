#' 2024_Tab1 UI Function
#'
#' @description A Shiny Module with a clean layout for the 2024 Campaign.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2024_Tab1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useWaiter(),
    # Title at the top
    fluidRow(
      column(
        width = 12,
        align = "center",
        h2(
          "2024 Campaign - Caleta Alaman",
          style = "border: 3px solid #111184; background-color: #111184; color: white; padding: 5px 10px; border-radius: 10px; display: inline-block;"
        )
      )
    )
    ,
    # Separator
    hr(),
    # Map and Campaign Information Row
    fluidRow(
      # Left: Leaflet Map
      column(
        width = 6,
        leafletOutput(ns("map"), height = "400px")
      ),
      # Right: Campaign Information and Input Selector
      column(
        width = 6,
        # Campaign information with labels in bold
        div(
          HTML("
            <div style='font-size: 16px;'>
              <p><strong>Start of Recording:</strong> Tuesday 5th November 2024, 13:35 (Local time)</p>
              <p><strong>End of Recording:</strong> Saturday 9th Nov 2024, 12:25 (Local time)</p>
              <hr>
              <p><strong>Device:</strong> SoundTrap ST300STD (ID: 5483)</p>
              <p><strong>Sensitivity:</strong> -175.9 dB</p>
              <p><strong>Sample Rate:</strong> 32 kHz</p>
              <hr>
              <p><strong>Additional Info:</strong> Third-octave noise level measurements were calculated for 28 frequency bands, with the highest frequency centered at 10 kHz</p>
            </div>
          ")
        ),
        br(),
        # Octave Band Selector
        selectInput(
          inputId = ns("N_octaveBand"),
          label = "Select Frequency Band to visualize in the Plot:",
          choices = setNames(
            1:28,
            c("20 Hz", "25 Hz", "31.5 Hz", "40 Hz", "50 Hz", "63 Hz", "80 Hz", "100 Hz",
              "125 Hz", "160 Hz", "200 Hz", "250 Hz", "315 Hz", "400 Hz", "500 Hz", "630 Hz",
              "800 Hz", "1 kHz", "1.25 kHz", "1.6 kHz", "2 kHz", "2.5 kHz", "3.15 kHz",
              "4 kHz", "5 kHz", "6.3 kHz", "8 kHz", "10 kHz")
          ),
          selected = 6,
          width = "100%"
        )
      )
    ),
    br(),
    # Time Series Plot Row
    fluidRow(
      column(
        width = 12,
        highchartOutput(ns("noisePlot"), width = "100%", height = "400px")
      )
    )
  )
}



#' 2024_Tab1 Server Functions
#'
#' @noRd
mod_2024_Tab1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render Leaflet Map
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(lng = -73.4743189, lat = -42.1427629, zoom = 12) %>%  # Centered near Chiloé
        addTiles() %>%  # Default OSM tiles
        addMarkers(
          lng = -73.4743189,
          lat = -42.1427629,
          popup = "Marker in the water near Chiloé",
          layerId = ns("chiloe_location")
        )
    })

    # Render the Time Series Plot based on the selected octave band
    output$noisePlot <- renderHighchart({
      req(input$N_octaveBand)
      noiseMean_noisePeak_plot(
        data = Quemchi2024_NoiseBand_final,
        N_octaveBand = as.numeric(input$N_octaveBand)
      )
    })
  })
}


## To be copied in the UI
# mod_2024_Tab1_ui("2024_Tab1_1")

## To be copied in the server
# mod_2024_Tab1_server("2024_Tab1_1")
