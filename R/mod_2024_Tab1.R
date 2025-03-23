#' 2024_Tab1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
library(DT)

mod_2024_Tab1_ui <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 4,
        bs4Dash::bs4Card(
          title = HTML(paste0(
            "<span style='font-family: \"headermedium\", Arial, sans-serif; font-size: 20px'>",
            "2024 Campaign",
            "</span>"
          )),
          status = "primary",
          elevation = 2,
          width = NULL,  # Adatta la larghezza dinamicamente
          solidHeader = TRUE,
          collapsible = FALSE,
          style = "padding: 0; margin: 0;",  # Rimuove spazio interno ed esterno
          leafletOutput(ns("map"), height = "500px"),  # Map Leaflet
          tags$style(
            ".leaflet-control-attribution { font-size: 12px; }"  # Stile per il controllo delle attribuzioni
          )
        )
      ),

      column(
        width = 8,
        fluidRow(

          column(
            width = 4,
            div(
              style = "height:500px; overflow:auto;",
              DT::dataTableOutput(ns("stats_table"))
            )
          ),
          column(
            width = 8,
            div(
              style = "height:500px;",
              highchartOutput(ns("stats_boxplot"), height = "500px")
              )
            ),
          )
        )

        ), # first fluidrow


    fluidRow(
      # Flex container for title and selectInput
      div(
        style = "display: flex; align-items: center; justify-content: center; gap: 20px; width: 100%; margin-bottom: -30px; margin-top: -20px;",
        tags$h5("Time Series of Noise SPL by Octave Band", style = "margin: 0;"),
        div(
          style = "margin-bottom: -10px;",
        selectInput(
          inputId = ns("N_octaveBand"),
          label = NULL,
          choices = setNames(
            1:28,
            c("20 Hz", "25 Hz", "31.5 Hz", "40 Hz", "50 Hz", "63 Hz", "80 Hz", "100 Hz",
              "125 Hz", "160 Hz", "200 Hz", "250 Hz", "315 Hz", "400 Hz", "500 Hz", "630 Hz",
              "800 Hz", "1 kHz", "1.25 kHz", "1.6 kHz", "2 kHz", "2.5 kHz", "3.15 kHz",
              "4 kHz", "5 kHz", "6.3 kHz", "8 kHz", "10 kHz")
          ),
          selected = 6,
          width = "200px"  # adjust as needed
          )
        )
      ),
      # Full-width plot below the flex container
      highchartOutput(ns("noisePlot"), width = "100%", height = "400px")
    )



    )


}

#' 2024_Tab1 Server Functions
#'
#' @noRd
mod_2024_Tab1_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #############################
    ### Render Leaflet Map ######
    #############################
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(lng = -73.4743189, lat = -42.1427629, zoom = 12) %>%  # Center on a water location near Chiloé
        addTiles() %>%  # Default OSM tiles
        addMarkers(
          lng = -73.4743189,
          lat = -42.1427629,
          popup = "Marker in the water near Chiloé",
          layerId = ns("chiloe_location")
        )
    })

    #######################################
    ### Actions when clicking on Map ######
    #######################################
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      # Check if the clicked marker is the one we added (its id is namespaced)
      if (!is.null(click$id) && click$id == ns("chiloe_location")) {
        showModal(
          modalDialog(
            title = "Marker Clicked",
            "You clicked on the marker in the water near Chiloé!",
            easyClose = TRUE
          )
        )
        # You can also trigger other actions here, e.g. update inputs, change plots, etc.
      }
    })


    ############################
    ### Updating Stats Table ###
    ############################
    output$stats_table <- DT::renderDataTable({
      # Define frequency ranges corresponding to each octaveBand value
      frequency_ranges <- c("20 Hz", "25 Hz", "31.5 Hz", "40 Hz", "50 Hz", "63 Hz", "80 Hz", "100 Hz",
                            "125 Hz", "160 Hz", "200 Hz", "250 Hz", "315 Hz", "400 Hz", "500 Hz", "630 Hz",
                            "800 Hz", "1 kHz", "1.25 kHz", "1.6 kHz", "2 kHz", "2.5 kHz", "3.15 kHz",
                            "4 kHz", "5 kHz", "6.3 kHz", "8 kHz", "10 kHz")

      # Compute the summary statistics and add the frequency range to the octaveBand label
      df <- Quemchi2024_NoiseBand_final %>%
        group_by(octaveBand) %>%
        summarise(
          Mean   = round(mean(noiseMean, na.rm = TRUE), 2),
          Median = round(median(noiseMean, na.rm = TRUE), 2),
          Max    = round(max(noisePeak, na.rm = TRUE), 2)
        ) %>%
        ungroup() %>%
        mutate(
          Frequency_Band = paste0(octaveBand, " (", frequency_ranges[as.numeric(octaveBand)], ")")

        ) %>%
        select(Frequency_Band, Mean, Median, Max)

      # Create the DT datatable with custom options and header styling
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          #dom = 't',           # only show the table body (remove search, pagination)
          ordering = FALSE    # disable sorting if not needed
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; font-size: 20px; font-weight: bold; margin-bottom: 10px;',
          'Summary Statistics for Noise Measurements'
        )
      )
    })





    ##############################
    ### Updating Stats Boxplot ###
    ##############################
    output$stats_boxplot <- renderHighchart({
      boxplotNoise(Quemchi2024_NoiseBand_final)
    })



    ###########################
    ### Updating lower plot ###
    ###########################
    output$noisePlot <- renderHighchart({
      req(input$N_octaveBand)  # Ensure input$N_octaveBand is available
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
