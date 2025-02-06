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
            1:8,
            c("1 (44.2–88.4 Hz)", "2 (88.4–176.8 Hz)",
              "3 (176.8–353.6 Hz)", "4 (353.6–707 Hz)",
              "5 (707–1414 Hz)", "6 (1414–2828 Hz)",
              "7 (2828–5657 Hz)", "8 (5656-11310 Hz)")
          ),
          selected = 1,
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

    ############################
    ### Updating Stats Table ###
    ############################
    output$stats_table <- DT::renderDataTable({
      # Define frequency ranges corresponding to each octaveBand value
      frequency_ranges <- c(
        "44.2–88.4 Hz", "88.4–176.8 Hz", "176.8–353.6 Hz", "353.6–707 Hz",
        "707–1414 Hz", "1414–2828 Hz", "2828–5657 Hz", "5656-11310 Hz"
      )

      # Compute the summary statistics and add the frequency range to the octaveBand label
      df <- Quemchi2024_NoiseBand %>%
        group_by(octaveBand) %>%
        summarise(
          Mean   = round(mean(noiseMean, na.rm = TRUE), 2),
          Median = round(median(noiseMean, na.rm = TRUE), 2),
          Max    = round(max(noisePeak, na.rm = TRUE), 2)
        ) %>%
        ungroup() %>%
        mutate(
          OctaveBand = paste0(octaveBand, " (", frequency_ranges[as.numeric(octaveBand)], ")")

        ) %>%
        select(OctaveBand, Mean, Median, Max)

      # Create the DT datatable with custom options and header styling
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          dom = 't',           # only show the table body (remove search, pagination)
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
      boxplotNoise(Quemchi2024_NoiseBand)
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
