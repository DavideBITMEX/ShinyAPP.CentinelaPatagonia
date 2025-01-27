library(shiny)
library(highcharter)
library(dplyr)

# Function to generate the noise plot
generate_noise_plot <- function(data, variable, title, y_axis_title) {

  # Since we reload the data with read.csv (in the server), we need some extra data manipulation
  data <- data %>%
    mutate(
      date_UTC = as.POSIXct(date_UTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      date_Local = as.POSIXct(date_Local, format = "%Y-%m-%d %H:%M:%S", tz = "America/Santiago")
    )

  # Ensure POSIXct `date` is converted to milliseconds
  data <- data %>%
    mutate(
      date_Local = (as.numeric(date_Local) * 1000) - 10800000,  # Convert to milliseconds with offset
      octaveBand = as.factor(octaveBand)  # Ensure octaveBand is a factor
    )

  # Frequency ranges for each octave band
  frequency_ranges <- c(
    "22.1–44.2 Hz", "44.2–88.4 Hz", "88.4–176.8 Hz",
    "176.8–353.6 Hz", "353.6–707 Hz", "707–1414 Hz",
    "1414–2828 Hz", "2828–5657 Hz"
  )

  # Combine octave band and frequency range for legend labels
  legend_labels <- paste(levels(data$octaveBand), " (", frequency_ranges, ")")

  # Create the Highcharter plot
  plot <- highchart(type = "stock") %>%
    hc_title(text = title) %>%
    hc_xAxis(
      title = list(text = "Date"),
      type = "datetime",  # Properly format x-axis as dates
      labels = list(format = "{value:%Y-%m-%d}")  # Display readable dates
    ) %>%
    hc_yAxis(title = list(text = y_axis_title)) %>%
    hc_legend(
      enabled = TRUE,  # Ensure the legend is visible
      title = list(text = "Octave Bands"),
      align = "right",   # Place the legend to the right of the plot
      verticalAlign = "middle", # Vertically center the legend
      layout = "vertical", # Arrange the legend items vertically
      labelFormatter = JS(
        paste0(
          "function() {",
          "var labels = ['", paste(legend_labels, collapse = "','"), "'];",
          "return labels[this.index];",
          "}"
        )
      )
    ) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>Octave Band: {series.name}</b><br>", y_axis_title, ": {point.y}"
      )
    ) %>%
    hc_plotOptions(
      series = list(
        dataGrouping = list(enabled = FALSE),  # Disable data grouping
        visible = FALSE,  # Hide all series by default
        type = "line"  # Set series type to line
      )
    )

  # Dynamically add series for each octave band
  for (i in seq_along(levels(data$octaveBand))) {
    series_data <- data %>%
      filter(octaveBand == levels(data$octaveBand)[i]) %>%
      select(date_Local, all_of(variable)) %>%
      as.data.frame()

    # Check if the series_data is non-empty
    if (nrow(series_data) > 0) {
      plot <- plot %>%
        hc_add_series(
          data = list_parse2(series_data),  # Convert data to the required format
          type = "line",
          name = paste0(levels(data$octaveBand)[i], " (", frequency_ranges[i], ")"),
          visible = (i == 1)  # Make only the first series visible
        )
    }
  }

  # Add chart zoom and range selector
  plot <- plot %>%
    hc_chart(zoomType = "x") %>%
    hc_rangeSelector(
      enabled = TRUE,  # Enable the range selector
      buttons = list(),  # No buttons, so only the date selector remains
      inputEnabled = TRUE,  # Keep the date selector visible
      inputDateFormat = "%Y-%m-%d %H:%M:%S",  # Set the input date format
      inputEditDateFormat = "%Y-%m-%d %H:%M:%S"  # Format for editing dates
    )

  return(plot)
}

# Shiny App
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("plot_button", "Generate Plot")
    ),
    mainPanel(
      highchartOutput("noise_plot")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the plot
  plot_reactive <- reactiveVal(NULL)

  observeEvent(input$plot_button, {
    # Load the dataset
    Quemchi2024_NoiseBand_final <- read.csv("inst/app/www/Quemchi2024_NoiseBand_final.csv")
















    # Generate the plot
    plot_reactive(
      generate_noise_plot(
        data = Quemchi2024_NoiseBand_final,
        variable = "noiseMean",
        title = "Noise Mean Over Time",
        y_axis_title = "Noise Mean (dB)"
      )
    )
  })

  # Render the plot
  output$noise_plot <- renderHighchart({
    req(plot_reactive())  # Ensure the plot is generated before rendering
    plot_reactive()
  })
}

shinyApp(ui = ui, server = server)


 #run_app()
