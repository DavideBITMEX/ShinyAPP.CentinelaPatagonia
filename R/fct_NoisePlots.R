library(highcharter)
library(dplyr)

###########################################################################################
# This function allows the user to launch the OctaveNoiseBand plots
# either showing the Mean values or the Peak values (variable = 'noiseMean' or 'noisePeak')
# data = this should be the final dataset created created in /R_ImportScripts/1_ImportNoise_PAMBinaries.R
# title
# y_axis_title

# generate_noise_plot <- function(data, variable, title, y_axis_title) {
#   # Ensure POSIXct `date` is converted to milliseconds
#   data <- data %>%
#     mutate(
#       date_Local = (as.numeric(date_Local) * 1000) - 10800000,  # Convert to milliseconds with offset
#       octaveBand = as.factor(octaveBand)  # Ensure octaveBand is a factor
#     )
#
#   # Frequency ranges for each octave band
#   frequency_ranges <- c("20 Hz", "25 Hz", "31.5 Hz", "40 Hz", "50 Hz", "63 Hz", "80 Hz", "100 Hz",
#                         "125 Hz", "160 Hz", "200 Hz", "250 Hz", "315 Hz", "400 Hz", "500 Hz", "630 Hz",
#                         "800 Hz", "1 kHz", "1.25 kHz", "1.6 kHz", "2 kHz", "2.5 kHz", "3.15 kHz",
#                         "4 kHz", "5 kHz", "6.3 kHz", "8 kHz", "10 kHz")
#
#   # Combine octave band and frequency range for legend labels
#   legend_labels <- paste(levels(data$octaveBand), " (", frequency_ranges, ")")
#
#   # Create the Highcharter plot
#   plot <- highchart(type = "stock") %>%
#     hc_title(text = title) %>%
#     hc_xAxis(
#       title = list(text = "Date"),
#       type = "datetime",  # Properly format x-axis as dates
#       labels = list(format = "{value:%Y-%m-%d}")  # Display readable dates
#     ) %>%
#     hc_yAxis(title = list(text = y_axis_title)) %>%
#     hc_legend(
#       enabled = TRUE,  # Ensure the legend is visible
#       title = list(text = "Octave Bands"),
#       floating = TRUE,         # Enable floating to overlay the plot
#       align = "left",   # Place the legend to the right of the plot
#       verticalAlign = "top", # Vertically center the legend
#       layout = "vertical", # Arrange the legend items vertically
#       labelFormatter = JS(
#         paste0(
#           "function() {",
#           "var labels = ['", paste(legend_labels, collapse = "','"), "'];",
#           "return labels[this.index];",
#           "}"
#         )
#       )
#     ) %>%
#     hc_tooltip(
#       pointFormat = paste0(
#         "<b>Octave Band: {series.name}</b><br>", y_axis_title, ": {point.y}"
#       )
#     ) %>%
#     hc_plotOptions(
#       series = list(
#         dataGrouping = list(enabled = FALSE),  # Disable data grouping
#         visible = FALSE,  # Hide all series by default
#         type = "line"  # Set series type to line
#       )
#     )
#
#   # Dynamically add series for each octave band
#   for (i in seq_along(levels(data$octaveBand))) {
#     series_data <- data %>%
#       filter(octaveBand == levels(data$octaveBand)[i]) %>%
#       select(date_Local, all_of(variable)) %>%
#       as.data.frame()
#
#     # Check if the series_data is non-empty
#     if (nrow(series_data) > 0) {
#       plot <- plot %>%
#         hc_add_series(
#           data = list_parse2(series_data),  # Convert data to the required format
#           type = "line",
#           name = paste0(levels(data$octaveBand)[i], " (", frequency_ranges[i], ")"),
#           visible = (i == 1)  # Make only the first series visible
#         )
#     }
#   }
#
#   # Add chart zoom and range selector
#   plot <- plot %>%
#     hc_chart(zoomType = "x") %>%
#     hc_rangeSelector(
#       enabled = TRUE,  # Enable the range selector
#       buttons = list(),  # No buttons, so only the date selector remains
#       inputEnabled = TRUE,  # Keep the date selector visible
#       inputDateFormat = "%Y-%m-%d %H:%M:%S",  # Set the input date format
#       inputEditDateFormat = "%Y-%m-%d %H:%M:%S"  # Format for editing dates
#     )
#
#   return(plot)
# }

# # Example: Plot noiseMean
# generate_noise_plot(
#   data = Quemchi2024_NoiseBand_final,
#   variable = "noiseMean",
#   title = "Time Series of Noise SPL rms by Octave Band",
#   y_axis_title = "SPL (dB re 1 µPa) rms"
# )

# # Example: Plot noisePeak
# generate_noise_plot(
#   data = Quemchi2024_NoiseBand_final,
#   variable = "noisePeak",
#   title = "Time Series of Noise Peak by Octave Band",
#   y_axis_title = "Noise SPL Peak (dB re 1 µPa)"
# )
###########################################################################################


###########################################################################################
noiseMean_noisePeak_plot <- function(data, N_octaveBand) {
  # Convert date_Local and octaveBand properly
  data <- data %>%
    mutate(
      date_Local = (as.numeric(date_Local) * 1000) - 10800000,  # Convert to milliseconds with offset
      octaveBand = as.factor(octaveBand)  # Ensure octaveBand is a factor
    )

  # Frequency ranges for each octave band
  frequency_ranges <- c("20 Hz", "25 Hz", "31.5 Hz", "40 Hz", "50 Hz", "63 Hz", "80 Hz", "100 Hz",
                        "125 Hz", "160 Hz", "200 Hz", "250 Hz", "315 Hz", "400 Hz", "500 Hz", "630 Hz",
                        "800 Hz", "1 kHz", "1.25 kHz", "1.6 kHz", "2 kHz", "2.5 kHz", "3.15 kHz",
                        "4 kHz", "5 kHz", "6.3 kHz", "8 kHz", "10 kHz")

  # Create the Highcharter plot
  plot <- highchart(type = "stock") %>%
    #hc_title(text = "Time Series of Noise SPL (rms) and Noise Peak by Octave Band") %>%
    hc_xAxis(
      title = list(text = "Date"),
      type = "datetime",  # Properly format x-axis as dates
      labels = list(format = "{value:%Y-%m-%d}")  # Display readable dates
    ) %>%
    hc_yAxis(title = list(text = "SPL (dB re 1 µPa)")) %>%
    hc_legend(
      enabled = TRUE,  # Ensure the legend is visible
      title = list(text = "1/3 Octave Bands values"),
      floating = TRUE,         # Enable floating to overlay the plot
      align = "left",          # Place the legend on the left
      verticalAlign = "top",   # Place the legend at the top
      layout = "vertical"      # Arrange the legend items vertically
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b><br>Value: {point.y}"
    ) %>%
    hc_plotOptions(
      series = list(
        dataGrouping = list(enabled = FALSE),  # Disable data grouping
        type = "line"                          # Set series type to line
      )
    ) %>%
    # Add the Noise SPL (rms) series for the specified Octave Band
    hc_add_series(
      data = data %>%
        filter(as.numeric(as.character(octaveBand)) == N_octaveBand) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(N_octaveBand, " - Noise SPL rms (", frequency_ranges[N_octaveBand], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = TRUE
    ) %>%
    # Add the Noise Peak series for the specified Octave Band
    hc_add_series(
      data = data %>%
        filter(as.numeric(as.character(octaveBand)) == N_octaveBand) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(N_octaveBand, " - Noise Peak (", frequency_ranges[N_octaveBand], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = TRUE
    ) %>%
    # Add chart zoom and range selector
    hc_chart(zoomType = "x") %>%
    hc_rangeSelector(
      enabled = TRUE,                     # Enable the range selector
      buttons = list(),                   # No buttons, so only the date selector remains
      inputEnabled = FALSE,                # Keep the date selector visible
      inputDateFormat = "%Y-%m-%d %H:%M:%S",# Set the input date format
      inputEditDateFormat = "%Y-%m-%d %H:%M:%S"  # Format for editing dates
    )

  return(plot)
}

# noiseMean_noisePeak_plot(
#   data = Quemchi2024_NoiseBand_final,
#   N_octaveBand = 1)
###########################################################################################
