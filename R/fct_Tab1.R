#' noiseMean_noisePeak_plot
#'
#' @description Launch the OctaveNoiseBand plots in Tab1 either showing the Mean values or the Peak values (variable = 'noiseMean' or 'noisePeak')
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
noiseMean_noisePeak_plot <- function(data, N_octaveBand) {

  library(highcharter)
  library(dplyr)

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


#' boxplotNoise
#'
#' @description Boxplot in Tab1
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
boxplotNoise <- function(data) {

  library(dplyr)
  library(highcharter)
  library(purrr)
  library(ggplot2)

  # Compute the summary statistics by octaveBand:
  data_box <- data %>%
    group_by(octaveBand) %>%
    summarise(
      low    = min(noiseMean, na.rm = TRUE),      # lower whisker from noiseMean
      q1     = quantile(noiseMean, 0.25, na.rm = TRUE),
      median = median(noiseMean, na.rm = TRUE),
      q3     = quantile(noiseMean, 0.75, na.rm = TRUE),
      high   = max(noisePeak, na.rm = TRUE)         # upper whisker from noisePeak
    ) %>%
    ungroup() %>%
    arrange(as.numeric(octaveBand))

  # Prepare the boxplot data in the format expected by Highcharts:
  box_data <- pmap(
    list(data_box$low, data_box$q1, data_box$median, data_box$q3, data_box$high),
    function(low, q1, median, q3, high) {
      list(low = low, q1 = q1, median = median, q3 = q3, high = high)
    }
  )

  # Build and return the Highcharter boxplot:
  highchart() %>%
    hc_chart(type = "boxplot") %>%
    hc_title(text = "Noise SPL Distribution by Octave Band") %>%
    hc_xAxis(
      categories = c("20", "25", "31.5", "40", "50", "63", "80", "100",
                     "125", "160", "200", "250", "315", "400", "500", "630",
                     "800", "1000", "1250", "1600", "2000", "2500", "3150",
                     "4000", "5000", "6300", "8000", "10000"),# data_box$octaveBand,
      title = list(text = "Frequency Band (Hz)"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) %>%
    hc_yAxis(
      title = list(text = "Noise SPL (dB re 1 µPa)")
    ) %>%
    hc_add_series(
      name = "Noise SPL Distribution",
      data = box_data,
      color = "#2b908f",       # Border color for the boxes
      fillColor = "#2b908f",    # Fill color inside the box
      medianColor = "#f45b5b",  # Color of the median line
      stemColor = "#f7a35c",    # Color of the stems (vertical lines)
      whiskerColor = "#8085e9", # Color of the whiskers
      whiskerLength = "20%"     # Relative length of the whiskers
    ) %>%
    hc_tooltip(
      headerFormat = "<em>Octave Band: {point.key}</em><br/>",
      pointFormat = paste(
        "Max: {point.high}",
        "Q3: {point.q3}<br>",
        "Median: {point.median}<br>",
        "Q1: {point.q1}<br>",
        "Min: {point.low}<br>"
      )
    )
}
