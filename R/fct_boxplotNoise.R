#' boxplotNoise
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
library(dplyr)
library(highcharter)
library(purrr)
library(ggplot2)


boxplotNoise <- function(data) {
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

