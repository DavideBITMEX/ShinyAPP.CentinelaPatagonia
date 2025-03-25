### Don't use


# Make sure you have loaded the .RData environment
library(highcharter)
library(dplyr)
library(purrr)

######################
# Data Manipulation
# Needed for the plots
{
  # Convert POSIXct `date` to milliseconds since the epoch (UNIX time)
  df <- Quemchi2024_NoiseBand_final
  df$date_UTC <- as.numeric(df$date_UTC) * 1000  # Convert to milliseconds
  df$date_Local <- (as.numeric(df$date_Local) * 1000) - 10800000  # Convert to milliseconds (removed 3 hours (10800000 ms) because otherwise it's in UTC)

  # Ensure octaveBand is treated as a factor for grouping
  df$octaveBand <- as.factor(df$octaveBand)

  # Frequency ranges for each octave band
  frequency_ranges <- c("44.2–88.4 Hz", "88.4–176.8 Hz",
                        "176.8–353.6 Hz", "353.6–707 Hz", "707–1414 Hz",
                        "1414–2828 Hz", "2828–5657 Hz", "5657-11310 Hz")

  # Combine octave band and frequency range for legend labels
  legend_labels <- paste(levels(df$octaveBand), " (", frequency_ranges, ")")
}
######################

######################################################
# Plot Noise Mean over time (all Bands)
# Highcharter plot
{
  ### Some data manipulation
  # Convert POSIXct `date` to milliseconds since the epoch (UNIX time)
  df <- Quemchi2024_NoiseBand_final
  df$date_UTC <- as.numeric(df$date_UTC) * 1000  # Convert to milliseconds
  df$date_Local <- (as.numeric(df$date_Local) * 1000) - 10800000  # Convert to milliseconds (removed 3 hours (10800000 ms) because otherwise it's in UTC)
  # Ensure octaveBand is treated as a factor for grouping
  df$octaveBand <- as.factor(df$octaveBand)
  # Frequency ranges for each octave band
  frequency_ranges <- c("44.2–88.4 Hz", "88.4–176.8 Hz",
                        "176.8–353.6 Hz", "353.6–707 Hz", "707–1414 Hz",
                        "1414–2828 Hz", "2828–5657 Hz", "5657-11310 Hz")
  # Combine octave band and frequency range for legend labels
  legend_labels <- paste(levels(df$octaveBand), " (", frequency_ranges, ")")

  ### Plot
  # Create the highcharter plot with stock chart type
  highchart(type = "stock") %>%
    hc_title(text = "Time Series of Noise SPL rms by Octave Band") %>%
    hc_xAxis(
      title = list(text = "Date"),
      type = "datetime",  # Properly format x-axis as dates
      labels = list(format = "{value:%Y-%m-%d}")  # Display readable dates
    ) %>%
    hc_yAxis(title = list(text = "SPL (dB re 1 µPa) rms")) %>%
    hc_legend(
      title = list(text = "Octave Bands"),
      floating = TRUE,         # Enable floating to overlay the plot
      align = "left",   # Place the legend to the right of the plot
      verticalAlign = "top", # Vertically center the legend
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
      pointFormat = "<b>Octave Band: {series.name}</b><br>SPL rms: {point.y}"
    ) %>%
    hc_plotOptions(
      series = list(
        dataGrouping = list(enabled = FALSE),  # Disable data grouping
        visible = FALSE,  # Hide all series by default
        type = "line"  # Set series type to line
      )
    ) %>%
    # Add series for each octave band
    # Octave 1
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[1]) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[1], " (", frequency_ranges[1], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = TRUE  # Make the first octave band visible
    ) %>%
    # Octave 2
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[2]) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[2], " (", frequency_ranges[2], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 3
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[3]) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[3], " (", frequency_ranges[3], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 4
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[4]) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[4], " (", frequency_ranges[4], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 5
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[5]) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[5], " (", frequency_ranges[5], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 6
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[6]) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[6], " (", frequency_ranges[6], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 7
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[7]) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[7], " (", frequency_ranges[7], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 8
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[8]) %>%
        select(date_Local, noiseMean) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[8], " (", frequency_ranges[8], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    hc_legend(enabled = TRUE) %>%
    hc_chart(zoomType = "x") %>%  # Enable zooming on the x-axis
    hc_rangeSelector(
      enabled = TRUE,  # Enable the range selector
      buttons = list(),  # No buttons, so only the date selector remains
      inputEnabled = TRUE,  # Keep the date selector visible
      inputDateFormat = "%Y-%m-%d %H:%M:%S",  # Set the input date format
      inputEditDateFormat = "%Y-%m-%d %H:%M:%S"  # Format for editing dates
    )
}
######################################################


######################################################
# Plot Noise Peak over time (all Bands)
# Highcharter plot
{
  # Create the highcharter plot with stock chart type
  highchart(type = "stock") %>%
    hc_title(text = "Time Series of Noise Peak by Octave Band") %>%
    hc_xAxis(
      title = list(text = "Date"),
      type = "datetime",  # Properly format x-axis as dates
      labels = list(format = "{value:%Y-%m-%d}")  # Display readable dates
    ) %>%
    hc_yAxis(title = list(text = "Noise SPL Peak (dB re 1 µPa)")) %>%
    hc_legend(
      title = list(text = "Octave Bands"),
      floating = TRUE,         # Enable floating to overlay the plot
      align = "left",   # Place the legend to the right of the plot
      verticalAlign = "top", # Vertically center the legend
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
      pointFormat = "<b>Octave Band: {series.name}</b><br>Noise SPL Peak: {point.y}"
    ) %>%
    hc_plotOptions(
      series = list(
        dataGrouping = list(enabled = FALSE),  # Disable data grouping
        visible = FALSE,  # Hide all series by default
        type = "line"  # Set series type to line
      )
    ) %>%
    # Add series for each octave band
    # Octave 1
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[1]) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[1], " (", frequency_ranges[1], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = TRUE  # Make the first octave band visible
    ) %>%
    # Octave 2
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[2]) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[2], " (", frequency_ranges[2], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 3
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[3]) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[3], " (", frequency_ranges[3], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 4
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[4]) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[4], " (", frequency_ranges[4], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 5
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[5]) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[5], " (", frequency_ranges[5], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 6
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[6]) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[6], " (", frequency_ranges[6], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 7
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[7]) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[7], " (", frequency_ranges[7], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    # Octave 8
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[8]) %>%
        select(date_Local, noisePeak) %>%
        as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[8], " (", frequency_ranges[8], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = FALSE  # Make the first octave band visible
    ) %>%
    hc_legend(enabled = TRUE) %>%
    hc_chart(zoomType = "x") %>%  # Enable zooming on the x-axis
    hc_rangeSelector(
      enabled = TRUE,  # Enable the range selector
      buttons = list(),  # No buttons, so only the date selector remains
      inputEnabled = TRUE,  # Keep the date selector visible
      inputDateFormat = "%Y-%m-%d %H:%M:%S",  # Set the input date format
      inputEditDateFormat = "%Y-%m-%d %H:%M:%S"  # Format for editing dates
    )
}
######################################################


######################################################
# Plot Noise Mean AND Noise Peak over time (1 octave band at a time)
# Highcharter plot
{
  ### Some data manipulation
  # Convert POSIXct `date` to milliseconds since the epoch (UNIX time)
  df <- Quemchi2024_NoiseBand_final
  df$date_UTC <- as.numeric(df$date_UTC) * 1000  # Convert to milliseconds
  df$date_Local <- (as.numeric(df$date_Local) * 1000) - 10800000  # Convert to milliseconds (removed 3 hours (10800000 ms) because otherwise it's in UTC)
  # Ensure octaveBand is treated as a factor for grouping
  df$octaveBand <- as.factor(df$octaveBand)
  # Frequency ranges for each octave band
  frequency_ranges <- c("44.2–88.4 Hz", "88.4–176.8 Hz",
                        "176.8–353.6 Hz", "353.6–707 Hz", "707–1414 Hz",
                        "1414–2828 Hz", "2828–5657 Hz", "5657-11310 Hz")
  # Combine octave band and frequency range for legend labels
  legend_labels <- paste(levels(df$octaveBand), " (", frequency_ranges, ")")

  ### Plot
  # Create the highcharter plot with stock chart type
  highchart(type = "stock") %>%
    hc_title(text = "Time Series of Noise SPL (rms) and Noise Peak by Octave Band") %>%
    hc_xAxis(
      title = list(text = "Date"),
      type = "datetime",
      labels = list(format = "{value:%Y-%m-%d}")
    ) %>%
    hc_yAxis(title = list(text = "SPL (dB re 1 µPa)")) %>%
    hc_legend(
      enabled = TRUE,
      title = list(text = "Octave Band values"),
      floating = TRUE,       # Overlay legend inside the plot area
      align = "left",        # Place the legend on the left
      verticalAlign = "top", # Place it at the top
      layout = "vertical"    # Stack legend items vertically
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b><br>Value: {point.y}"
    ) %>%
    hc_plotOptions(
      series = list(
        dataGrouping = list(enabled = FALSE),  # Disable data grouping
        type = "line"
        # Remove the global visible = FALSE here so that individual series settings work
      )
    ) %>%
    # Add the Noise SPL (rms) series for Octave Band 1
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[1]) %>%
        select(date_Local, noiseMean) %>% as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[1], " - Noise SPL rms (", frequency_ranges[1], ")"),
      hcaes(x = date_Local, y = noiseMean),
      visible = TRUE
    ) %>%
    # Add the Noise Peak series for Octave Band 1
    hc_add_series(
      data = df %>% filter(octaveBand == levels(df$octaveBand)[1]) %>%
        select(date_Local, noisePeak) %>% as.data.frame(),
      type = "line",
      name = paste0(levels(df$octaveBand)[1], " - Noise Peak (", frequency_ranges[1], ")"),
      hcaes(x = date_Local, y = noisePeak),
      visible = TRUE
    ) %>%
    hc_chart(zoomType = "x") %>%  # Enable x-axis zooming
    hc_rangeSelector(
      enabled = TRUE,
      buttons = list(),
      inputEnabled = TRUE,
      inputDateFormat = "%Y-%m-%d %H:%M:%S",
      inputEditDateFormat = "%Y-%m-%d %H:%M:%S"
    )

}
######################################################



######################################################
#
### prepare data
# since PAMGuard gives us noiseMean and noisePeak, we calculate:
# min, Q1, Median and Q3 based on noiseMean
# max based on noisePeak
{
data_box <- Quemchi2024_NoiseBand %>%
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

box_data <- pmap(
  list(data_box$low, data_box$q1, data_box$median, data_box$q3, data_box$high),
  function(low, q1, median, q3, high) {
    list(low = low, q1 = q1, median = median, q3 = q3, high = high)
  }
)

}
### Highcharter plot
{
  hc_box <- highchart() %>%
    hc_chart(type = "boxplot") %>%
    hc_title(text = "Noise SPL Distribution by Octave Band") %>%
    hc_xAxis(
      categories = data_box$octaveBand,
      title = list(text = "Octave Band")
    ) %>%
    hc_yAxis(
      title = list(text = "Noise SPL (dB re 1 µPa)")
    ) %>%
    hc_add_series(
      name = "Noise SPL Distribution",
      data = box_data,
      # Custom colors for the boxplot elements:
      color = "#2b908f",         # Border color for the boxes
      fillColor = "#2b908f",      # Fill color inside the box
      medianColor = "#f45b5b",    # Color of the median line
      stemColor = "#f7a35c",      # Color of the stems (vertical lines)
      whiskerColor = "#8085e9",   # Color of the whiskers
      whiskerLength = "20%"
    ) %>%
    hc_tooltip(
      headerFormat = "<em>Octave Band: {point.key}</em><br/>",
      pointFormat = "Min: {point.low}<br>Q1: {point.q1}<br>Median: {point.median}<br>Q3: {point.q3}<br>Max: {point.high}"
    )

}
######################################################
