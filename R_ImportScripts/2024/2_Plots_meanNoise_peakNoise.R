# Make sure you have loaded the .RData environment
library(highcharter)

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
  frequency_ranges <- c("22.1–44.2 Hz", "44.2–88.4 Hz", "88.4–176.8 Hz",
                        "176.8–353.6 Hz", "353.6–707 Hz", "707–1414 Hz",
                        "1414–2828 Hz", "2828–5657 Hz")

  # Combine octave band and frequency range for legend labels
  legend_labels <- paste(levels(df$octaveBand), " (", frequency_ranges, ")")
}
######################

######################################################
# Plot Noise Mean over time
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
  frequency_ranges <- c("22.1–44.2 Hz", "44.2–88.4 Hz", "88.4–176.8 Hz",
                        "176.8–353.6 Hz", "353.6–707 Hz", "707–1414 Hz",
                        "1414–2828 Hz", "2828–5657 Hz")
  # Combine octave band and frequency range for legend labels
  legend_labels <- paste(levels(df$octaveBand), " (", frequency_ranges, ")")

  ### Plot
  # Create the highcharter plot with stock chart type
  highchart(type = "stock") %>%
    hc_title(text = "Time Series of Noise Mean by Octave Band") %>%
    hc_xAxis(
      title = list(text = "Date"),
      type = "datetime",  # Properly format x-axis as dates
      labels = list(format = "{value:%Y-%m-%d}")  # Display readable dates
    ) %>%
    hc_yAxis(title = list(text = "Noise Mean (dB)")) %>%
    hc_legend(
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
      pointFormat = "<b>Octave Band: {series.name}</b><br>Noise Mean: {point.y}"
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
# Plot Noise Peak over time
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
    hc_yAxis(title = list(text = "Noise Peak (dB)")) %>%
    hc_legend(
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
      pointFormat = "<b>Octave Band: {series.name}</b><br>Noise Peak: {point.y}"
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
