#' DayNightBoxPlot
#'
#' @description A function to create a highchart boxplot of noise levels by octave band and time of day.
#'
#' @return A highchart object.
#' @noRd
DayNightBoxPlot <- function(data, signif_vector, CentralFreq, highlight_bands = c("6", "9")) {
  # Load required packages (if not already loaded)
  library(dplyr)
  library(highcharter)

  # Helper to convert each row of a data.frame to an unnamed list
  list_parse_no_names <- function(df) {
    lapply(seq_len(nrow(df)), function(i) {
      unname(as.list(df[i, ]))
    })
  }

  # Helper to remove names from a vector (for boxplot stats)
  unname_box_stats <- function(x) unname(x)

  ###############################################################
  # Prepare data and calculate boxplot statistics
  ###############################################################
  unique_bands <- sort(unique(data$octaveBand))

  # Create a factor for the octave bands
  plot_data <- data %>%
    mutate(octaveBandFactor = factor(octaveBand,
                                     levels = unique_bands,
                                     labels = as.character(unique_bands)))

  # Compute boxplot statistics (using noiseMean) for Day and Night
  bp_stats <- plot_data %>%
    group_by(octaveBandFactor, Day_Night) %>%
    summarise(
      ymin   = min(noiseMean, na.rm = TRUE),
      lower  = quantile(noiseMean, 0.25, na.rm = TRUE),
      middle = median(noiseMean, na.rm = TRUE),
      upper  = quantile(noiseMean, 0.75, na.rm = TRUE),
      ymax   = max(noiseMean, na.rm = TRUE)
    ) %>%
    ungroup()

  # Compute maximum noisePeak for each (for Day and Night)
  max_peak <- plot_data %>%
    group_by(octaveBandFactor, Day_Night) %>%
    summarise(max_noisePeak = max(noisePeak, na.rm = TRUE)) %>%
    ungroup()

  # Overall maximum value for scaling
  overallMaxVal <- max(bp_stats$ymax, max_peak$max_noisePeak, na.rm = TRUE)

  # Prepare boxplot series data for "Day"
  bp_day <- bp_stats %>%
    filter(Day_Night == "Day") %>%
    arrange(as.numeric(as.character(octaveBandFactor)))
  day_boxplot_data <- bp_day %>%
    rowwise() %>%
    mutate(vals = list(unname_box_stats(c(ymin, lower, middle, upper, ymax)))) %>%
    pull(vals)

  # Prepare boxplot series data for "Night"
  bp_night <- bp_stats %>%
    filter(Day_Night == "Night") %>%
    arrange(as.numeric(as.character(octaveBandFactor)))
  night_boxplot_data <- bp_night %>%
    rowwise() %>%
    mutate(vals = list(unname_box_stats(c(ymin, lower, middle, upper, ymax)))) %>%
    pull(vals)

  # Prepare scatter series data for max noisePeak (Day)
  max_day <- max_peak %>%
    filter(Day_Night == "Day") %>%
    arrange(as.numeric(as.character(octaveBandFactor))) %>%
    mutate(x = as.numeric(as.character(octaveBandFactor)) - 1) %>%
    select(x, y = max_noisePeak)
  max_day_list <- list_parse_no_names(max_day)

  # Prepare scatter series data for max noisePeak (Night)
  max_night <- max_peak %>%
    filter(Day_Night == "Night") %>%
    arrange(as.numeric(as.character(octaveBandFactor))) %>%
    mutate(x = as.numeric(as.character(octaveBandFactor)) - 1) %>%
    select(x, y = max_noisePeak)
  max_night_list <- list_parse_no_names(max_night)

  ###############################################################
  # Create plotBands for highlighted octave bands (0-indexed)
  ###############################################################
  highlight_indices <- as.numeric(highlight_bands) - 1
  plotBands <- lapply(highlight_indices, function(i) {
    list(from = i - 0.5, to = i + 0.5, color = "rgba(16,16,16,0.2)")
  })

  ###############################################################
  # Define colors (with transparency) for Day and Night
  ###############################################################
  day_color <- adjustcolor("#E31A1C", alpha.f = 0.5)
  night_color <- adjustcolor("#1F78B4", alpha.f = 0.5)

  ###############################################################
  # Build the Highchart
  ###############################################################
  hc <- highchart() %>%
    hc_chart(type = "boxplot") %>%
    hc_chart(zoomType = "xy") %>%
    hc_title(text = "Noise Levels by Third Octave Bands grouped by Day & Night") %>%
    hc_xAxis(
      categories = CentralFreq,  # Use the input CentralFreq vector
      title = list(text = "Frequency (Hz)", style = list(fontWeight = "bold")),
      labels = list(style = list(fontWeight = "bold")),
      plotBands = plotBands
    ) %>%
    hc_yAxis(
      title = list(text = "1/3 Octave SPL RMS (dB re 1 µPa)", style = list(fontWeight = "bold")),
      labels = list(style = list(fontWeight = "bold")),
      max = overallMaxVal + 20
    ) %>%
    # Add boxplot series for "Day"
    hc_add_series(
      name = "Day",
      data = day_boxplot_data,
      type = "boxplot",
      color = "#E31A1C",
      fillColor = day_color
    ) %>%
    # Add boxplot series for "Night"
    hc_add_series(
      name = "Night",
      data = night_boxplot_data,
      type = "boxplot",
      color = "#1F78B4",
      fillColor = night_color
    ) %>%
    # Add scatter series for max noisePeak (Day)
    hc_add_series(
      name = "Max noisePeak (Day)",
      data = max_day_list,
      type = "scatter",
      color = day_color,
      marker = list(symbol = "circle", radius = 3),
      tooltip = list(pointFormatter = JS("function() {
         return 'Max noisePeak: ' + Highcharts.numberFormat(this.y, 2);
      }"))
    ) %>%
    # Add scatter series for max noisePeak (Night)
    hc_add_series(
      name = "Max noisePeak (Night)",
      data = max_night_list,
      type = "scatter",
      color = night_color,
      marker = list(symbol = "circle", radius = 3),
      tooltip = list(pointFormatter = JS("function() {
         return 'Max noisePeak: ' + Highcharts.numberFormat(this.y, 2);
      }"))
    )

  ###############################################################
  # Global tooltip formatter using signif_vector from input
  ###############################################################
  # Manually construct a JS array literal from signif_vector
  sig_js <- paste0("[", paste(sprintf('"%s"', signif_vector), collapse = ","), "]")
  hc <- hc %>%
    hc_tooltip(
      useHTML = TRUE,
      formatter = JS(paste0("
         function() {
           if (this.series.type === 'boxplot') {
               var cat = this.series.xAxis.categories[this.point.x];
               var sigMapping = ", sig_js, ";
               var sigText = (sigMapping[this.point.x] === 'yes') ? 'Yes' : 'No';
               return '<b>[' + cat + ' Hz] ' + this.series.name + '</b><br/>' +
                      'Min: ' + Highcharts.numberFormat(this.point.low, 2) + '<br/>' +
                      'Q1: ' + Highcharts.numberFormat(this.point.q1, 2) + '<br/>' +
                      '<b>Median: ' + Highcharts.numberFormat(this.point.median, 2) + '</b><br/>' +
                      'Q3: ' + Highcharts.numberFormat(this.point.q3, 2) + '<br/>' +
                      'Max: ' + Highcharts.numberFormat(this.point.high, 2) + '<br/>' +
                      '<b>Significant: ' + sigText + '</b>';
           } else if (this.series.type === 'scatter') {
               return 'Max noisePeak: ' + Highcharts.numberFormat(this.point.y, 2);
           } else {
               return this.point.y;
           }
         }
      "))
    ) %>%
    hc_legend(layout = "horizontal", align = "center", verticalAlign = "top")

  return(hc)
}




#' DayNightBoxPlotOneBand (NOT IN USE AT THE MOMENT)
#'
#' @description A function to create a highchart boxplot of noise levels by octave band and time of day.
#'
#' @return A highchart object.
#' @noRd
DayNightBoxPlotOneBand <- function(data, NoctaveBand, centreFreq) {
  library(dplyr)
  library(highcharter)

  # Filter data for the specified octave band
  data_filtered <- data %>% filter(octaveBand == NoctaveBand)

  head(data_filtered)

  if(nrow(data_filtered) == 0) {
    return(highchart() %>%
             hc_title(text = paste("No data for octave band", octaveBand)))
  }

  # Compute boxplot statistics for noiseMean by Day/Night
  stats <- data_filtered %>%
    group_by(Day_Night) %>%
    summarise(
      ymin   = min(noiseMean, na.rm = TRUE),
      lower  = quantile(noiseMean, 0.25, na.rm = TRUE),
      middle = median(noiseMean, na.rm = TRUE),
      upper  = quantile(noiseMean, 0.75, na.rm = TRUE),
      ymax   = max(noiseMean, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(match(Day_Night, c("Day", "Night")))

  # Extract the five-number summaries for Day and Night
  day_box <- as.numeric(stats %>% filter(Day_Night == "Day") %>% select(ymin, lower, middle, upper, ymax))
  night_box <- as.numeric(stats %>% filter(Day_Night == "Night") %>% select(ymin, lower, middle, upper, ymax))

  # Define colors (using your red and blue scheme)
  day_color <- adjustcolor("#E31A1C", alpha.f = 0.5)
  night_color <- adjustcolor("#1F78B4", alpha.f = 0.5)

  # Build the highchart boxplot:
  hc <- highchart() %>%
    hc_chart(type = "boxplot") %>%
    hc_title(text = paste("Noise Levels for Octave Band", centreFreq, "Hz")) %>%
    hc_xAxis(
      categories = c("Day", "Night"),
      title = list(text = NULL),
      labels = list(style = list(fontWeight = "bold"))
    ) %>%
    hc_yAxis(
      title = list(text = NULL)
    ) %>%
    hc_add_series(
      name = paste(centreFreq, "Hz"),
      data = list(day_box, night_box)
    ) %>%
    hc_plotOptions(boxplot = list(
      stemWidth = 2,
      whiskerLength = "50%",
      lineWidth = 2,
      medianColor = "black",   # Sets the median line color to red
      medianWidth = 1
      )) %>%
    hc_legend(enabled = FALSE)

  return(hc)
}

