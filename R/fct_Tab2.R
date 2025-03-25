#' DayNightBoxPlot
#'
#' @description A function to create a highchart boxplot of noise levels by frequency bands and time of day, highlighting important frequency bands and significance levels between day and night.
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



#' single_boxplot
#'
#' @description A function to create a boxplot of noise levels for a specific frequency band, grouped by day and night.
#'
#' @return A highchart object.
#' @noRd
single_boxplot <- function(data, selected_band) {
  # Filter the data for the selected octave band
  data_filtered <- data %>% filter(octaveBand == selected_band)

  # If there's no data for the selected band, return a placeholder chart
  if(nrow(data_filtered) == 0) {
    return(
      highchart() %>%
        hc_title(text = "No data available for the selected filters.")
    )
  }

  # Calculate boxplot statistics for each Day_Night group
  box_stats <- data_filtered %>%
    group_by(Day_Night) %>%
    summarise(
      low  = round(min(noiseMean, na.rm = TRUE), 2),
      q1   = round(quantile(noiseMean, 0.25, na.rm = TRUE), 2),
      med  = round(median(noiseMean, na.rm = TRUE), 2),
      q3   = round(quantile(noiseMean, 0.75, na.rm = TRUE), 2),
      high = round(max(noiseMean, na.rm = TRUE), 2)
    ) %>%
    ungroup()

  # Build and return the highchart boxplot
  highchart() %>%
    hc_chart(type = "boxplot") %>%
    hc_title(text = "NoiseMean by Day/Night") %>%
    hc_xAxis(
      categories = as.character(box_stats$Day_Night),
      title = list(text = "Period"),
      gridLineWidth = 1,
      lineColor = "#ccc"
    ) %>%
    hc_yAxis(
      title = list(text = "NoiseMean (SPL RMS)"),
      gridLineDashStyle = "ShortDot"
    ) %>%
    hc_add_series(
      data = box_stats,
      name = "NoiseMean",
      showInLegend = FALSE  # Prevents the series from being clickable in the legend
    ) %>%
    hc_plotOptions(boxplot = list(
      stemWidth = 2,
      whiskerLength = "50%",
      lineWidth = 2,
      medianColor = "red",   # Sets the median line color to red
      medianWidth = 4        # Increases the thickness of the median line
    )) %>%
    hc_tooltip(pointFormat =
                 "<b>Low:</b> {point.low}<br><b>Q1:</b> {point.q1}<br><b>Median:</b> {point.median}<br><b>Q3:</b> {point.q3}<br><b>High:</b> {point.high}"
    )
}



#' single_violinplot
#'
#' @description A function to create a Violin plot of noise levels for a specific frequency band, grouped by day and night.
#'
#' @return A highchart object.
#' @noRd
single_violinplot <- function(data, selected_band) {
  # Filter the data for the selected octave band
  data_filtered <- data %>% filter(octaveBand == selected_band)

  # If there's no data for the selected band, return a placeholder chart
  if(nrow(data_filtered) == 0) {
    return(
      highchart() %>%
        hc_title(text = "No data available for the selected filters.")
    )
  }

  # Define categories and their positions
  categories <- c("Day", "Night")
  cat_positions <- setNames(0:(length(categories) - 1), categories)

  # Start building the highchart
  hc <- highchart() %>%
    hc_chart(type = "area") %>%
    hc_title(text = "Violin Plot: NoiseMean by Period") %>%
    hc_xAxis(categories = categories, title = list(text = "Period")) %>%
    hc_yAxis(title = list(text = "NoiseMean (SPL RMS)"))

  max_violin_width <- 0.3

  # Loop over each category (Day and Night)
  for(cat in categories) {
    group_data <- data_filtered[data_filtered$Day_Night == cat, ]
    if(nrow(group_data) < 2) next  # Skip if insufficient data

    # Calculate density estimates
    dens <- density(group_data$noiseMean, na.rm = TRUE)
    scale_factor <- max_violin_width / max(dens$y)
    offsets <- dens$y * scale_factor
    x_base <- cat_positions[cat]
    left_side <- data.frame(x = x_base - offsets, y = dens$x)
    right_side <- data.frame(x = x_base + offsets, y = dens$x)
    polygon_points <- rbind(left_side, right_side[order(-right_side$y), ])

    # Add the violin area (density) series
    hc <- hc %>% hc_add_series(
      data = highcharter::list_parse2(polygon_points),
      type = "area",
      name = paste("Violin", cat),
      color = "#7cb5ec",
      fillOpacity = 0.5,
      lineWidth = 2,
      showInLegend = FALSE
    )

    # Calculate summary statistics for markers
    med_val <- round(median(group_data$noiseMean, na.rm = TRUE), 2)
    q1 <- round(quantile(group_data$noiseMean, 0.25, na.rm = TRUE), 2)
    q3 <- round(quantile(group_data$noiseMean, 0.75, na.rm = TRUE), 2)

    # Add a marker for the median
    median_point <- data.frame(x = x_base, y = med_val)
    hc <- hc %>% hc_add_series(
      data = highcharter::list_parse2(median_point),
      type = "scatter",
      name = paste("Median", cat),
      marker = list(symbol = "diamond", radius = 6, fillColor = "red"),
      color = "red",
      showInLegend = FALSE
    )

    # Add markers for the quartiles
    quartile_points <- data.frame(
      x = rep(x_base, 2),
      y = c(q1, q3),
      name = c("25% Quartile", "75% Quartile")
    )
    hc <- hc %>% hc_add_series(
      data = highcharter::list_parse2(quartile_points),
      type = "scatter",
      name = paste("Quartiles", cat),
      marker = list(symbol = "circle", radius = 5, fillColor = "gray"),
      color = "gray",
      tooltip = list(pointFormat = "{point.name}: {point.y:.2f}"),
      showInLegend = FALSE
    )
  }

  return(hc)
}



#' single_densityplot
#'
#' @description A function to create a Density plots of noise levels for a specific frequency band, grouped by day and night.
#'
#' @return A highchart object.
#' @noRd
single_densityplot <- function(data, selected_band) {
  # Filter the data for the selected octave band
  data_filtered <- data %>% filter(octaveBand == selected_band)

  # If no data is available for the selected filters, return a placeholder chart
  if(nrow(data_filtered) == 0) {
    return(
      highchart() %>%
        hc_title(text = "No data available for the selected filters.")
    )
  }

  # Calculate density estimates for Day and Night groups
  dens_day <- density(data_filtered %>% filter(Day_Night == "Day") %>% pull(noiseMean), na.rm = TRUE)
  dens_night <- density(data_filtered %>% filter(Day_Night == "Night") %>% pull(noiseMean), na.rm = TRUE)

  # Convert density estimates to series data
  day_series <- purrr::map2(dens_day$x, dens_day$y, function(x, y) list(x, y))
  night_series <- purrr::map2(dens_night$x, dens_night$y, function(x, y) list(x, y))

  # Build and return the highchart object
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = "Density Plot: Distribution of Noise Levels") %>%
    hc_add_series(name = "Day", data = day_series, type = "line") %>%
    hc_add_series(name = "Night", data = night_series, type = "line") %>%
    hc_xAxis(title = list(text = "NoiseMean (SPL RMS)")) %>%
    hc_yAxis(title = list(text = "Density"))
}




