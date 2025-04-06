#' DayNightBoxPlot
#'
#' @description A function to create a highchart boxplot of noise levels by third octave bands grouped by Day & Night.
#' Optionally, if a control dataset is provided (without Day_Night), additional boxes for each frequency band are added.
#' (Used in Tab2, Results section)
#'
#' @return A highchart object.
#' @noRd
DayNightBoxPlot <- function(data, signif_vector, CentralFreq, highlight_bands = c("6", "9"),
                            control_dataset = NULL) {
  library(dplyr)
  library(highcharter)

  # Helper: convert each row of a data.frame to an unnamed list.
  list_parse_no_names <- function(df) {
    lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, ])))
  }

  # Helper: remove names from a vector.
  unname_box_stats <- function(x) unname(x)

  ###############################################################
  # Prepare main data and calculate boxplot statistics
  ###############################################################
  unique_bands <- sort(unique(data$octaveBand))

  plot_data <- data %>%
    mutate(octaveBandFactor = factor(octaveBand,
                                     levels = unique_bands,
                                     labels = as.character(unique_bands)))

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

  max_peak <- plot_data %>%
    group_by(octaveBandFactor, Day_Night) %>%
    summarise(max_noisePeak = max(noisePeak, na.rm = TRUE)) %>%
    ungroup()

  overallMaxVal <- max(bp_stats$ymax, max_peak$max_noisePeak, na.rm = TRUE)

  bp_day <- bp_stats %>%
    filter(Day_Night == "Day") %>%
    arrange(as.numeric(as.character(octaveBandFactor)))
  day_boxplot_data <- bp_day %>%
    rowwise() %>%
    mutate(vals = list(unname_box_stats(c(ymin, lower, middle, upper, ymax)))) %>%
    pull(vals)

  bp_night <- bp_stats %>%
    filter(Day_Night == "Night") %>%
    arrange(as.numeric(as.character(octaveBandFactor)))
  night_boxplot_data <- bp_night %>%
    rowwise() %>%
    mutate(vals = list(unname_box_stats(c(ymin, lower, middle, upper, ymax)))) %>%
    pull(vals)

  max_day <- max_peak %>%
    filter(Day_Night == "Day") %>%
    arrange(as.numeric(as.character(octaveBandFactor))) %>%
    mutate(x = as.numeric(as.character(octaveBandFactor)) - 1) %>%
    select(x, y = max_noisePeak)
  max_day_list <- list_parse_no_names(max_day)

  max_night <- max_peak %>%
    filter(Day_Night == "Night") %>%
    arrange(as.numeric(as.character(octaveBandFactor))) %>%
    mutate(x = as.numeric(as.character(octaveBandFactor)) - 1) %>%
    select(x, y = max_noisePeak)
  max_night_list <- list_parse_no_names(max_night)

  ###############################################################
  # Prepare control dataset (if provided)
  ###############################################################
  if (!is.null(control_dataset)) {
    control_stats <- control_dataset %>%
      group_by(octaveBand) %>%
      summarise(
        ymin   = min(noiseMean, na.rm = TRUE),
        lower  = quantile(noiseMean, 0.25, na.rm = TRUE),
        middle = median(noiseMean, na.rm = TRUE),
        upper  = quantile(noiseMean, 0.75, na.rm = TRUE),
        ymax   = max(noiseMean, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      arrange(octaveBand)
    overallMaxVal <- max(overallMaxVal, max(control_stats$ymax, na.rm = TRUE))
    control_boxplot_data <- control_stats %>%
      rowwise() %>%
      mutate(vals = list(unname_box_stats(c(ymin, lower, middle, upper, ymax)))) %>%
      pull(vals)
  }

  ###############################################################
  # Create plotBands for highlighted octave bands (0-indexed)
  ###############################################################
  highlight_indices <- as.numeric(highlight_bands) - 1
  plotBands <- lapply(highlight_indices, function(i) {
    list(from = i - 0.5, to = i + 0.5, color = "rgba(16,16,16,0.2)")
  })

  ###############################################################
  # Define colors for Day and Night (red and blue)
  ###############################################################
  day_color <- adjustcolor("#E31A1C", alpha.f = 0.5)
  night_color <- adjustcolor("#1F78B4", alpha.f = 0.5)

  ###############################################################
  # Build the highchart
  ###############################################################
  hc <- highchart() %>%
    hc_chart(type = "boxplot", zoomType = "xy") %>%
    hc_title(text = "Noise Levels by Third Octave Bands grouped by Day & Night") %>%
    hc_xAxis(
      categories = CentralFreq,
      title = list(text = "Frequency (Hz)", style = list(fontWeight = "bold")),
      labels = list(style = list(fontWeight = "bold")),
      plotBands = plotBands
    ) %>%
    hc_yAxis(
      title = list(text = "1/3 Octave SPL RMS (dB re 1 µPa)", style = list(fontWeight = "bold")),
      labels = list(style = list(fontWeight = "bold")),
      max = overallMaxVal + 20
    ) %>%
    hc_add_series(
      name = "Day",
      data = day_boxplot_data,
      type = "boxplot",
      color = "#E31A1C",
      fillColor = day_color
    ) %>%
    hc_add_series(
      name = "Night",
      data = night_boxplot_data,
      type = "boxplot",
      color = "#1F78B4",
      fillColor = night_color
    ) %>%
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

  # Add control series if provided.
  if (!is.null(control_dataset)) {
    hc <- hc %>% hc_add_series(
      name = "Control Site (Day)",
      data = control_boxplot_data,
      type = "boxplot",
      color = "gray",
      fillColor = adjustcolor("gray", alpha.f = 0.5)
    )
  }

  ###############################################################
  # Global tooltip formatter using signif_vector from input
  ###############################################################
  sig_js <- paste0('["', paste(signif_vector, collapse = '","'), '"]')
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
                      '<b>Significant between Day & Night: ' + sigText + '</b>';
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
#' (Used in Tab1, Data Exploration section)
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
                 "<b>Low:</b> {point.low}<br><b>Q1:</b> {point.q1}<br><b>Median:</b> {point.med}<br><b>Q3:</b> {point.q3}<br><b>High:</b> {point.high}"
    )
}



#' single_violinplot
#'
#' @description A function to create a Violin plot of noise levels for a specific frequency band, grouped by day and night.
#' (Used in Tab1, Data Exploration section)
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
    offsets <- round(dens$y * scale_factor, 2)
    x_base <- cat_positions[cat]
    left_side <- data.frame(x = x_base - offsets, y = dens$x)
    right_side <- data.frame(x = x_base + offsets, y = dens$x)
    polygon_points <- rbind(left_side, right_side[order(-right_side$y), ])

    # Add the violin area (density) series
    hc <- hc %>% hc_add_series(
      data = highcharter::list_parse2(round(polygon_points, 2)),
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
#' (Used in Tab1, Data Exploration section)
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



#' HourlyEventsByDayPlot
#'
#' @description Creates a highchart bar plot of high noise events (noiseMean above the 90th percentile)
#' by hour and day. Each day/hour “box” is annotated with the top three frequency bands (with event counts and L10 sound levels).
#'
#' Additionally, a table is created that, for each hour (aggregated over all days), displays the top three bands
#' (with their corresponding L10 metrics) in separate columns.
#'
#' @return A list with two elements:
#'   - hc: A highchart object.
#'   - l10_table: An HTML object containing the top bands table.
#' @noRd
HourlyEventsByDayPlot <- function(data) {
  library(dplyr)
  library(lubridate)
  library(highcharter)
  library(tidyr)

  # Define threshold for high noise events (90th percentile)
  threshold <- quantile(data$noiseMean, 0.90, na.rm = TRUE)

  # Define frequency range labels for the 28 octave bands.
  frequency_ranges <- c("20 Hz", "25 Hz", "31.5 Hz", "40 Hz", "50 Hz", "63 Hz", "80 Hz", "100 Hz",
                        "125 Hz", "160 Hz", "200 Hz", "250 Hz", "315 Hz", "400 Hz", "500 Hz", "630 Hz",
                        "800 Hz", "1 kHz", "1.25 kHz", "1.6 kHz", "2 kHz", "2.5 kHz", "3.15 kHz",
                        "4 kHz", "5 kHz", "6.3 kHz", "8 kHz", "10 kHz")

  # Filter for high events and extract day, hour (in CLST), and map octaveBand to a frequency label.
  high_events <- data %>%
    filter(noiseMean > threshold) %>%
    mutate(
      day = as.Date(date_Local, tz = "America/Santiago"),
      hour = hour(with_tz(date_Local, tzone = "America/Santiago")),
      frequency = frequency_ranges[octaveBand]
    )

  # --- For the Highchart Plot ---
  # Aggregate counts by day, hour, and octaveBand; compute L10 for each group.
  band_agg <- high_events %>%
    group_by(day, hour, octaveBand, frequency) %>%
    summarise(
      event_count = n(),
      L10 = quantile(noiseMean, 0.90, na.rm = TRUE),
      .groups = "drop"
    )

  # Compute overall event count by day and hour (summing over all bands).
  daily_hourly_total <- high_events %>%
    group_by(day, hour) %>%
    summarise(
      total_count = n(),
      .groups = "drop"
    )

  # For each day and hour, identify the top 3 frequency bands and format a descriptive string.
  top_bands_info <- band_agg %>%
    group_by(day, hour) %>%
    arrange(desc(event_count)) %>%
    slice_head(n = 3) %>%
    summarise(
      top_bands = paste0(frequency, " (", event_count, " events, L10: ", round(L10, 1), " dB)", collapse = ", "),
      .groups = "drop"
    )

  # Merge the top bands info with the daily/hourly totals.
  daily_hourly <- left_join(daily_hourly_total, top_bands_info, by = c("day", "hour"))

  # Create one series per day as a list of (x, y) points with additional info for tooltips.
  series_list <- lapply(unique(daily_hourly$day), function(d) {
    day_data <- daily_hourly %>% filter(day == d) %>% arrange(hour)
    data_points <- lapply(seq_len(nrow(day_data)), function(i) {
      list(
        x = day_data$hour[i],
        y = day_data$total_count[i],
        topBands = day_data$top_bands[i]
      )
    })
    list(name = as.character(d), data = data_points, zIndex = 2)
  })

  # Compute overall hourly total counts (summing over days).
  overall_hourly <- daily_hourly %>%
    group_by(hour) %>%
    summarise(total_count = sum(total_count), .groups = "drop") %>%
    arrange(hour)

  # Build the highchart (without the moving average series).
  hc <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "High noiseMean Events by Hour (Colored by Day)") %>%
    hc_yAxis(title = list(text = "Number of High Events")) %>%
    hc_tooltip(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = "<b>Hour: {point.x}</b><br>Total Events: {point.y}<br><i>Top Bands:</i> {point.topBands}"
    )

  # Add overall total series as wider, semi-transparent columns.
  overall_series <- list(
    name = "Total Count",
    data = overall_hourly$total_count,
    type = "column",
    grouping = FALSE,       # Centers on each category.
    pointWidth = 30,        # Wider columns.
    color = "rgba(0, 0, 0, 0.3)",  # Semi-transparent black.
    zIndex = 1              # Drawn behind the individual day series.
  )
  hc <- hc %>% hc_add_series_list(list(overall_series))

  # Add each day's series on top.
  hc <- hc %>% hc_add_series_list(series_list)

  # --- For the Top Bands Table (Professional Format) ---
  # Aggregate data by hour and octaveBand (across all days).
  hourly_band_agg <- high_events %>%
    group_by(hour, octaveBand, frequency) %>%
    summarise(
      event_count = n(),
      L10 = round(quantile(noiseMean, 0.90, na.rm = TRUE), 1),
      .groups = "drop"
    )

  # For each hour, select the top 3 bands and assign a rank.
  top_bands_hourly <- hourly_band_agg %>%
    group_by(hour) %>%
    arrange(desc(event_count)) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 3) %>%
    select(hour, frequency, L10, rank) %>%
    pivot_wider(names_from = rank, values_from = c(frequency, L10))

  # Rename columns for clarity.
  top_bands_hourly <- top_bands_hourly %>%
    select(hour, frequency_1, L10_1, frequency_2, L10_2, frequency_3, L10_3) %>%
    rename(
      `Hour` = hour,
      `1st Freq. Band` = frequency_1,
      `Top Band 1 L10 (dB)` = L10_1,
      `2nd Freq. Band` = frequency_2,
      `Top Band 2 L10 (dB)` = L10_2,
      `3rd Freq. Band` = frequency_3,
      `Top Band 3 L10 (dB)` = L10_3
    ) %>%
    arrange(`Hour`)

  # Wrap table in a div with horizontal and vertical scrolling and format it using knitr::kable.
  l10_table_html <- HTML(paste0(
    "<div style='max-height:300px; overflow-y:auto; overflow-x:auto;'>",
    knitr::kable(top_bands_hourly, format = 'html', table.attr = 'class=\"table table-striped table-hover\"'),
    "</div>"
  ))

  # Return a list with the highchart object and the L10 metrics table.
  return(list(hc = hc, l10_table = l10_table_html))
}


