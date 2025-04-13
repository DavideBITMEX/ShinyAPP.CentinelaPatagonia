#####################
# Plot 1
#####################

# Boxplot: Diurnal Variation in 1/3-Octave Noise Levels: Day-time, Night-time, and Control Measurements
# + Table
{
### Plot
{
library(ggplot2)
library(dplyr)

# Example external significance vector (one per octave band; adjust as needed)
signif_vector <- c("yes", "yes", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes",
                   "yes", "yes", "yes", "yes", "yes", "yes", "no", "yes", "yes", "yes",
                   "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")

# Which octave bands to highlight (refers to factor labels, as characters)
highlight_bands <- c("6", "9")

# Unique bands from Quemchi data (typically 1..28), and custom frequency labels
unique_bands <- sort(unique(Quemchi2024_NoiseBand_final$octaveBand))
CentralFreq <- c("20", "25", "31.5", "40", "50", "63", "80", "100",
                 "125", "160", "200", "250", "315", "400", "500", "630",
                 "800", "1000", "1250", "1600", "2000", "2500", "3150",
                 "4000", "5000", "6300", "8000", "10000")

# Create a temporary copy for plotting from Quemchi, adding a factor version of octaveBand
plot_data <- Quemchi2024_NoiseBand_final %>%
  mutate(octaveBandFactor = factor(octaveBand,
                                   levels = unique_bands,
                                   labels = as.character(unique_bands)))

# Create a temporary copy for the Control dataset with the same factor conversion
control_plot_data <- Control_final %>%
  mutate(octaveBandFactor = factor(octaveBand,
                                   levels = unique_bands,
                                   labels = as.character(unique_bands)))

# Data frame for highlighted background rectangles
highlight_df <- data.frame(octaveBand = highlight_bands)
highlight_df$pos <- as.numeric(highlight_df$octaveBand)

# Calculate boxplot statistics for each (octaveBandFactor, Day_Night) group (Quemchi data)
bp_stats <- plot_data %>%
  group_by(octaveBandFactor, Day_Night) %>%
  summarise(
    ymin   = min(noiseMean, na.rm = TRUE),
    lower  = quantile(noiseMean, 0.25, na.rm = TRUE),
    middle = median(noiseMean, na.rm = TRUE),
    upper  = quantile(noiseMean, 0.75, na.rm = TRUE),
    ymax   = max(noiseMean, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate boxplot statistics for the control dataset
bp_stats_control <- control_plot_data %>%
  group_by(octaveBandFactor) %>%
  summarise(
    ymin   = min(noiseMean, na.rm = TRUE),
    lower  = quantile(noiseMean, 0.25, na.rm = TRUE),
    middle = median(noiseMean, na.rm = TRUE),
    upper  = quantile(noiseMean, 0.75, na.rm = TRUE),
    ymax   = max(noiseMean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Day_Night = "Control")

# Combine Day/Night and Control stats
bp_stats_combined <- bind_rows(bp_stats, bp_stats_control)

# Create a significance data frame with the “*” label if significance == "yes"
sig_df <- data.frame(octaveBandFactor = factor(as.character(unique_bands),
                                               levels = as.character(unique_bands)),
                     significance = signif_vector) %>%
  mutate(label = ifelse(significance == "yes", "*", ""))

# Compute maximum noisePeak for each (octaveBandFactor, Day_Night) group (Quemchi data)
max_peak <- plot_data %>%
  group_by(octaveBandFactor, Day_Night) %>%
  summarise(max_noisePeak = max(noisePeak, na.rm = TRUE), .groups = "drop")

# Adjust colors to be a bit transparent
day_color     <- adjustcolor("#E31A1C", alpha.f = 0.5)  # red for Day
night_color   <- adjustcolor("#1F78B4", alpha.f = 0.5)  # blue for Night
control_color <- "gray70"                               # for Control

# Build the ggplot
p <- ggplot() +
  # Grey background rectangles for highlighted frequency bands
  geom_rect(data = highlight_df,
            aes(xmin = pos - 0.5, xmax = pos + 0.5, ymin = -Inf, ymax = Inf),
            fill = "grey10", alpha = 0.2, inherit.aes = FALSE) +

  # Boxplots (Day, Night, Control)
  geom_boxplot(data = bp_stats_combined,
               aes(x = octaveBandFactor, ymin = ymin, lower = lower, middle = middle,
                   upper = upper, ymax = ymax, fill = Day_Night),
               stat = "identity",
               position = position_dodge(width = 0.8),
               width = 0.7,
               color = "black",
               alpha = 0.8) +

  # Significance asterisks placed at a fixed y=65 (adjust if needed)
  geom_text(data = sig_df,
            aes(x = octaveBandFactor, y = 70, label = label),
            inherit.aes = FALSE,
            size = 6, vjust = 0) +

  # Points showing max noisePeak, mapping both color and shape to Day_Night
  geom_point(data = max_peak,
             aes(x = octaveBandFactor, y = max_noisePeak, color = Day_Night, shape = Day_Night),
             fill = NA,      # We'll rely on color for the fill to match the legend
             size = 2,
             stroke = 1,
             alpha = 0.8,
             position = position_dodge(width = 0.8)) +

  # Change x-axis labels to the provided frequency values
  scale_x_discrete(labels = CentralFreq) +

  # Fill scale for the boxplots
  scale_fill_manual(
    name = "Boxplots:",  # legend title
    values = c("Day" = day_color,
               "Night" = night_color,
               "Control" = control_color),
    labels = c("Day" = "Day",
               "Night" = "Night",
               "Control" = "Control"),
    guide = guide_legend(order = 1)
  ) +

  # Color scale for the max noisePeak diamonds
  scale_color_manual(
    name = "Max noisePeak:",  # legend title
    values = c("Day" = day_color,
               "Night" = night_color),
    labels = c("Day" = "Day",
               "Night" = "Night"),
    guide = guide_legend(order = 2)
  ) +

  # Shape scale for the max noisePeak diamonds (both Day and Night are shape 23)
  scale_shape_manual(
    name = "Max noisePeak:",
    values = c("Day" = 23,
               "Night" = 23),
    labels = c("Day" = "Day",
               "Night" = "Night"),
    guide = guide_legend(order = 2)
  ) +

  labs(title = "Diurnal Variation in 1/3-Octave Noise Levels: Day-time, Night-time, and Control Measurements",
       x = "Centered frequency (Hz)",
       y = "1/3 Octave SPL RMS (dB re 1 µPa)") +

  theme_bw(base_size = 13) +
  theme(
    # Keep horizontal grid lines, remove vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_line(color = "gray90"),

    plot.title   = element_text(face = "bold", size = 17, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x  = element_text(face = "bold", color = "black", angle = 45, hjust = 1),
    axis.text.y  = element_text(face = "bold", color = "black"),

    # Position the legends at the top;
    # you can also try legend.box = "horizontal" or legend.box = "vertical" for different layouts
    legend.position = "top",
    legend.background = element_rect(fill = adjustcolor("white", alpha.f = 0.5), color = NA),
    legend.text  = element_text(size = 12)
  )

# Display the plot
p
}

### Table
  {
    # Load necessary package
    library(dplyr)

    # --- Summarize Quemchi2024_NoiseBand_final ---
    quemchi_summary <- Quemchi2024_NoiseBand_final %>%
      group_by(octaveBand, Day_Night) %>%
      summarise(
        median_noiseMean = median(noiseMean, na.rm = TRUE),
        average_noiseMean = mean(noiseMean, na.rm = TRUE),  # This is the RMS value
        q1 = quantile(noiseMean, 0.25, na.rm = TRUE),
        q3 = quantile(noiseMean, 0.75, na.rm = TRUE),
        max_noisePeak = max(noisePeak, na.rm = TRUE)
      ) %>%
      ungroup()

    # --- Summarize Control_final ---
    control_summary <- Control_final %>%
      mutate(Day_Night = "Control") %>%
      group_by(octaveBand, Day_Night) %>%
      summarise(
        median_noiseMean = median(noiseMean, na.rm = TRUE),
        average_noiseMean = mean(noiseMean, na.rm = TRUE),  # This is the RMS value
        q1 = quantile(noiseMean, 0.25, na.rm = TRUE),
        q3 = quantile(noiseMean, 0.75, na.rm = TRUE),
        max_noisePeak = max(noisePeak, na.rm = TRUE)
      ) %>%
      ungroup()

    # --- Combine both summaries ---
    combined_summary <- bind_rows(quemchi_summary, control_summary) %>%
      arrange(octaveBand)

    # --- Define frequency labels for the 28 octave bands ---
    frequency_labels <- c("20", "25", "31.5", "40", "50", "63", "80", "100",
                          "125", "160", "200", "250", "315", "400", "500", "630",
                          "800", "1000", "1250", "1600", "2000", "2500", "3150",
                          "4000", "5000", "6300", "8000", "10000")

    # --- Add the frequency label ---
    combined_summary <- combined_summary %>%
      mutate(Frequency_Hz = frequency_labels[octaveBand])

    # --- Create filtered table for selected frequency bands ---
    selected_summary <- combined_summary %>%
      filter(Frequency_Hz %in% c("63", "125"))

    # --- Rename columns for the full table ---
    combined_summary <- combined_summary %>%
      select(octaveBand, Frequency_Hz, Day_Night, average_noiseMean, median_noiseMean, q1, q3, max_noisePeak) %>%
      rename(
        'Centered Freq (Hz)'     = Frequency_Hz,
        'Day/Night/Control'      = Day_Night,
        'RMS Noise Levels'       = average_noiseMean,   # Renamed from average_noiseMean
        'Median RMS Noise Levels'= median_noiseMean,
        '25th percentile'        = q1,
        '75th percentile'        = q3,
        'Max NoisePeak'          = max_noisePeak
      ) %>%
      select(-octaveBand)

    # --- Rename columns for the filtered table ---
    selected_summary <- selected_summary %>%
      select(octaveBand, Frequency_Hz, Day_Night, average_noiseMean, median_noiseMean, q1, q3, max_noisePeak) %>%
      rename(
        'Centered Freq (Hz)'     = Frequency_Hz,
        'Day/Night/Control'      = Day_Night,
        'RMS Noise Levels'       = average_noiseMean,   # Renamed from average_noiseMean
        'Median RMS Noise Levels'= median_noiseMean,
        '25th percentile'        = q1,
        '75th percentile'        = q3,
        'Max NoisePeak'          = max_noisePeak
      ) %>%
      select(-octaveBand)

  }
  # --- View the final tables ---
  print(combined_summary)
  print(selected_summary) # just 63 and 125 Hz

}


#####################
# Plot 2 Loud events
#####################
{
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(tidyr)

  # Static version: High Noise Events by Hour and Day Plot
  HourlyEventsByDayPlot_static <- function(data) {

    # Define threshold for high noise events (90th percentile)
    threshold <- quantile(data$noiseMean, 0.90, na.rm = TRUE)

    # Define frequency range labels for the 28 octave bands.
    frequency_ranges <- c("20 Hz", "25 Hz", "31.5 Hz", "40 Hz", "50 Hz", "63 Hz", "80 Hz", "100 Hz",
                          "125 Hz", "160 Hz", "200 Hz", "250 Hz", "315 Hz", "400 Hz", "500 Hz", "630 Hz",
                          "800 Hz", "1 kHz", "1.25 kHz", "1.6 kHz", "2 kHz", "2.5 kHz", "3.15 kHz",
                          "4 kHz", "5 kHz", "6.3 kHz", "8 kHz", "10 kHz")

    # Filter data for high events and compute day, hour (using America/Santiago timezone), and map octaveBand to a frequency label.
    high_events <- data %>%
      filter(noiseMean > threshold) %>%
      mutate(
        day = as.Date(date_Local, tz = "America/Santiago"),
        hour = hour(with_tz(date_Local, tzone = "America/Santiago")),
        frequency = frequency_ranges[octaveBand]
      )

    # --- Prepare Summary Data for the Plot ---

    # Overall event count by day and hour (summing over all bands)
    daily_hourly_total <- high_events %>%
      group_by(day, hour) %>%
      summarise(total_count = n(), .groups = "drop")

    # For each day and hour, get the top 3 frequency bands and format a descriptive string
    top_bands_info <- high_events %>%
      group_by(day, hour, octaveBand, frequency) %>%
      summarise(event_count = n(),
                L10 = quantile(noiseMean, 0.90, na.rm = TRUE),
                .groups = "drop") %>%
      group_by(day, hour) %>%
      arrange(desc(event_count)) %>%
      slice_head(n = 3) %>%
      summarise(top_bands = paste0(frequency, " (", event_count, " events, L10: ", round(L10, 1), " dB)", collapse = ", "),
                .groups = "drop")

    # Merge the daily/hourly totals with the top bands information.
    daily_hourly <- left_join(daily_hourly_total, top_bands_info, by = c("day", "hour"))

    # Compute overall totals by hour (aggregated over days)
    overall_hourly <- daily_hourly %>%
      group_by(hour) %>%
      summarise(total_count = sum(total_count), .groups = "drop") %>%
      arrange(hour)

    # --- Build the Static Plot ---

    # Here, we create a grouped bar chart:
    #  - The overall count by hour is shown as background grey bars.
    #  - Each day’s count (for the same hour) is overlaid as grouped bars,
    #    with the day used as the fill.
    #  - Each day/hour bar is annotated with the top frequency bands string.
    p <- ggplot() +
      # Overall hourly count (background)
      geom_col(data = overall_hourly,
               aes(x = factor(hour), y = total_count),
               fill = "grey80", alpha = 0.5, width = 0.9) +

      # Day-specific counts (grouped by hour)
      geom_col(data = daily_hourly,
               aes(x = factor(hour), y = total_count, fill = factor(day)),
               position = position_dodge(width = 0.9), width = 0.8) +

      # Annotate each day/hour bar with top bands info (text rotated for clarity)
      # geom_text(data = daily_hourly,
      #           aes(x = factor(hour), y = total_count + 1, label = top_bands),
      #           position = position_dodge(width = 0.9),
      #           size = 3, angle = 45, hjust = 0) +

      labs(title = "Loud Events by Hour and Day",
           subtitle = "Loud events = n. of minutes where RMS Noise Levels > 90th percentile",
           x = "Hour of Day",
           y = "Number of Loud Events",
           fill = "Day") +

      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

    # --- Create a Top Bands Table (Aggregated over Days) ---

    # Aggregate data by hour and frequency (across all days)
    hourly_band_agg <- high_events %>%
      group_by(hour, octaveBand, frequency) %>%
      summarise(event_count = n(),
                L10 = round(quantile(noiseMean, 0.90, na.rm = TRUE), 1),
                .groups = "drop")

    # For each hour, select the top 3 frequency bands and arrange them into columns.
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
        Hour = hour,
        `1st Freq. Band` = frequency_1,
        `Top Band 1 L10 (dB)` = L10_1,
        `2nd Freq. Band` = frequency_2,
        `Top Band 2 L10 (dB)` = L10_2,
        `3rd Freq. Band` = frequency_3,
        `Top Band 3 L10 (dB)` = L10_3
      ) %>%
      arrange(Hour)

    # Return the plot and the table (you can render the table separately if needed).
    return(list(plot = p, top_bands_table = top_bands_hourly))
  }

  # --- Example Usage ---
  # Assuming your data is in a data frame called 'data':
  result <- HourlyEventsByDayPlot_static(data = Quemchi2024_NoiseBand_final)
  # 'result$plot' contains the ggplot object for the bar chart.
  # 'result$top_bands_table' contains the summary table for top bands.

  # To display the plot:
  print(result$plot)

  # To view the table in the console:
  print(result$top_bands_table)

}
