#################################################
### STATIC PLOTS (suitable for pubblications) ###
#################################################

### Boxplot: All frequency bands
# Day - Night
{

library(ggplot2)
library(dplyr)


# Example external significance vector (one per octave band; adjust as needed)
signif_vector <- c("yes", "yes", "yes", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes",
                   "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")

# Allow the user to specify which two octave bands to highlight (using the underlying factor levels)
highlight_bands <- c("6", "9")  # as characters, matching the factor labels

# Define unique bands and custom frequency labels
unique_bands <- sort(unique(Quemchi2024_NoiseBand_final$octaveBand))  # numeric vector
CentralFreq <- c("20", "25", "31.5", "40", "50", "63", "80", "100",
                 "125", "160", "200", "250", "315", "400", "500", "630",
                 "800", "1000", "1250", "1600", "2000", "2500", "3150",
                 "4000", "5000", "6300", "8000", "10000")

# Create a temporary copy for plotting only, adding a factor version of octaveBand
plot_data <- Quemchi2024_NoiseBand_final %>%
  mutate(octaveBandFactor = factor(octaveBand, levels = unique_bands,
                                   labels = as.character(unique_bands)))

# Data frame for highlighted background rectangles.
highlight_df <- data.frame(octaveBand = highlight_bands)
highlight_df$pos <- as.numeric(highlight_df$octaveBand)

# Calculate boxplot statistics for each (octaveBandFactor, Day_Night) group using noiseMean
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

# Compute the top of the box (maximum upper quartile) for each octave band (for asterisk placement)
box_top_df <- bp_stats %>%
  group_by(octaveBandFactor) %>%
  summarise(box_top = max(upper, na.rm = TRUE))

# Create a significance data frame using the external vector (per octave band)
sig_df <- data.frame(octaveBandFactor = factor(as.character(unique_bands),
                                               levels = as.character(unique_bands)),
                     significance = signif_vector)
sig_data <- left_join(sig_df, box_top_df, by = "octaveBandFactor") %>%
  mutate(label = ifelse(significance == "yes", "*", ""))

# Compute maximum noisePeak for each (octaveBandFactor, Day_Night)
max_peak <- plot_data %>%
  group_by(octaveBandFactor, Day_Night) %>%
  summarise(max_noisePeak = max(noisePeak, na.rm = TRUE)) %>%
  ungroup()

# Adjust colors to be a bit transparent
day_color <- adjustcolor("#E31A1C", alpha.f = 0.5)    # red for Day
night_color <- adjustcolor("#1F78B4", alpha.f = 0.5)  # blue for Night

# Build the plot
p <- ggplot() +
  # Grey background rectangles for highlighted noise bands
  geom_rect(data = highlight_df,
            aes(xmin = pos - 0.5, xmax = pos + 0.5, ymin = -Inf, ymax = Inf),
            fill = "grey10", alpha = 0.2, inherit.aes = FALSE) +
  # Boxplots based on noiseMean statistics
  geom_boxplot(data = bp_stats,
               aes(x = octaveBandFactor, ymin = ymin, lower = lower, middle = middle,
                   upper = upper, ymax = ymax, fill = Day_Night),
               stat = "identity",
               position = position_dodge(width = 0.8),
               width = 0.7,
               color = "black",
               alpha = 0.8) +
  # Significance asterisks (placed above the boxes)
  geom_text(data = sig_data,
            aes(x = octaveBandFactor, y = box_top + 3, label = label),
            inherit.aes = FALSE, size = 6, vjust = 0) +
  # Points showing the maximum noisePeak for each (octaveBandFactor, Day_Night)
  geom_point(data = max_peak,
             aes(x = octaveBandFactor, y = max_noisePeak, color = Day_Night),
             position = position_dodge(width = 0.8),
             size = 2, shape = 21, alpha = 0.8) +
  scale_x_discrete(labels = CentralFreq) +
  # Switch colors: Day in red, Night in blue, with transparency
  scale_fill_manual(name = "", values = c("Day" = day_color, "Night" = night_color)) +
  scale_color_manual(name = "", values = c("Day" = day_color, "Night" = night_color)) +
  labs(title = "Noise Levels by Octave Band and Time of Day",
       x = "Frequency (Hz)",
       y = "1/3 Octave SPL RMS (dB re 1 µPa)") +
  theme_bw(base_size = 13) +
  theme(
    plot.title  = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    legend.position = "top",  # legend placed at the top (under the title)
    legend.background = element_rect(fill = alpha("white", 0.5), color = "NA"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p
}

### Boxplot: All frequency bands
# Day - Night - Control
{

library(ggplot2)
library(dplyr)

# Example external significance vector (one per octave band; adjust as needed)
signif_vector <- c("yes", "yes", "yes", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes",
                   "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")

# Allow the user to specify which two octave bands to highlight (using the underlying factor levels)
highlight_bands <- c("3", "6")  # as characters, matching the factor labels

# Define unique bands and custom frequency labels
unique_bands <- sort(unique(Quemchi2024_NoiseBand_final$octaveBand))  # numeric vector
CentralFreq <- c("20", "25", "31.5", "40", "50", "63", "80", "100",
                 "125", "160", "200", "250", "315", "400", "500", "630",
                 "800", "1000", "1250", "1600", "2000", "2500", "3150",
                 "4000", "5000", "6300", "8000", "10000")

# Create a temporary copy for plotting only, adding a factor version of octaveBand
plot_data <- Quemchi2024_NoiseBand_final %>%
  mutate(octaveBandFactor = factor(octaveBand, levels = unique_bands,
                                   labels = as.character(unique_bands)))

# Data frame for highlighted background rectangles.
highlight_df <- data.frame(octaveBand = highlight_bands)
highlight_df$pos <- as.numeric(highlight_df$octaveBand)

# Calculate boxplot statistics for each (octaveBandFactor, Day_Night) group using noiseMean (for Day and Night)
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

# Create fake "Control" data for each octave band (using overall noiseMean stats)
bp_stats_control <- plot_data %>%
  group_by(octaveBandFactor) %>%
  summarise(
    ymin   = min(noiseMean, na.rm = TRUE),
    lower  = quantile(noiseMean, 0.25, na.rm = TRUE),
    middle = median(noiseMean, na.rm = TRUE),
    upper  = quantile(noiseMean, 0.75, na.rm = TRUE),
    ymax   = max(noiseMean, na.rm = TRUE)
  ) %>%
  mutate(Day_Night = "Control")

# Combine Day/Night stats with Control stats
bp_stats_combined <- bind_rows(bp_stats, bp_stats_control)

# Compute the top of the box (maximum upper quartile) for each octave band (for asterisk placement)
box_top_df <- bp_stats %>%
  group_by(octaveBandFactor) %>%
  summarise(box_top = max(upper, na.rm = TRUE))

# Create a significance data frame using the external vector (per octave band)
sig_df <- data.frame(octaveBandFactor = factor(as.character(unique_bands),
                                               levels = as.character(unique_bands)),
                     significance = signif_vector)
sig_data <- left_join(sig_df, box_top_df, by = "octaveBandFactor") %>%
  mutate(label = ifelse(significance == "yes", "*", ""))

# Compute maximum noisePeak for each (octaveBandFactor, Day_Night) for Day and Night groups only
max_peak <- plot_data %>%
  group_by(octaveBandFactor, Day_Night) %>%
  summarise(max_noisePeak = max(noisePeak, na.rm = TRUE)) %>%
  ungroup()

# Adjust colors to be a bit transparent
day_color <- adjustcolor("#E31A1C", alpha.f = 0.5)    # red for Day
night_color <- adjustcolor("#1F78B4", alpha.f = 0.5)  # blue for Night
control_color <- "gray70"  # for Control

# Build the plot
p <- ggplot() +
  # Grey background rectangles for highlighted noise bands
  geom_rect(data = highlight_df,
            aes(xmin = pos - 0.5, xmax = pos + 0.5, ymin = -Inf, ymax = Inf),
            fill = "grey10", alpha = 0.2, inherit.aes = FALSE) +
  # Boxplots based on noiseMean statistics (for Day, Night, and Control)
  geom_boxplot(data = bp_stats_combined,
               aes(x = octaveBandFactor, ymin = ymin, lower = lower, middle = middle,
                   upper = upper, ymax = ymax, fill = Day_Night),
               stat = "identity",
               position = position_dodge(width = 0.8),
               width = 0.7,
               color = "black",
               alpha = 0.8) +
  # Significance asterisks (placed above the boxes; only for Day and Night)
  geom_text(data = sig_data,
            aes(x = octaveBandFactor, y = box_top + 3, label = label),
            inherit.aes = FALSE, size = 6, vjust = 0) +
  # Points showing the maximum noisePeak for each (octaveBandFactor, Day_Night) (Day and Night only)
  geom_point(data = max_peak,
             aes(x = octaveBandFactor, y = max_noisePeak, color = Day_Night),
             position = position_dodge(width = 0.8),
             size = 2, shape = 21, alpha = 0.8) +
  scale_x_discrete(labels = CentralFreq) +
  # Switch colors: Day in red, Night in blue, Control in gray
  scale_fill_manual(name = "", values = c("Day" = day_color,
                                          "Night" = night_color,
                                          "Control" = control_color)) +
  scale_color_manual(name = "", values = c("Day" = day_color,
                                           "Night" = night_color)) +
  labs(title = "Noise Levels by Octave Band and Time of Day",
       x = "Frequency (Hz)",
       y = "1/3 Octave SPL RMS (dB re 1 µPa)") +
  theme_bw(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    legend.position = "top",  # legend placed at the top (under the title)
    legend.background = element_rect(fill = alpha("white", 0.5), color = NA),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p


}


#################################################
### INTERACTIVE PLOTS (suitable for shiny app) ##
#################################################
### Boxplot: All frequency bands
# Day - Night
{

library(dplyr)
library(highcharter)

# Helper to convert each row of a data.frame to an unnamed list
list_parse_no_names <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    unname(as.list(df[i, ]))
  })
}

unname_box_stats <- function(x) unname(x)

# --- Data Processing ---

# Example external significance vector (one per octave band; adjust as needed)
signif_vector <- c("yes", "yes", "yes", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes",
                   "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")

# Allow the user to specify which two octave bands to highlight (using the underlying factor levels)
highlight_bands <- c("6", "9")  # as characters, matching the factor labels

# Define unique bands and custom frequency labels
unique_bands <- sort(unique(Quemchi2024_NoiseBand_final$octaveBand))  # numeric vector
CentralFreq <- c("20", "25", "31.5", "40", "50", "63", "80", "100",
                 "125", "160", "200", "250", "315", "400", "500", "630",
                 "800", "1000", "1250", "1600", "2000", "2500", "3150",
                 "4000", "5000", "6300", "8000", "10000")

# Temporary plotting data: add a factor version of octaveBand
plot_data <- Quemchi2024_NoiseBand_final %>%
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

# Create plotBands for highlighted octave bands (0-indexed)
highlight_indices <- as.numeric(highlight_bands) - 1
plotBands <- lapply(highlight_indices, function(i) {
  list(from = i - 0.5, to = i + 0.5, color = "rgba(16,16,16,0.2)")
})

# Define colors with transparency
day_color <- adjustcolor("#E31A1C", alpha.f = 0.5)   # red for Day
night_color <- adjustcolor("#1F78B4", alpha.f = 0.5)   # blue for Night

# Build the interactive Highchart
highchart() %>%
  hc_chart(type = "boxplot") %>%
  hc_title(text = "Noise Levels by Octave Band and Time of Day") %>%
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
    color = "#1F78B4",      # border color
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
  ) %>%
  # Global tooltip formatter for boxplot series that rounds values and shows significance info
  hc_tooltip(
    useHTML = TRUE,
    formatter = JS("
      function() {
         if (this.series.type === 'boxplot') {
             var cat = this.series.xAxis.categories[this.point.x];
             var sigMapping = ['yes','yes','yes','yes','yes','yes','no','yes','no','yes','yes','yes','yes','yes','yes','yes','yes','no','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes'];
             var sigText = (sigMapping[this.point.x] === 'yes') ? 'Yes' : 'No';
             return '<b>' + '[' + cat + ' Hz] ' + this.series.name + '</b><br/>' +
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
    ")
  ) %>%
  hc_legend(layout = "horizontal", align = "center", verticalAlign = "top")

}
