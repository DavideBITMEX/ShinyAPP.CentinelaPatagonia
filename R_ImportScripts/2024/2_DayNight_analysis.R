######################################################
# Day/Night Analysis
######################################################
library(dplyr)
library(highcharter)
library(lubridate)
library(ggplot2)
library(nlme) # for mixed effect models



######################################################
######### Data Preparation and Exploration ###########
######################################################
# 1. Aggregate Data by minute (already done, 'Quemchi2024_NoiseBand_final')
    # minute aggregation is a good intermediate step, but verify that the aggregation level adequately addresses autocorrelation
    # issues without oversmoothing the data. You may also experiment with additional levels of aggregation or adjust your modeling
    # approach accordingly.
# 2. Define Day and Night (will use the Sunrise/Sunset times in Chiloe in November)
    # Day: from 06:31 until 20:45
    # Night: from 20:46 until 06:30
{
  ### 1. Already done
  head(Quemchi2024_NoiseBand_final)
  ### 2.
  Quemchi2024_NoiseBand_final <- Quemchi2024_NoiseBand_final %>%
    mutate(Day_Night = ifelse(
      format(date_Local, "%H:%M") >= "06:31:00" & format(date_Local, "%H:%M") <= "20:45",
      "Day",
      "Night"
    ))

}

### Save R.Data
save("Nov5", "Nov6", "Nov7", "Nov8", "Nov9",
     "Quemchi2024_NoiseBand",
     "Quemchi2024_NoiseBand_final",
     file = "R/Environment.RData")



######################################################
######### Exploratory Data Analysis (EDA) ############
######################################################
# 1. Summary Statistics: this gives you an initial overview of differences in central tendency and variability.
# 2. Visual Inspection
#   a. Istograms and Densisty plots (to check if the data is normally distributed)
#   b. Boxplots and Violin Plots (to check heteroschedasticity)
#   c. Time series plot
{
  ### 1. Calculate summary statistics for noiseMean by Day_Night (and octaveBand if needed)
        summary_stats <- Quemchi2024_NoiseBand_final %>%
          group_by(Day_Night, octaveBand) %>%
          summarise(
            count = n(),
            mean_noiseMean = mean(noiseMean, na.rm = TRUE),
            median_noiseMean = median(noiseMean, na.rm = TRUE),
            sd_noiseMean = sd(noiseMean, na.rm = TRUE),
            min_noiseMean = min(noiseMean, na.rm = TRUE),
            max_noiseMean = max(noiseMean, na.rm = TRUE),
            .groups = "drop"
          )
        summary_stats
  ### 2a. Istograms and Densisty plots
  # Histogram for noiseMean by Day_Night
        ggplot(Quemchi2024_NoiseBand_final, aes(x = noiseMean, fill = Day_Night)) +
          geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
          facet_wrap(~ octaveBand) +
          labs(title = "Histogram of Noise Mean by Day and Night",
               x = "Noise Mean (SPL RMS)", y = "Count")

        # Density Plot for noiseMean by Day_Night
        ggplot(Quemchi2024_NoiseBand_final, aes(x = noiseMean, color = Day_Night)) +
          geom_density(linewidth = 1) +
          facet_wrap(~ octaveBand) +
          labs(title = "Density Plot of Noise Mean by Day and Night",
               x = "Noise Mean (SPL RMS)", y = "Density")

        # Based on the histograms and density plots across the 28 octave bands, the noiseMean data for both day and night still
        # do not appear to follow a normal distribution. Many of the bands show right-skewed distributions, multiple peaks
        # (suggesting possible multimodality), and some heavy tails and outliers. Overall, this indicates that the noiseMean values
        # remain non-normally distributed across different bands and times of day.
        # ---> NOT NORMALLY DISTRIBUTED

  ### 2b. Boxplots and Violin Plots
  # Boxplot for noiseMean by Day_Night
        ggplot(Quemchi2024_NoiseBand_final, aes(x = Day_Night, y = noiseMean, fill = Day_Night)) +
          geom_boxplot() +
          facet_wrap(~ octaveBand) +
          labs(title = "Boxplot of Noise Mean by Day and Night",
               x = "Period", y = "Noise Mean (SPL RMS)")

        # From the boxplots across the 28 octave bands, we can see that the interquartile ranges (IQRs) for day vs. night differ
        # in multiple bands (e.g., 1, 3, 5), indicating that the spread of the data is not consistent across time periods.
        # In many of these bands, the whiskers (which capture most of the data points) suggest that nighttime measurements often
        # exhibit greater variance than daytime, although this is not uniform across all bands. We also observe outliers in both day
        # and night data, highlighting potential extreme values. Overall, these boxplots confirm that noiseMean distributions vary
        # noticeably between day and night across several octave bands.

        # Violin Plot for noiseMean by Day_Night
        ggplot(Quemchi2024_NoiseBand_final, aes(x = Day_Night, y = noiseMean, fill = Day_Night)) +
          geom_violin(trim = FALSE) +
          facet_wrap(~ octaveBand) +
          labs(title = "Violin Plot of Noise Mean by Day and Night",
               x = "Period", y = "Noise Mean (SPL RMS)")

        # From these violin plots, we can see that the distribution of noiseMean differs noticeably between day and night across
        # many of the 28 octave bands. In several bands the nighttime data appears to have a wider spread, indicating greater variance,
        # whereas in other bands, daytime distributions may be tighter or occasionally show multiple peaks. These patterns highlight
        # that the variance is not equal between day and night in numerous bands, further confirming unequal variances.
        # ---> UNEQUAL VARIANCES

  ### 2c. Time series plot
        # Time Series Plot for noiseMean, highlighting day and night periods
        ggplot(Quemchi2024_NoiseBand_final, aes(x = date_Local, y = noiseMean, color = Day_Night)) +
          geom_line(alpha = 0.7) +
          facet_wrap(~ octaveBand, scales = "free_y") +
          labs(title = "Time Series of Noise Mean",
               x = "Time", y = "Noise Mean (SPL RMS)") +
          theme_minimal()


}




######################################################
############# Statistical Testing ####################
######################################################
# The data was NOT Normally Distributed and Variances were NOT equal
# 1. non-parametric test Wilcoxon rank-sum test (for each Octave Band)
# 2. Mixed-Effects Model
#   Because the data consist of repeated measures (multiple aggregated observations per day) and we might have day-to-day variability,
#   a mixed-effects model can help account for non-independence among observations.
# 3. Model Diagnostic (Mixed-Effect Model)
# 4. ---> applying log-transformation (can help stabilize variance and improve the normality of residuals)

### Stat testing (not used for this campaign)
{
  library(dplyr)

  summarize_stat_tests_table <- function(data,
                                         band_col = "octaveBand",
                                         outcome = "noiseMean",
                                         group = "Day_Night") {

    # Get unique bands (converted to character to avoid issues with factors)
    bands <- sort(unique(as.character(data[[band_col]])))

    # Create an empty data frame to store the results
    results <- data.frame(
      Band = character(),
      p_value = numeric(),
      Significant = numeric(),
      Louder = character(),
      stringsAsFactors = FALSE
    )

    # Loop over each band
    for(b in bands) {
      # Filter the data for the current band
      sub_data <- subset(data, as.character(data[[band_col]]) == b)

      # Perform the Wilcoxon test: compare outcome between groups (e.g., Day vs Night)
      test_formula <- as.formula(paste(outcome, "~", group))
      test_res <- wilcox.test(test_formula, data = sub_data)

      p_val <- test_res$p.value
      sig <- ifelse(p_val < 0.05, 1, 0)

      # Determine which group is louder if significant by comparing medians
      louder <- ""
      if(sig == 1) {
        summ <- sub_data %>%
          group_by_at(group) %>%
          summarise(median_val = median(.data[[outcome]], na.rm = TRUE)) %>%
          ungroup()

        # Assume two groups exist ("Day" and "Night")
        if(nrow(summ) >= 2 && all(c("Day", "Night") %in% summ[[group]])) {
          day_med <- summ$median_val[summ[[group]] == "Day"]
          night_med <- summ$median_val[summ[[group]] == "Night"]
          louder <- if(day_med > night_med) "Day" else "Night"
        }
      }

      # Append the result for this band
      results <- rbind(
        results,
        data.frame(
          Band = b,
          p_value = round(p_val, 4),
          Significant = sig,
          Louder = louder,
          stringsAsFactors = FALSE
        )
      )
    }

    return(results)
  }

  # Example usage:
  wilcoxon_table <- summarize_stat_tests_table(Quemchi2024_NoiseBand_final)
  wilcoxon_table <- wilcoxon_table %>%
    mutate(Band = as.numeric(Band)) %>%
    arrange(Band)


  # You can then print it to the console...
  print(wilcoxon_table)

  stat_sig_vector <- ifelse(wilcoxon_table$Significant == 1, "yes", "no")

  # Inspect the result
  stat_sig_vector



}

### Mixed-effect model (use this for this campaign)
# Measurements are grouped by day (or other repeated measures), meaning observations within the same day may be correlated.
# By including a random effect for “day,” the mixed‐effects model properly handles this within-group correlation, providing
# more accurate standard errors and p-values compared to tests that assume all observations are independent (like the Wilcoxon test).
{
  library(nlme)
  library(dplyr)

  summarize_mixed_models_table <- function(data,
                                           band_col = "octaveBand",
                                           outcome = "noiseMean",
                                           group = "Day_Night") {
    # Ensure we have a date variable for the random effect:
    data <- data %>% mutate(day = as.Date(date_Local))

    # Get sorted unique bands (as characters)
    bands <- sort(unique(as.character(data[[band_col]])))

    # Prepare an empty results data frame
    results <- data.frame(
      Band = character(),
      p_value = numeric(),
      Significant = numeric(),
      Louder = character(),
      p_value_log = numeric(),
      Significant_log = numeric(),
      Louder_log = character(),
      stringsAsFactors = FALSE
    )

    for(b in bands) {
      sub_data <- subset(data, as.character(data[[band_col]]) == b)
      if(nrow(sub_data) < 2) next  # Skip bands with insufficient data

      ### 1. Mixed-Effects Model (Original)
      model <- lme(as.formula(paste(outcome, "~", group)),
                   random = ~ 1 | day,
                   data = sub_data)
      summ_model <- summary(model)
      # For a two-level factor, the second row corresponds to the effect of the non-baseline group.
      if(nrow(summ_model$tTable) < 2) next
      p_val <- summ_model$tTable[2, "p-value"]
      estimate <- summ_model$tTable[2, "Value"]
      sig <- ifelse(p_val < 0.05, 1, 0)
      louder <- ""
      # Here we assume that the baseline is "Day" (i.e. Day is the reference)
      # so a negative estimate means that Night is lower, hence Day is louder;
      # a positive estimate implies Night is louder.
      if(sig == 1) {
        if(estimate < 0) {
          louder <- "Day"
        } else if(estimate > 0) {
          louder <- "Night"
        }
      }

      ### 2. Log-Transformed Mixed-Effects Model
      model_log <- lme(as.formula(paste("log(", outcome, ") ~", group)),
                       random = ~ 1 | day,
                       data = sub_data,
                       method = "REML")
      summ_model_log <- summary(model_log)
      p_val_log <- summ_model_log$tTable[2, "p-value"]
      estimate_log <- summ_model_log$tTable[2, "Value"]
      sig_log <- ifelse(p_val_log < 0.05, 1, 0)
      louder_log <- ""
      if(sig_log == 1) {
        if(estimate_log < 0) {
          louder_log <- "Day"
        } else if(estimate_log > 0) {
          louder_log <- "Night"
        }
      }

      # Append a row to the results table with both sets of model information
      results <- rbind(
        results,
        data.frame(
          Band = b,
          p_value = round(p_val, 4),
          Significant = sig,
          Louder = louder,
          p_value_log = round(p_val_log, 4),
          Significant_log = sig_log,
          Louder_log = louder_log,
          stringsAsFactors = FALSE
        )
      )
    }

    return(results)
  }

  # Example usage:
  mixedmodel_table <- summarize_mixed_models_table(Quemchi2024_NoiseBand_final)

  mixedmodel_table <- mixedmodel_table %>%
    mutate(Band = as.numeric(Band)) %>%
    arrange(Band)
  print(mixedmodel_table)


  mixedMod_sig_vector <- ifelse(mixedmodel_table$Significant == 1, "yes", "no")
  # Inspect the result
  mixedMod_sig_vector
  ### or, if you use the log-transformation:
  # mixedModLog_sig_vector <- ifelse(mixedmodel_table$Significant_log == 1, "yes", "no")
  # # Inspect the result
  # mixedModLog_sig_vector

}



#### MODEL DIAGNOSTICS (OPTIONAL)
### TO DECIDE IF TO USE THE ORIGINAL OR LOG-TRANSFORMATION OF THE MIXED-EFFECT MODEL
### IN THIS CAMPAIGN I USE THE ORIGINAL (EASIER INTERPRETATION), ALSO BECAUSE THERE'S NO DIFFERENCE WITH THE LOG
{
  library(nlme)
  library(ggplot2)

  # Select a specific band (e.g., band "1") for diagnostics.
  # You can loop through bands if needed.
  band_example <- "20"
  sub_data <- subset(Quemchi2024_NoiseBand_final, as.character(octaveBand) == band_example)

  # Ensure there is a 'day' variable for the random effect.
  sub_data$day <- as.Date(sub_data$date_Local)

  # Fit the original mixed-effects model
  model_orig <- lme(noiseMean ~ Day_Night, random = ~1 | day, data = sub_data)

  # Fit the log-transformed mixed-effects model
  model_log <- lme(log(noiseMean) ~ Day_Night, random = ~1 | day, data = sub_data, method = "REML")

  # Function to create a residual diagnostic plot panel
  plot_diagnostics <- function(model, title_suffix) {
    par(mfrow = c(2,2))

    # 1. Residuals vs. Fitted
    plot(model, resid(., type = "p") ~ fitted(.),
         main = paste("Residuals vs Fitted -", title_suffix),
         xlab = "Fitted values", ylab = "Residuals")

    # 2. Normal Q-Q Plot
    qqnorm(resid(model), main = paste("Normal Q-Q -", title_suffix))
    qqline(resid(model))

    # 3. Histogram of Residuals
    hist(resid(model), main = paste("Histogram of Residuals -", title_suffix),
         xlab = "Residuals", breaks = 10)

    # 4. Scale-Location Plot (to check homoscedasticity)
    fitted_vals <- fitted(model)
    sqrt_abs_resid <- sqrt(abs(resid(model)))
    plot(fitted_vals, sqrt_abs_resid,
         main = paste("Scale-Location -", title_suffix),
         xlab = "Fitted values", ylab = "Sqrt(|Residuals|)")
    abline(h = mean(sqrt_abs_resid, na.rm = TRUE), col = "red")
  }

  # Plot diagnostics for the original model
  plot_diagnostics(model_orig, "Original Model")

  # Plot diagnostics for the log-transformed model
  plot_diagnostics(model_log, "Log-Transformed Model")

}



######################################################
##################### Plots ##########################
######################################################
# Interactive Boxplots
{
  # Compute five-number summary for each Day_Night group
  box_stats <- Quemchi2024_NoiseBand_final %>%
    group_by(Day_Night) %>%
    summarise(
      low  = min(noiseMean, na.rm = TRUE),
      q1   = quantile(noiseMean, 0.25, na.rm = TRUE),
      med  = median(noiseMean, na.rm = TRUE),
      q3   = quantile(noiseMean, 0.75, na.rm = TRUE),
      high = max(noiseMean, na.rm = TRUE)
    ) %>% ungroup()

  # Convert the computed summary to a list of numeric vectors, one per group
  box_data <- split(as.matrix(box_stats %>% select(low, q1, med, q3, high)),
                    seq(nrow(box_stats)))

  # Create the interactive boxplot using hc_add_series_boxplot
  highchart() %>%
    hc_chart(type = "boxplot") %>%
    hc_title(text = "Interactive Boxplot: NoiseMean by Day/Night") %>%
    hc_xAxis(categories = box_stats$Day_Night,
             title = list(text = "Period")) %>%
    hc_add_series(data = box_stats, name = "NoiseMean") %>%
    hc_yAxis(title = list(text = "NoiseMean (SPL RMS)"))
}
# Time series
{
  # Ensure the time variable is in POSIXct format and sorted
  time_series_data <- Quemchi2024_NoiseBand_final %>%
    arrange(date_Local)

  # Create an interactive time series plot
  highchart(type = "stock") %>%
    hc_title(text = "Interactive Time Series of NoiseMean by Day/Night") %>%
    hc_xAxis(type = "datetime") %>%
    # Add separate series for Day and Night
    hc_add_series(data = time_series_data %>% filter(Day_Night == "Day") %>%
                    mutate(time = datetime_to_timestamp(date_Local)),
                  type = "line",
                  hcaes(x = time, y = noiseMean),
                  name = "Day") %>%
    hc_add_series(data = time_series_data %>% filter(Day_Night == "Night") %>%
                    mutate(time = datetime_to_timestamp(date_Local)),
                  type = "line",
                  hcaes(x = time, y = noiseMean),
                  name = "Night") %>%
    # Add smoothed trend lines (using Highcharts' built-in "spline" type)
    hc_add_series(data = time_series_data %>% filter(Day_Night == "Day") %>%
                    mutate(time = datetime_to_timestamp(date_Local)),
                  type = "spline",
                  hcaes(x = time, y = noiseMean),
                  name = "Day Trend", color = "#4572A7") %>%
    hc_add_series(data = time_series_data %>% filter(Day_Night == "Night") %>%
                    mutate(time = datetime_to_timestamp(date_Local)),
                  type = "spline",
                  hcaes(x = time, y = noiseMean),
                  name = "Night Trend", color = "#AA4643")
}
# Density Plot
{
  # Compute kernel density estimates by Day_Night
  density_data <- Quemchi2024_NoiseBand_final %>%
    group_by(Day_Night) %>%
    do({
      dens <- density(.$noiseMean, na.rm = TRUE)
      data.frame(x = dens$x, y = dens$y)
    })

  # Create an interactive density plot
  hc <- highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = "Interactive Density Plot of NoiseMean") %>%
    hc_xAxis(title = list(text = "NoiseMean (SPL RMS)")) %>%
    hc_yAxis(title = list(text = "Density"))

  # Add a series for each Day_Night group
  for(d in unique(density_data$Day_Night)){
    dens_subset <- filter(density_data, Day_Night == d)
    hc <- hc %>%
      hc_add_series(data = list_parse2(dens_subset),
                    type = "line",
                    name = d)
  }
  hc

}
# Faceted Plots for octave Band
{
  # Get the list of unique octaveBands
  bands <- unique(Quemchi2024_NoiseBand_final$octaveBand)

  # Create a list to store the charts
  chart_list <- list()

  for(b in bands){
    # Subset data for the current octaveBand
    data_band <- Quemchi2024_NoiseBand_final %>% filter(octaveBand == b)

    # Calculate the y-axis limits from the data for this band
    ylims <- data_band %>% summarise(ymin = min(noiseMean, na.rm = TRUE),
                                     ymax = max(noiseMean, na.rm = TRUE))

    # Prepare the series data following your reasoning:
    # For each Day_Night group, collect the raw noiseMean values in a list.
    # (Normally, a boxplot series expects a five-number summary per group,
    #  but if this worked for you before, we'll keep it as is.)
    series_data <- data_band %>%
      group_by(Day_Night) %>%
      summarise(values = list(noiseMean)) %>%
      pull(values)

    # Create the highchart boxplot
    chart <- highchart() %>%
      hc_chart(type = "boxplot") %>%
      hc_title(text = paste("Octave Band", b)) %>%
      hc_xAxis(
        categories = unique(data_band$Day_Night),
        title = list(text = "Day/Night")
      ) %>%
      hc_add_series(
        data = series_data,
        name = paste("Octave Band", b)
      ) %>%
      hc_yAxis(
        min = ylims$ymin,
        max = ylims$ymax,
        title = list(text = "NoiseMean (SPL RMS)")
      )

    chart_list[[as.character(b)]] <- chart
  }

  # To view one of the charts (for example, for octaveBand "1"):
  chart_list[["8"]]


  # In a Shiny app or RMarkdown, you can then display these charts in a grid.
  # For example, in RMarkdown, you might use:
  #   library(gridExtra)
  #   grid.arrange(grobs = lapply(chart_list, highchartOutput))
  # In Shiny, you can render each chart in separate UI output elements.

}


######################################################
###### Looking for recurrent Loud events #############
######################################################
# noisePeak
{
  threshold <- quantile(Quemchi2024_NoiseBand_final$noisePeak, 0.9, na.rm = TRUE)

  high_events <- Quemchi2024_NoiseBand_final %>%
    filter(noisePeak > threshold)
  high_events <- high_events %>%
    mutate(highEvent = TRUE)

  # By Hour of Day
  high_events_by_hour <- high_events %>%
    mutate(hour = lubridate::hour(date_Local)) %>%
    group_by(hour) %>%
    summarise(event_count = n())
  # By Day
  high_events_by_day <- high_events %>%
    mutate(day = as.Date(date_Local)) %>%
    group_by(day) %>%
    summarise(event_count = n())

  # Time Series of Events count
  # Assuming high_events_by_day is computed as above:
  highchart(type = "stock") %>%
    hc_title(text = "High noisePeak Events by Day") %>%
    hc_xAxis(type = "datetime") %>%
    hc_add_series(
      data = high_events_by_day %>%
        mutate(timestamp = datetime_to_timestamp(as.POSIXct(day))) %>%
        select(timestamp, event_count),
      type = "column",
      hcaes(x = timestamp, y = event_count),
      name = "Event Count"
    ) %>%
    hc_yAxis(title = list(text = "Number of High Events"))

  # density plot of high events by hour
  # Compute the counts by hour
  high_events_by_hour <- high_events %>%
    mutate(hour = lubridate::hour(date_Local)) %>%
    group_by(hour) %>%
    summarise(event_count = n())

  # Create an interactive line chart for hour-of-day
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = "High noisePeak Events by Hour of Day") %>%
    hc_xAxis(categories = high_events_by_hour$hour,
             title = list(text = "Hour of Day")) %>%
    hc_add_series(name = "Event Count", data = high_events_by_hour$event_count) %>%
    hc_yAxis(title = list(text = "Number of High Events"))


  ### Combined plot
  # Create an interactive time series with markers for high events
  # all_data <- Quemchi2024_NoiseBand_final %>%
  #   arrange(date_Local) %>%
  #   mutate(timestamp = datetime_to_timestamp(date_Local))
  #
  # high_events_markers <- high_events %>%
  #   arrange(date_Local) %>%
  #   mutate(timestamp = datetime_to_timestamp(date_Local))
  #
  # highchart(type = "stock") %>%
  #   hc_title(text = "Time Series of noisePeak with High Event Markers") %>%
  #   hc_xAxis(type = "datetime") %>%
  #   # Plot full noisePeak time series as a line
  #   hc_add_series(data = all_data, type = "line", hcaes(x = timestamp, y = noisePeak),
  #                 name = "noisePeak") %>%
  #   # Overlay high events as markers
  #   hc_add_series(data = high_events_markers, type = "scatter", hcaes(x = timestamp, y = noisePeak),
  #                 name = "High Events", color = "red", marker = list(symbol = "circle", radius = 4))
  #

}
# noiseMean
{
  threshold <- quantile(Quemchi2024_NoiseBand_final$noiseMean, 0.9, na.rm = TRUE)

  high_events <- Quemchi2024_NoiseBand_final %>%
    filter(noiseMean > threshold)
  high_events <- high_events %>%
    mutate(highEvent = TRUE)

  # By Hour of Day
  high_events_by_hour <- high_events %>%
    mutate(hour = lubridate::hour(date_Local)) %>%
    group_by(hour) %>%
    summarise(event_count = n())
  # By Day
  high_events_by_day <- high_events %>%
    mutate(day = as.Date(date_Local)) %>%
    group_by(day) %>%
    summarise(event_count = n())

  # Time Series of Events count
  # Assuming high_events_by_day is computed as above:
  highchart(type = "stock") %>%
    hc_title(text = "High noiseMean Events by Day") %>%
    hc_xAxis(type = "datetime") %>%
    hc_add_series(
      data = high_events_by_day %>%
        mutate(timestamp = datetime_to_timestamp(as.POSIXct(day))) %>%
        select(timestamp, event_count),
      type = "column",
      hcaes(x = timestamp, y = event_count),
      name = "Event Count"
    ) %>%
    hc_yAxis(title = list(text = "Number of High Events"))

  # density plot of high events by hour
  # Compute the counts by hour
  high_events_by_hour <- high_events %>%
    mutate(hour = lubridate::hour(date_Local)) %>%
    group_by(hour) %>%
    summarise(event_count = n())

  # Create an interactive line chart for hour-of-day
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = "High noiseMean Events by Hour of Day") %>%
    hc_xAxis(categories = high_events_by_hour$hour,
             title = list(text = "Hour of Day")) %>%
    hc_add_series(name = "Event Count", data = high_events_by_hour$event_count) %>%
    hc_yAxis(title = list(text = "Number of High Events"))


  ### Combined plot
  # Create an interactive time series with markers for high events
  # all_data <- Quemchi2024_NoiseBand_final %>%
  #   arrange(date_Local) %>%
  #   mutate(timestamp = datetime_to_timestamp(date_Local))
  #
  # high_events_markers <- high_events %>%
  #   arrange(date_Local) %>%
  #   mutate(timestamp = datetime_to_timestamp(date_Local))
  #
  # highchart(type = "stock") %>%
  #   hc_title(text = "Time Series of noisePeak with High Event Markers") %>%
  #   hc_xAxis(type = "datetime") %>%
  #   # Plot full noisePeak time series as a line
  #   hc_add_series(data = all_data, type = "line", hcaes(x = timestamp, y = noisePeak),
  #                 name = "noisePeak") %>%
  #   # Overlay high events as markers
  #   hc_add_series(data = high_events_markers, type = "scatter", hcaes(x = timestamp, y = noisePeak),
  #                 name = "High Events", color = "red", marker = list(symbol = "circle", radius = 4))
  #

}











