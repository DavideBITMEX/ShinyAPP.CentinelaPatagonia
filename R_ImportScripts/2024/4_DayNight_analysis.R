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

        # The distributions of noiseMean appear to be NOT Normally Distributed for day and night across different octave bands: many appear
        # to be right-sweed (right tale), multiple peaks and there are outliers and some heavy tails.
        # ---> NOT NORMALLY DISTRIBUTED

  ### 2b. Boxplots and Violin Plots
  # Boxplot for noiseMean by Day_Night
        ggplot(Quemchi2024_NoiseBand_final, aes(x = Day_Night, y = noiseMean, fill = Day_Night)) +
          geom_boxplot() +
          facet_wrap(~ octaveBand) +
          labs(title = "Boxplot of Noise Mean by Day and Night",
               x = "Period", y = "Noise Mean (SPL RMS)")

        # The interquartile ranges (IQRs) (box width) differ between day and night in multiple facets (e.g., period 1, 3, 5).
        # The length of the whiskers (which indicate the spread of most data points) suggests that night-time variance is often higher than daytime variance in some periods.

        # Violin Plot for noiseMean by Day_Night
        ggplot(Quemchi2024_NoiseBand_final, aes(x = Day_Night, y = noiseMean, fill = Day_Night)) +
          geom_violin(trim = FALSE) +
          facet_wrap(~ octaveBand) +
          labs(title = "Violin Plot of Noise Mean by Day and Night",
               x = "Period", y = "Noise Mean (SPL RMS)")

        # Violin plots visualize the distribution density and show that in some periods, the night-time noise has wider spread, indicating greater variance.
        # Some periods (e.g., 4, 5, and possibly 3) show a more uniform spread at night, whereas daytime distributions are tighter.
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
{
  ### 1. Wilcoxon test (tested for each Octave Band)
        # Null Hypothesis (H₀): The median noiseMean for day and night are equal
        # Alternative Hypothesis (H₁): The median noiseMean for day and night are different
        # A p-value < 0.05, you reject the null hypothesis and conclude that there is a statistically significant difference in the median noise levels between day and night
        # A p-value > 0.05 would indicate insufficient evidence to claim a difference

        ### Octave Band 1
        data_band1 <- subset(Quemchi2024_NoiseBand_final, octaveBand == "1")
        # Apply the Wilcoxon rank-sum test to compare noiseMean between day and night
        wilcox_result1 <- wilcox.test(noiseMean ~ Day_Night, data = data_band1)
        print(wilcox_result1)
        # pvalue < 2.2e-16...There is a statistically significant difference in the median noise levels between day and night for Octave Band 1
        # Is day or night louder (we check the median)? You can also check the boxplots and violin plots
        data_band1 %>%
          group_by(Day_Night) %>%
          summarise(median = median(noiseMean, na.rm = TRUE),
                    mean   = mean(noiseMean, na.rm = TRUE))
        # Day is Louder

        ### Octave Band 2
        data_band2 <- subset(Quemchi2024_NoiseBand_final, octaveBand == "2")
        # Apply the Wilcoxon rank-sum test to compare noiseMean between day and night
        wilcox_result2 <- wilcox.test(noiseMean ~ Day_Night, data = data_band2)
        print(wilcox_result2)
        # pvalue < 2.2e-16...There is a statistically significant difference in the median noise levels between day and night for Octave Band 2
        # Is day or night louder (we check the median)? You can also check the boxplots and violin plots
        data_band2 %>%
          group_by(Day_Night) %>%
          summarise(median = median(noiseMean, na.rm = TRUE),
                    mean   = mean(noiseMean, na.rm = TRUE))
        # Day is Louder

        ### Octave Band 3
        data_band3 <- subset(Quemchi2024_NoiseBand_final, octaveBand == "3")
        # Apply the Wilcoxon rank-sum test to compare noiseMean between day and night
        wilcox_result3 <- wilcox.test(noiseMean ~ Day_Night, data = data_band3)
        print(wilcox_result3)
        # pvalue = 0.0013...There is a statistically significant difference in the median noise levels between day and night for Octave Band 3
        # Is day or night louder (we check the median)? You can also check the boxplots and violin plots
        data_band3 %>%
          group_by(Day_Night) %>%
          summarise(median = median(noiseMean, na.rm = TRUE),
                    mean   = mean(noiseMean, na.rm = TRUE))
        # Day is Louder

        ### Octave Band 4
        data_band4 <- subset(Quemchi2024_NoiseBand_final, octaveBand == "4")
        # Apply the Wilcoxon rank-sum test to compare noiseMean between day and night
        wilcox_result4 <- wilcox.test(noiseMean ~ Day_Night, data = data_band4)
        print(wilcox_result4)
        # pvalue = 0.01449...There is a statistically significant difference in the median noise levels between day and night for Octave Band 4
        # Is day or night louder (we check the median)? You can also check the boxplots and violin plots
        data_band4 %>%
          group_by(Day_Night) %>%
          summarise(median = median(noiseMean, na.rm = TRUE),
                    mean   = mean(noiseMean, na.rm = TRUE))
        # Day is Louder

        ### Octave Band 5
        data_band5 <- subset(Quemchi2024_NoiseBand_final, octaveBand == "5")
        # Apply the Wilcoxon rank-sum test to compare noiseMean between day and night
        wilcox_result5 <- wilcox.test(noiseMean ~ Day_Night, data = data_band5)
        print(wilcox_result5)
        # pvalue < 2.2e-16...There is a statistically significant difference in the median noise levels between day and night for Octave Band 5
        # Is day or night louder (we check the median)? You can also check the boxplots and violin plots
        data_band5 %>%
          group_by(Day_Night) %>%
          summarise(median = median(noiseMean, na.rm = TRUE),
                    mean   = mean(noiseMean, na.rm = TRUE))
        # Day is Louder

        ### Octave Band 6
        data_band6 <- subset(Quemchi2024_NoiseBand_final, octaveBand == "6")
        # Apply the Wilcoxon rank-sum test to compare noiseMean between day and night
        wilcox_result6 <- wilcox.test(noiseMean ~ Day_Night, data = data_band6)
        print(wilcox_result6)
        # pvalue < 2.2e-16...There is a statistically significant difference in the median noise levels between day and night for Octave Band 6
        # Is day or night louder (we check the median)? You can also check the boxplots and violin plots
        data_band6 %>%
          group_by(Day_Night) %>%
          summarise(median = median(noiseMean, na.rm = TRUE),
                    mean   = mean(noiseMean, na.rm = TRUE))
        # Day is Louder

        ### Octave Band 7
        data_band7 <- subset(Quemchi2024_NoiseBand_final, octaveBand == "7")
        # Apply the Wilcoxon rank-sum test to compare noiseMean between day and night
        wilcox_result7 <- wilcox.test(noiseMean ~ Day_Night, data = data_band7)
        print(wilcox_result7)
        # pvalue < 2.2e-16...There is a statistically significant difference in the median noise levels between day and night for Octave Band 7
        # Is day or night louder (we check the median)? You can also check the boxplots and violin plots
        data_band7 %>%
          group_by(Day_Night) %>%
          summarise(median = median(noiseMean, na.rm = TRUE),
                    mean   = mean(noiseMean, na.rm = TRUE))
        # Day is Louder

        ### Octave Band 8
        data_band8 <- subset(Quemchi2024_NoiseBand_final, octaveBand == "8")
        # Apply the Wilcoxon rank-sum test to compare noiseMean between day and night
        wilcox_result8 <- wilcox.test(noiseMean ~ Day_Night, data = data_band8)
        print(wilcox_result8)
        # pvalue < 2.2e-16...There is a statistically significant difference in the median noise levels between day and night for Octave Band 8
        # Is day or night louder (we check the median)? You can also check the boxplots and violin plots
        data_band8 %>%
          group_by(Day_Night) %>%
          summarise(median = median(noiseMean, na.rm = TRUE),
                    mean   = mean(noiseMean, na.rm = TRUE))
        # Day is Louder

        # ---> Statistically significant difference for every Octave Band, where the louder Data was always during Day Time

  ### 2. Mixed-Effects Model
        # Ensure you have a variable representing the day (e.g., as Date)
        Quemchi2024_NoiseBand_final$day <- as.Date(Quemchi2024_NoiseBand_final$date_Local)
        # Fit a mixed-effects model:
        # - Fixed effect: Day_Night
        # - Random effect: day (to account for variability across different days)
        mixed_model <- lme(noiseMean ~ Day_Night, random = ~ 1 | day, data = Quemchi2024_NoiseBand_final)
        summary(mixed_model)

        # Results:
        # Baseline (Day): The average noiseMean is approximately 94.88.
        # Night Effect: NoiseMean is about 0.67 units lower at night than during the day.
        # Significance: The difference is highly statistically significant.
        # Between-Day Variation: There is moderate variability in baseline noise levels across days, as indicated by the random effect.

  ### 3. Model Diagnostic
        plot(fitted(mixed_model), residuals(mixed_model, type = "normalized"),
             xlab = "Fitted values",
             ylab = "Normalized Residuals",
             main = "Residuals vs. Fitted Values")
        abline(h = 0, col = "red", lty = 2)

        # Q-Q plot of normalized residuals
        qqnorm(residuals(mixed_model, type = "normalized"),
               main = "Q-Q Plot of Normalized Residuals")
        qqline(residuals(mixed_model, type = "normalized"), col = "red", lty = 2)

        hist(residuals(mixed_model, type = "normalized"),
             breaks = 30,
             main = "Histogram of Normalized Residuals",
             xlab = "Normalized Residuals",
             col = "lightblue")

  ### 4. Log-transformation
        # Fit the model by applying the log transformation in the formula
        mixed_model_log <- lme(log(noiseMean) ~ Day_Night,
                               random = ~ 1 | day,
                               data = Quemchi2024_NoiseBand_final,
                               method = "REML")
        summary(mixed_model_log)

        # Model Diagnostic
        # Plot the fitted values vs. normalized residuals
        plot(fitted(mixed_model_log), residuals(mixed_model_log, type = "normalized"),
             xlab = "Fitted Values",
             ylab = "Normalized Residuals",
             main = "Residuals vs. Fitted Values (Log-Transformed Model)")
        abline(h = 0, col = "red", lty = 2)

        # Q-Q plot for the normalized residuals
        qqnorm(residuals(mixed_model_log, type = "normalized"),
               main = "Q-Q Plot of Normalized Residuals (Log-Transformed Model)")
        qqline(residuals(mixed_model_log, type = "normalized"), col = "red", lty = 2)



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


# save("Nov5", "Nov6", "Nov7", "Nov8", "Nov9",
#      "Quemchi2024_NoiseBand",
#      "Quemchi2024_NoiseBand_final",
#      file = "R/Environment.RData")









