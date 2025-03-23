#' 2024_Tab2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
library(shinyWidgets)
library(lubridate)

OLDmod_2024_Tab2_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Overview section (placed on top)
    fluidRow(
      column(
        width = 6,
          box(
            title = tagList(icon("info-circle"), "Analysis Methods"),
            style ='overflow-x: scroll;height:150px;overflow-y: scroll;', # allows user to scroll vertically (set the same height as below)
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "150px",
            collapsible = FALSE,
            maximizable = TRUE,
            htmlOutput(ns("analysisMethod"))
          )
      ),
      column(
        width = 6,
        box(
          title = tagList(icon("lightbulb"), "Results"),
          style ='overflow-x: scroll;height:150px;overflow-y: scroll;', # allows user to scroll vertically (set the same height as below)
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = "150px",
          collapsible = FALSE,
          maximizable = TRUE,
          htmlOutput(ns("resultsSummary"))
        )
      )
    ),

    fluidRow(
      # Sidebar with controls using a collapsible box for a modern look
      column(
        width = 4,
        box(
          title = tagList(icon("sliders-h"), "Controls"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = NULL,
          sliderInput(ns("threshold"), "High noisePeak Threshold (dB):",
                      min = 80, max = 145, value = 100, step = 1),
          radioButtons(ns("aggPeriod"), "Aggregation Period:",
                       choices = c("Hour", "Day"),
                       selected = "Hour"),
          selectInput(ns("octaveBand"), "Select Frequency Band:",
                      choices = setNames(
                        sort(unique(Quemchi2024_NoiseBand_final$octaveBand)),
                        c("20 Hz", "25 Hz", "31.5 Hz", "40 Hz", "50 Hz", "63 Hz", "80 Hz", "100 Hz",
                          "125 Hz", "160 Hz", "200 Hz", "250 Hz", "315 Hz", "400 Hz", "500 Hz", "630 Hz",
                          "800 Hz", "1 kHz", "1.25 kHz", "1.6 kHz", "2 kHz", "2.5 kHz", "3.15 kHz",
                          "4 kHz", "5 kHz", "6.3 kHz", "8 kHz", "10 kHz")
                      ),
                      selected = unique(Quemchi2024_NoiseBand_final$octaveBand)[1]),
          dateRangeInput(ns("dateRange"), "Select Date Range:",
                         start = min(Quemchi2024_NoiseBand_final$date_Local),
                         end = "2024-11-10",
                         format = "yyyy-mm-dd")
        )
      ),
      # Main panel with tabs for different visualizations and summaries
      column(
        width = 8,
        tabsetPanel(
          tabPanel(
            "Summary Stats",
            fluidRow(
              box(
                title = tagList(icon("chart-bar"), "Summary Statistics"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                htmlOutput(ns("summaryStats")),
                htmlOutput(ns("summaryCaption"))
              )
            )
          ),
          tabPanel(
            "Boxplots",
            box(
                title = "Boxplots and Violin plots",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                fluidRow(
                  box(
                    # title = tagList(icon("box"), "Boxplot: Day vs Night"),
                    # status = "primary",
                    # solidHeader = TRUE,
                    width = 6,
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    highchartOutput(ns("boxplotPlot"))
                  ),
                  box(
                    # title = tagList(icon("chart-area"), "Violin Plot: Day vs Night"),
                    # status = "primary",
                    # solidHeader = TRUE,
                    width = 6,
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    highchartOutput(ns("violinPlot"))
                  ),
                  htmlOutput(ns("boxviolinCaption"))
                )
            )
          ),
          tabPanel(
            "Time Series",
            fluidRow(
              box(
                title = tagList(icon("line-chart"), "Time Series: Noise Levels Over Time"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                highchartOutput(ns("timeSeriesPlot")),
                htmlOutput(ns("timeSeriesCaption"))
              )
            )
          ),
          tabPanel(
            "Density",
            fluidRow(
              box(
                title = tagList(icon("chart-area"), "Density Plot: Distribution of Noise Levels"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                highchartOutput(ns("densityPlot")),
                htmlOutput(ns("densityCaption"))
              )
            )
          ),
          tabPanel(
            "High Events",
            box(
              title = "Special Events plots",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              fluidRow(
                box(
                   title = tagList(icon("exclamation-triangle"), "High noisePeak Events Over Time"),
                  # status = "primary",
                  # solidHeader = TRUE,
                  width = 6,
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  highchartOutput(ns("highEventsTimeSeries"))
                  ),
                box(
                  title = tagList(icon("clock"), "High noisePeak Events by Hour"),
                  # status = "primary",
                  # solidHeader = TRUE,
                  width = 6,
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  highchartOutput(ns("highEventsByHour"))
                ),
                htmlOutput(ns("SpecialEventsCaption"))
              )
            )

          )
        )
      )
    )




  )
}

#' 2024_Tab2 Server Functions
#'
#' @noRd
OLDmod_2024_Tab2_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ###############################################################
    # Reactive expression to filter the dataset based on UI inputs
    ###############################################################
    filteredData <- reactive({
      req(input$dateRange)  # Ensure dateRange is provided
      # Filter data based on date range
      data <- Quemchi2024_NoiseBand_final %>%
        filter(date_Local >= as.POSIXct(input$dateRange[1]) &
                 date_Local <= as.POSIXct(input$dateRange[2]))
      # Further filter by the selected octave band if provided
      if (!is.null(input$octaveBand) && input$octaveBand != "") {
        data <- data %>% filter(octaveBand == input$octaveBand)
      }
      # Debug: Print the number of rows after filtering
      print(paste("Filtered data rows:", nrow(data)))
      data
    })


    ########################################
    # Render the Methods and Results as HTML
    ########################################
    # Methods
    {
      output$analysisMethod <- renderUI({
        HTML("
              <p>This analysis aims to examine noise level differences between <strong>day</strong> and <strong>night</strong> using bio-acoustic data from Quemchi (Chiloe, November 2024).</p>

              <h5>1. Data Preparation</h5>
              <ul>
                <li>Aggregated data by minute (to balance detail and noise smoothness).</li>
                <li>Defined 'Day' (06:31 - 20:45) and 'Night' (20:46 - 06:30) using local sunrise/sunset times.</li>
              </ul>

              <h5>2. Exploratory Data Analysis (EDA)</h5>
              <ul>
                <li><strong>Summary statistics</strong>: Mean, median, standard deviation, min, and max noise levels per period.</li>
                <li><strong>Visual Inspection</strong>:
                  <ul>
                    <li>Histograms & Density plots: Check normality.</li>
                    <li>Boxplots & Violin plots: Assess variance differences.</li>
                    <li>Time series plots: Show trends over time.</li>
                  </ul>
                </li>
              </ul>

              <h5>3. Statistical Testing</h5>
              <ul>
                <li><strong>Wilcoxon rank-sum test</strong>: Non-parametric test for median noise level differences between day and night.</li>
                <li><strong>Mixed-Effects Model</strong>:
                  <ul>
                    <li>Fixed effect: Day/Night</li>
                    <li>Random effect: Day (to account for variability across days)</li>
                  </ul>
                </li>
                <li>Model diagnostics: Residual analysis, Q-Q plots, histogram checks.</li>
                <li>Applied <strong>log-transformation</strong> to stabilize variance and improve normality of residuals.</li>
              </ul>
            ")
      })

    }
    # And Results
    {
      output$resultsSummary <- renderUI({
        HTML("
              <h5>1. Exploratory Data Analysis (EDA)</h5>
              <ul>
                <li>Noise distributions are <strong>not normally distributed</strong> (right-skewed, multiple peaks, heavy tails).</li>
                <li>Daytime and nighttime variances are <strong>not equal</strong> (nighttime variance generally higher).</li>
              </ul>

              <h5>2. Statistical Testing</h5>
              <ul>
                <li><strong>Wilcoxon test results</strong>: Statistically significant difference in noise levels for all octave bands (p-value < 0.05 in all cases).</li>
                <li>Daytime median noise levels are consistently <strong>higher</strong> than nighttime levels.</li>
              </ul>

              <h5>3. Mixed-Effects Model</h5>
              <ul>
                <li>Daytime noise levels are significantly <strong>higher</strong> (baseline ~94.88 dB, night ~0.67 dB lower on average).</li>
                <li>Between-day variation is moderate, confirming the necessity of a mixed-effects model.</li>
              </ul>

              <h5>4. Model Diagnostics</h5>
              <ul>
                <li>Residual analysis shows slight skewness, but log-transformation improves normality.</li>
                <li>Log-transformed model confirms significant day/night effect.</li>
              </ul>

              <p><strong>Conclusion:</strong> Daytime noise levels are consistently higher than nighttime across all octave bands, with significant statistical support.</p>
            ")
      })
    }


    ########################################
    # Render the summary statistics as HTML
    ########################################
    {
    output$summaryStats <- renderUI({
      data <- filteredData()

      # Check if data is empty; if so, return a message
      if (nrow(data) == 0) {
        return("No data found for the selected filters.")
      }

      # Calculate summary stats for noiseMean by Day_Night
      summary_stats <- data %>%
        group_by(Day_Night) %>%
        summarise(
          Count  = n(),
          Mean   = round(mean(noiseMean, na.rm = TRUE), 2),
          Median = round(median(noiseMean, na.rm = TRUE), 2),
          Standard_Ddeviation = round(sd(noiseMean, na.rm = TRUE), 2),
          Peak = round(max(noisePeak), 2)
        )

      # Create an HTML table using knitr::kable
      HTML(knitr::kable(summary_stats, format = "html"))
    })

    }
    # And Summary Caption
    {
      output$summaryCaption <- renderUI({
        HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
         Note: The table above summarizes key noise statistics (count, mean, median, standard deviation, peak)
         for each period. Adjust the filters as needed.
       </p>")
      })

    }

    ########################################
    # Render the boxplot
    ########################################
    {
    output$boxplotPlot <- renderHighchart({
      # Retrieve filtered data from the reactive expression
      data <- filteredData()

      # If no data is available, return a highchart with a message
      if(nrow(data) == 0) {
        return(highchart() %>% hc_title(text = "No data available for the selected filters."))
      }

      # Compute five-number summaries for noiseMean by Day_Night
      box_stats <- data %>%
        group_by(Day_Night) %>%
        summarise(
          low  = round(min(noiseMean, na.rm = TRUE), 2),
          q1   = round(quantile(noiseMean, 0.25, na.rm = TRUE), 2),
          med  = round(median(noiseMean, na.rm = TRUE), 2),
          q3   = round(quantile(noiseMean, 0.75, na.rm = TRUE), 2),
          high = round(max(noiseMean, na.rm = TRUE), 2)
        ) %>% ungroup()

      # Build the interactive boxplot
      highchart() %>%
        hc_chart(type = "boxplot") %>%
        hc_title(text = "Interactive Boxplot: NoiseMean by Day/Night") %>%
        hc_xAxis(
          categories = as.character(box_stats$Day_Night),
          title = list(text = "Period")
        ) %>%
        hc_add_series(data = box_stats, name = "NoiseMean") %>%
        hc_yAxis(title = list(text = "NoiseMean (SPL RMS)"))
    })

    }
    # And Boxplot / ViolinPlot Caption
    {
      output$boxviolinCaption <- renderUI({
        HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
         Note: (Left) Boxplot showing noise RMS levels for both Day and Night data. (Right) Violin plot showing the distribution of the data, divided
         by Day and Night data. Through the control panel on the left the user can choose which Octave Band to visualize.
       </p>")
      })
    }


    ########################################
    # Render the violinPlot
    ########################################
    {
      output$violinPlot <- renderHighchart({
        data <- filteredData()
        # Optionally, filter to include only Day and Night:
        # data <- data[data$Day_Night %in% c("Day", "Night"), ]

        if(nrow(data) == 0) {
          return(
            highchart() %>%
              hc_title(text = "No data available for the selected filters.")
          )
        }

        # Explicitly set the x-axis categories to Day and Night.
        categories <- c("Day", "Night")
        # Define the x positions for these categories (using 0-indexing)
        cat_positions <- setNames(0:(length(categories) - 1), categories)

        # Build the base chart:
        # - x-axis: the categorical period ("Day", "Night")
        # - y-axis: continuous noiseMean values.
        hc <- highchart() %>%
          hc_chart(type = "area") %>%
          hc_title(text = "Violin Plot: NoiseMean by Day/Night") %>%
          hc_xAxis(
            categories = categories,
            title = list(text = "Period")
          ) %>%
          hc_yAxis(
            title = list(text = "NoiseMean (SPL RMS)")
          )

        # Maximum horizontal expansion for the violin shape (in x-axis units)
        max_violin_width <- 0.3

        # For each category, compute a density-based polygon and overlay summary markers.
        for(cat in categories) {
          group_data <- data[data$Day_Night == cat, ]
          if(nrow(group_data) < 2) next  # Need enough points to compute density

          # Compute the kernel density for noiseMean.
          # In a vertical violin, the density's x values represent noiseMean.
          dens <- density(group_data$noiseMean, na.rm = TRUE)
          # Scale the density so that the maximum horizontal offset is max_violin_width.
          scale_factor <- max_violin_width / max(dens$y)
          offsets <- dens$y * scale_factor
          # x position for the category
          x_base <- cat_positions[cat]

          # Build the polygon for the violin shape:
          # • x: category position ± offset (horizontal direction)
          # • y: the noiseMean values from dens$x (vertical direction)
          left_side <- data.frame(x = x_base - offsets, y = dens$x)
          right_side <- data.frame(x = x_base + offsets, y = dens$x)
          # Combine left side and reversed right side to form a closed polygon.
          polygon_points <- rbind(left_side, right_side[order(-right_side$y), ])

          # Add the violin shape as an area series.
          hc <- hc %>% hc_add_series(
            data = highcharter::list_parse2(polygon_points),
            type = "area",
            name = paste("Violin", cat),
            color = "rgba(124, 181, 236, 0.5)",
            lineWidth = 1,
            showInLegend = FALSE
          ) %>%
            hc_yAxis(
              title = list(text = "NoiseMean (SPL RMS)"),
              min = 70,        # Set your desired lower limit here
              max = 150       # Set your desired upper limit here
            )

          # Compute summary statistics: median, 25th (Q1), and 75th (Q3) percentiles.
          med_val <- median(group_data$noiseMean, na.rm = TRUE)
          q1 <- quantile(group_data$noiseMean, 0.25, na.rm = TRUE)
          q3 <- quantile(group_data$noiseMean, 0.75, na.rm = TRUE)

          # Add a marker for the median.
          median_point <- data.frame(x = x_base, y = med_val)
          hc <- hc %>% hc_add_series(
            data = highcharter::list_parse2(median_point),
            type = "scatter",
            name = paste("Median", cat),
            marker = list(symbol = "diamond", radius = 4),
            color = "black",
            showInLegend = FALSE
          )

          # Add markers for the 25th and 75th percentiles.
          quartile_points <- data.frame(
            x = rep(x_base, 2),
            y = c(q1, q3),
            name = c("25% Quartiles", "75% Quartiles")
          )
          hc <- hc %>% hc_add_series(
            data = highcharter::list_parse2(quartile_points),
            type = "scatter",
            name = paste("Quartiles", cat),
            marker = list(symbol = "circle", radius = 3),
            color = "gray",
            tooltip = list(pointFormat = "{point.name}: {point.y:.2f}"),
            showInLegend = FALSE
          )

        }

        hc
      })





    }

    ########################################
    # Render the time series
    ########################################
    {
      output$timeSeriesPlot <- renderHighchart({
        data <- filteredData()

        # If no data available, show a message
        if(nrow(data) == 0) {
          return(
            highchart() %>%
              hc_title(text = "No data available for the selected filters.")
          )
        }

        # Order data by date and add a timestamp column using force_tz() so that clock time remains unchanged
        data <- data %>%
          arrange(date_Local) %>%
          mutate(timestamp = datetime_to_timestamp(force_tz(date_Local, tzone = "UTC")))

        # Separate the data by Day and Night
        data_day <- data %>% filter(Day_Night == "Day")
        data_night <- data %>% filter(Day_Night == "Night")

        highchart(type = "stock") %>%
          hc_title(text = "Time Series of NoiseMean by Day and Night") %>%
          # Remove the built-in navigator and range selector
          hc_rangeSelector(enabled = FALSE) %>%
          hc_navigator(enabled = TRUE) %>%
          hc_xAxis(type = "datetime") %>%
          # Add the Day series as markers only (no connecting line)
          hc_add_series(
            data = data_day,
            type = "line",
            hcaes(x = timestamp, y = noiseMean),
            name = "Day",
            color = "#4572A7",
            tooltip = list(valueDecimals = 2),
            lineWidth = 0,         # removes the connecting line
            marker = list(enabled = TRUE)  # shows individual points
          ) %>%
          # Add the Night series as markers only
          hc_add_series(
            data = data_night,
            type = "line",
            hcaes(x = timestamp, y = noiseMean),
            name = "Night",
            color = "#AA4643",
            tooltip = list(valueDecimals = 2),
            lineWidth = 0,         # removes the connecting line
            marker = list(enabled = TRUE)
          ) %>%
          hc_yAxis(title = list(text = "NoiseMean (SPL RMS)"))
      })


    }
    # And timeSeries Caption
    {
      output$timeSeriesCaption <- renderUI({
        HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
         Note: The table above summarizes key noise statistics (count, mean, median, standard deviation, peak)
         for each period. Adjust the filters as needed.
       </p>")
      })

    }

    ########################################
    # Render the Density plot
    ########################################
    {
      output$densityPlot <- renderHighchart({
        data <- filteredData()

        # Check for available data
        if(nrow(data) == 0) {
          return(
            highchart() %>%
              hc_title(text = "No data available for the selected filters.")
          )
        }

        # Filter the data into Day and Night subsets
        data_day <- data %>% filter(Day_Night == "Day")
        data_night <- data %>% filter(Day_Night == "Night")

        # Compute density estimates for noiseMean for Day and Night
        dens_day <- density(data_day$noiseMean, na.rm = TRUE)
        dens_night <- density(data_night$noiseMean, na.rm = TRUE)

        # Prepare the series: convert each density estimate into a list of [x, y] pairs
        library(purrr)
        day_series <- map2(dens_day$x, dens_day$y, function(x, y) list(x, y))
        night_series <- map2(dens_night$x, dens_night$y, function(x, y) list(x, y))

        # Build the interactive density plot
        highchart() %>%
          hc_chart(type = "line") %>%
          hc_title(text = "Density Plot: Distribution of Noise Levels") %>%
          hc_add_series(name = "Day", data = day_series, type = "line") %>%
          hc_add_series(name = "Night", data = night_series, type = "line") %>%
          hc_xAxis(title = list(text = "NoiseMean (SPL RMS)")) %>%
          hc_yAxis(title = list(text = "Density"))

        # The density value at a given x (noiseMean value) tells you about the relative likelihood of observing values around that point
      })

    }
    # And Density Caption
    {
      output$densityCaption <- renderUI({
        HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
         Note: The table above summarizes key noise statistics (count, mean, median, standard deviation, peak)
         for each period. Adjust the filters as needed.
       </p>")
      })

    }

    ########################################
    # Render the TimeSeries w Marker & Events by hour
    ########################################
    {
      # High Events: Time Series with Markers
      output$highEventsTimeSeries <- renderHighchart({
        data <- filteredData()

        # Check if data is available
        if(nrow(data) == 0) {
          return(
            highchart() %>%
              hc_title(text = "No data available for the selected filters.")
          )
        }

        # Use the threshold from the slider to define high noisePeak events
        high_threshold <- input$threshold

        # Filter high events based on noisePeak threshold
        high_events <- data %>%
          filter(noisePeak > high_threshold)

        # Convert date_Local to a numeric timestamp.
        # We use force_tz() to preserve the clock time (so that 20:46 remains 20:46).
        data <- data %>%
          arrange(date_Local) %>%
          mutate(timestamp = datetime_to_timestamp(force_tz(date_Local, tzone = "UTC")))

        high_events <- high_events %>%
          arrange(date_Local) %>%
          mutate(timestamp = datetime_to_timestamp(force_tz(date_Local, tzone = "UTC")))

        # Build the time series plot: full noisePeak series plus markers for high events.
        highchart(type = "stock") %>%
          hc_title(text = "Time Series of noisePeak with High Event Markers") %>%
          hc_rangeSelector(enabled = FALSE) %>%
          hc_navigator(enabled = FALSE) %>%
          hc_xAxis(type = "datetime") %>%
          hc_add_series(
            data = data,
            type = "line",
            hcaes(x = timestamp, y = noisePeak),
            name = "noisePeak",
            tooltip = list(valueDecimals = 2)
          ) %>%
          hc_add_series(
            data = high_events,
            type = "scatter",
            hcaes(x = timestamp, y = noisePeak),
            name = "High Events",
            color = "red",
            marker = list(symbol = "circle", radius = 4),
            tooltip = list(valueDecimals = 2)
          ) %>%
          hc_yAxis(title = list(text = "noisePeak (dB)"))
      })

      # High Events: Column Chart by Hour
      output$highEventsByHour <- renderHighchart({
        data <- filteredData()

        # Check if data is available
        if(nrow(data) == 0) {
          return(
            highchart() %>%
              hc_title(text = "No data available for the selected filters.")
          )
        }

        # Use the threshold from the slider to define high noisePeak events
        high_threshold <- input$threshold

        # Filter the data for high noisePeak events
        high_events <- data %>% filter(noisePeak > high_threshold)

        # Group high events by hour (using lubridate to extract the hour)
        high_events_by_hour <- high_events %>%
          mutate(hour = lubridate::hour(date_Local)) %>%
          group_by(hour) %>%
          summarise(event_count = n())

        # Build an interactive column chart
        highchart() %>%
          hc_chart(type = "column") %>%
          hc_title(text = "High noisePeak Events by Hour of Day") %>%
          hc_xAxis(
            categories = as.character(high_events_by_hour$hour),
            title = list(text = "Hour of Day")
          ) %>%
          hc_add_series(
            name = "High Events",
            data = high_events_by_hour$event_count
          ) %>%
          hc_yAxis(title = list(text = "Number of High Events"))
      })

    }
    # And Special Events Caption
    {
      output$SpecialEventsCaption <- renderUI({
        HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
         Note: The table above summarizes key noise statistics (count, mean, median, standard deviation, peak)
         for each period. Adjust the filters as needed.
       </p>")
      })

    }




  })
}

## To be copied in the UI
# OLDmod_2024_Tab2_ui("2024_Tab2_1")

## To be copied in the server
# OLDmod_2024_Tab2_server("2024_Tab2_1")
