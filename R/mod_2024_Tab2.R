# Load required packages
library(shiny)
library(shinyWidgets)
library(highcharter)
library(dplyr)
library(lubridate)
library(knitr)
library(purrr)


## UI function for the module
mod_2024_Tab2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Custom CSS for the table and tabset
    tags$style(HTML("
  /* Force table headers to stay on one line (no wrapping) */
  .table.table-striped.table-hover th {
    white-space: nowrap;
  }
  /* Enhanced Pill tabs styling */
  .nav-pills > li > a {
    border-radius: 8px;
    margin: 3px;
    padding: 12px 20px;
    font-size: 16px;
    font-weight: bold;
    background-color: #ffffff;
    color: #333;
    border: 1px solid #ddd;
  }
  /* On-hover effect for the tabs */
  .nav-pills > li > a:hover {
    background-color: #f7f7f7;
  }
"))

    ,

    # Timeline navigation: shows the sequential steps in the analysis
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 20px;",
          radioGroupButtons(
            inputId = ns("timelineStep"),
            choices = c("1. Data Preparation/Methods",
                        "2. Data Exploration",
                        "3. Results"),
            #selected = "1. Data Preparation/Methods",
            status = "primary",
            justified = TRUE,
            checkIcon = list(yes = icon("check"))
          )
        )
      )
    ),

    # Main content area – content changes dynamically according to the selected timeline step.
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("mainContent"))
      )
    )
  )
}

## Server function for the module
mod_2024_Tab2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Assume data is already loaded as Quemchi2024_NoiseBand_final
    data <- Quemchi2024_NoiseBand_final

    ###############################################################
    # Frequency lookup for the 1/3 octave bands (in Hz)
    ###############################################################
    freqs <- c("20", "25", "31.5", "40", "50", "63", "80", "100",
               "125", "160", "200", "250", "315", "400", "500", "630",
               "800", "1000", "1250", "1600", "2000", "2500", "3150",
               "4000", "5000", "6300", "8000", "10000")

    # Sorted unique bands from the dataset (assuming these are in the correct order)
    bands_ordered <- sort(unique(data$octaveBand))

    ###############################################################
    # Define UI components for each timeline step
    ###############################################################

    # --- Step 1: Data Preparation and Methods ---
    ui_methods <- box(
      title = "Data Preparation & Methods",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      HTML("
        <p>This analysis examines noise level differences between <strong>day</strong> and <strong>night</strong> using bio-acoustic data from Quemchi (Chiloe, November 2024).</p>
        <h5>1. Data Preparation</h5>
        <ul>
          <li>Data aggregated by minute to balance detail and smooth out noise.</li>
          <li>Defined 'Day' (06:31 - 20:45) and 'Night' (20:46 - 06:30) using local sunrise/sunset times.</li>
        </ul>
        <h5>2. Exploratory Data Analysis (EDA)</h5>
        <ul>
          <li><strong>Summary statistics</strong>: Mean, median, standard deviation, min, and max noise levels per period.</li>
          <li><strong>Visual Inspection</strong>:
            <ul>
              <li>Histograms & Density plots for normality checks.</li>
              <li>Boxplots & Violin plots to assess variance differences.</li>
              <li>Time series plots for trends over time.</li>
            </ul>
          </li>
        </ul>
        <h5>3. Statistical Testing</h5>
        <ul>
          <li><strong>Wilcoxon rank-sum test</strong> to compare medians between day and night.</li>
          <li><strong>Mixed-Effects Model</strong> with:
            <ul>
              <li>Fixed effect: Day/Night</li>
              <li>Random effect: Day (to account for between-day variability)</li>
            </ul>
          </li>
          <li>Log-transformation applied to stabilize variance and improve residual normality.</li>
        </ul>
      ")
    )

    # --- Step 2: Data Exploration ---
    ui_exploration <- tagList(
      fluidRow(
        # Left column: Summary Statistics Table (with horizontal & vertical scroll)
        column(
          width = 4,
          box(
            title = tagList(icon("chart-bar"), "Summary Statistics"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            htmlOutput(ns("summaryStats")),
            htmlOutput(ns("summaryCaption"))
          )
        ),
        # Right column: Tabset with two tabs: "Boxplot & Violin" and "Density"
        column(
          width = 8,
          # We do not set 'selected' here, so Shiny can highlight whichever tab is clicked
          tabsetPanel(
            type = "pills",
            id = ns("explorationTabs"),

            tabPanel(
              "Boxplot & Violin",
              box(
                title = tagList(icon("chart-box"), "Boxplot & Violin Plot"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                # Select input for octave band (with frequency label)
                selectInput(ns("octaveBandBV"), "Choose Third Octave Band:",
                            choices = setNames(bands_ordered,
                                               paste0(bands_ordered, " (", freqs, " Hz)")),
                            selected = bands_ordered[1]),
                fluidRow(
                  column(
                    width = 6,
                    highchartOutput(ns("boxplotPlot"), height = "400px")
                  ),
                  column(
                    width = 6,
                    highchartOutput(ns("violinPlot"), height = "400px")
                  )
                ),
                htmlOutput(ns("boxviolinCaption"))
              )
            ),

            tabPanel(
              "Density",
              box(
                title = tagList(icon("chart-area"), "Density Plot"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                # Select input for octave band (with frequency label)
                selectInput(ns("octaveBandDensity"), "Choose Octave Band:",
                            choices = setNames(bands_ordered,
                                               paste0(bands_ordered, " (", freqs, " Hz)")),
                            selected = bands_ordered[1]),
                highchartOutput(ns("densityPlot"), height = "500px"),
                htmlOutput(ns("densityCaption"))
              )
            )
          )
        )
      )
    )

    # --- Step 3: Results & Conclusions ---
    ui_results <- box(
      title = "Results & Conclusions",
      status = "primary",
      solidHeader = TRUE,
      width = 12,

      ### old version showing 3 plots
      # fluidRow(
      #   column(
      #     width = 6,
      #     div(
      #       style = "border: none; box-shadow: none;",
      #       highchartOutput(ns("ResultsPlot1"), height = "400px")
      #     )
      #   ),
      #   column(
      #     width = 6,
      #     fluidRow(
      #       div(
      #         style = "border: none; box-shadow: none; margin-bottom: 10px;",
      #         highchartOutput(ns("ResultsPlot2"), height = "200px")
      #       )
      #     ),
      #     fluidRow(
      #       div(
      #         style = "border: none; box-shadow: none;",
      #         highchartOutput(ns("ResultsPlot3"), height = "200px")
      #       )
      #     )
      #   )
      # ),

      fluidRow(
        column(
          width = 12,
          div(
            style = "border: none; box-shadow: none;",
            highchartOutput(ns("ResultsPlot1"), height = "400px")
          )
        )
      ),



      HTML("<p style='font-style: italic; font-size: 16px; color: #555; text-align: center; margin-top: 10px;'>
              Figure 1. Results & Conclusions: Left panel shows the primary trend in the noise data, while the right panels provide complementary insights into variability and distribution.
           </p>
           <hr>"),
      HTML("
        <h5>Exploratory Data Analysis (EDA)</h5>
        <ul>
          <li>Noise distributions are <strong>non-normal</strong> (right-skewed with multiple peaks and heavy tails).</li>
          <li>Variances differ between day and night – with nighttime showing higher variability.</li>
        </ul>
        <h5>Statistical Testing</h5>
        <ul>
          <li><strong>Wilcoxon test</strong>: Statistically significant differences (p < 0.05) across all octave bands.</li>
          <li>Daytime median noise levels are consistently <strong>higher</strong> than nighttime levels.</li>
        </ul>
        <h5>Mixed-Effects Model</h5>
        <ul>
          <li>Daytime levels are significantly higher (~94.88 dB vs. night ~0.67 dB lower on average).</li>
          <li>Between-day variation supports the use of a mixed-effects framework.</li>
        </ul>
        <h5>Model Diagnostics</h5>
        <ul>
          <li>Residuals show slight skewness; log-transformation improves normality.</li>
          <li>The log-transformed model reaffirms a significant day/night effect.</li>
        </ul>
        <p><strong>Conclusion:</strong> Daytime noise levels are consistently higher than nighttime across all octave bands, with robust statistical evidence supporting this finding.</p>
      ")
    )

    ###############################################################
    # Main content logic
    ###############################################################
    output$mainContent <- renderUI({
      if (input$timelineStep == "1. Data Preparation/Methods") {
        ui_methods
      } else if (input$timelineStep == "2. Data Exploration") {
        ui_exploration
      } else if (input$timelineStep == "3. Results") {
        ui_results
      }
    })

    ###############################################################
    # Render outputs for the Exploration section
    ###############################################################

    # --- Summary Statistics Table with measurement units & no text wrapping ---
    output$summaryStats <- renderUI({
      if (nrow(data) == 0) {
        return("No data found for the selected filters.")
      }
      summary_stats <- data %>%
        group_by(octaveBand, Day_Night) %>%
        summarise(
          Count             = n(),
          `Mean (dB SPL)`   = round(mean(noiseMean, na.rm = TRUE), 2),
          `Median (dB SPL)` = round(median(noiseMean, na.rm = TRUE), 2),
          `SD (dB SPL)`     = round(sd(noiseMean, na.rm = TRUE), 2),
          `Peak (dB)`       = round(max(noisePeak, na.rm = TRUE), 2)
        ) %>%
        ungroup()

      # Add frequency info based on the ordering (match by the order of bands)
      summary_stats$Frequency <- freqs[match(summary_stats$octaveBand, bands_ordered)]

      # Rename columns for better display (with units if needed)
      summary_stats <- summary_stats %>%
        rename(`Third Octave Band` = octaveBand,
               `Day/Night` = Day_Night,
               `Frequency (Hz)` = Frequency)

      # Reorder columns as desired
      summary_stats <- summary_stats %>%
        select(`Frequency (Hz)`, `Day/Night`, Count,
               `Mean (dB SPL)`, `Median (dB SPL)`, `SD (dB SPL)`, `Peak (dB)`, `Third Octave Band`)

      # Wrap table in a div with horizontal and vertical scrolling
      HTML(paste0(
        "<div style='max-height:300px; overflow-y:auto; overflow-x:auto;'>",
        knitr::kable(summary_stats, format = 'html', table.attr = 'class=\"table table-striped table-hover\"'),
        "</div>"
      ))
    })

    output$summaryCaption <- renderUI({
      HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
             Note: This table summarizes key noise statistics per period (with corresponding center frequencies). Adjust filters as needed.
           </p>")
    })

    # --- Boxplot of NoiseMean by Day/Night ---
    output$boxplotPlot <- renderHighchart({
      req(input$octaveBandBV)
      data_filtered <- data %>% filter(octaveBand == input$octaveBandBV)
      if(nrow(data_filtered) == 0) {
        return(highchart() %>% hc_title(text = "No data available for the selected filters."))
      }
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
          showInLegend = FALSE  # This prevents the series from being clickable in the legend
        ) %>%
        hc_plotOptions(boxplot = list(
          stemWidth = 2,
          whiskerLength = "50%",
          lineWidth = 2,
          medianColor = "red",   # Sets the median line color to red
          medianWidth = 4        # Increases the thickness of the median line
        )) %>%
        hc_tooltip(pointFormat =
                     "<b>Low:</b> {point.low}<br><b>Q1:</b> {point.q1}<br><b>Median:</b> {point.median}<br><b>Q3:</b> {point.q3}<br><b>High:</b> {point.high}")
    })

    output$boxviolinCaption <- renderUI({
      HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
             (Left) Boxplot of noise levels; (Right) Violin plot showing distributions by period.
           </p>")
    })

    # --- Violin Plot (via density estimates) ---
    output$violinPlot <- renderHighchart({
      req(input$octaveBandBV)
      data_filtered <- data %>% filter(octaveBand == input$octaveBandBV)
      if(nrow(data_filtered) == 0) {
        return(highchart() %>% hc_title(text = "No data available for the selected filters."))
      }
      categories <- c("Day", "Night")
      cat_positions <- setNames(0:(length(categories) - 1), categories)

      hc <- highchart() %>%
        hc_chart(type = "area") %>%
        hc_title(text = "Violin Plot: NoiseMean by Period") %>%
        hc_xAxis(categories = categories, title = list(text = "Period")) %>%
        hc_yAxis(title = list(text = "NoiseMean (SPL RMS)"))

      max_violin_width <- 0.3

      for(cat in categories) {
        group_data <- data_filtered[data_filtered$Day_Night == cat, ]
        if(nrow(group_data) < 2) next

        dens <- density(group_data$noiseMean, na.rm = TRUE)
        scale_factor <- max_violin_width / max(dens$y)
        offsets <- dens$y * scale_factor
        x_base <- cat_positions[cat]
        left_side <- data.frame(x = x_base - offsets, y = dens$x)
        right_side <- data.frame(x = x_base + offsets, y = dens$x)
        polygon_points <- rbind(left_side, right_side[order(-right_side$y), ])

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
      hc
    })

    # --- Density Plot ---
    output$densityPlot <- renderHighchart({
      req(input$octaveBandDensity)
      data_filtered <- data %>% filter(octaveBand == input$octaveBandDensity)
      if(nrow(data_filtered) == 0) {
        return(highchart() %>% hc_title(text = "No data available for the selected filters."))
      }
      dens_day <- density(data_filtered %>% filter(Day_Night == "Day") %>% pull(noiseMean), na.rm = TRUE)
      dens_night <- density(data_filtered %>% filter(Day_Night == "Night") %>% pull(noiseMean), na.rm = TRUE)
      day_series <- map2(dens_day$x, dens_day$y, function(x, y) list(x, y))
      night_series <- map2(dens_night$x, dens_night$y, function(x, y) list(x, y))
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = "Density Plot: Distribution of Noise Levels") %>%
        hc_add_series(name = "Day", data = day_series, type = "line") %>%
        hc_add_series(name = "Night", data = night_series, type = "line") %>%
        hc_xAxis(title = list(text = "NoiseMean (SPL RMS)")) %>%
        hc_yAxis(title = list(text = "Density"))
    })

    output$densityCaption <- renderUI({
      HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
             Note: The density plot illustrates the distribution of noise levels for both Day and Night.
           </p>")
    })

    ###############################################################
    # Render outputs for the Results section (Random Plots)
    ###############################################################

    output$ResultsPlot1 <- renderHighchart({
      # Define your significance vector and central frequencies (these should match your data)
      my_signif_vector <- c("yes", "yes", "yes", "yes", "yes", "yes", "no", "yes",
                            "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes",
                            "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes",
                            "yes", "yes", "yes", "yes")
      my_CentralFreq <- c("20", "25", "31.5", "40", "50", "63", "80", "100",
                          "125", "160", "200", "250", "315", "400", "500", "630",
                          "800", "1000", "1250", "1600", "2000", "2500", "3150",
                          "4000", "5000", "6300", "8000", "10000")

      DayNightBoxPlot(data,
                      signif_vector = my_signif_vector,
                      CentralFreq = my_CentralFreq,
                      highlight_bands = c("6", "9"))
    })



    # output$ResultsPlot2 <- renderHighchart({
    #   DayNightBoxPlotOneBand(data, NoctaveBand = 6, centreFreq = "63")
    # })

    # output$ResultsPlot3 <- renderHighchart({
    #   DayNightBoxPlotOneBand(data, NoctaveBand = 9, centreFreq = "125")
    # })

  })
}
