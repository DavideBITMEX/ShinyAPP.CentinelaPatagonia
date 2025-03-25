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
      fluidRow(
        column(
          width = 12,
          div(
            style = "border: none; box-shadow: none;",
            highchartOutput(ns("ResultsPlot"), height = "400px")
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

    # --- Summary Statistics Caption ---
    output$summaryCaption <- renderUI({
      HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
             Note: This table summarizes key noise statistics per period (with corresponding center frequencies). Adjust filters as needed.
           </p>")
    })

    # # --- Boxplot of NoiseMean by Day/Night ---
    output$boxplotPlot <- renderHighchart({
      req(input$octaveBandBV)
      single_boxplot(data = Quemchi2024_NoiseBand_final, selected_band = input$octaveBandBV)
    })

    # --- Violin Plot of NoiseMean by Day/Night ---
    output$violinPlot <- renderHighchart({
      req(input$octaveBandBV)
      single_violinplot(data = Quemchi2024_NoiseBand_final, selected_band = input$octaveBandBV)
    })

    # --- Density Plot ---
    output$densityPlot <- renderHighchart({
      req(input$octaveBandDensity)
      single_densityplot(data = Quemchi2024_NoiseBand_final, selected_band = input$octaveBandDensity)
    })


    # --- Caption below BoxPlot & ViolinPlot --- #
    output$boxviolinCaption <- renderUI({
      HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
             (Left) Boxplot of noise levels; (Right) Violin plot showing distributions by period.
           </p>")
    })

    # --- Caption below DensityPlot --- #
    output$densityCaption <- renderUI({
      HTML("<p style='font-style: italic; font-size: 16px; color: #666; margin-top: 10px;'>
             Note: The density plot illustrates the distribution of noise levels for both Day and Night.
           </p>")
    })

    ###############################################################
    # Render outputs for the Results section (Random Plots)
    ###############################################################

    output$ResultsPlot <- renderHighchart({
      # Define your significance vector and central frequencies (these should match your data)
      my_signif_vector <- c("yes", "yes", "yes", "yes", "yes", "no", "yes", "no",
                            "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes",
                            "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes",
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





  })
}
