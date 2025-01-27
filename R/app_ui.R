#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(golem)
library(shiny)
library(bs4Dash)


app_ui <- function(request) {
  bs4DashPage(
    dark = FALSE,
    title = "Centinela Patagonia Dashboard",

    # Header with logo
    header = bs4DashNavbar(
      skin = "dark",
      title = bs4DashBrand(
        title = img(src = "Logo.png", height = "40px"), # Logo added to the header
        color = "primary"
      ),
      status = "primary",
      border = TRUE
    ),

    # No sidebar
    sidebar = bs4DashSidebar(disable = TRUE),

    # Full-width body using `fluidPage`
    body = bs4DashBody(
      fluidPage(  # Explicitly use fluidPage for full-width layout
        fluidRow(
          # Left column
          column(
            width = 10,
            selectInput(
              inputId = "campaign_selector",
              label = "Select Year",
              choices = c("2023", "2024"),
              selected = "2024"
            ),
            mod_map_2024_ui("map_2024_1")
          ),
          column(
            width = 2,
            selectInput(
              inputId = "trial",
              label = "trial",
              choices = c("2023", "2024"),
              selected = "2024"
          )
          )
          ),

        hr(),

        fluidRow(
          # Right column with tabs
          column(
            width = 12,
            bs4TabCard(
              width = 12,
              id = "tabs",
              side = "left",  # Tab navigation on the left
              # Tab 1: Noise
              tabPanel(
                title = "Noise",
                active = TRUE,
                mod_2024_ui("2024_1")  # Dynamically include the plots via the module
              ),
              # Tab 2: Placeholder for now
              tabPanel(title = "Tab2", "Content for Tab2"),
              # Tab 3: Placeholder for now
              tabPanel(title = "Tab3", "Content for Tab3")
            )
          )

        )
      )
    ),

    # Footer
    footer = bs4DashFooter(
      left = "Centinela Patagonia © 2025",
      right = "Powered by bs4Dash"
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ShinyAPP.CentinelaPatagonia"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
