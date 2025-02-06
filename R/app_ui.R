#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(golem)
library(shiny)
# devtools::install_github("RinteRface/tablerDash")
library(tablerDash)
library(bs4Dash)
library(bslib)


app_ui <- function(request) {
  tagList(
    # External resources (CSS, JS, etc.)
    golem_add_external_resources(),

    tags$div(
      style = "background-color: #007bff; color: white; padding: -10px; text-align: center;
           font-size: 24px; font-weight: bold; height: 100px;",
      tags$img(src = "www/Centinela_logo.png", style = "height: 110px;")
    )
    ,

    bs4DashPage(
      fullscreen = TRUE,
      ########################
      ### Dashboard Header ###
      ########################
      header = bs4DashNavbar(
        title = NULL,  # No title
        skin = "primary", # Navbar theme
        border = TRUE,
        # Flexbox row in the header to combine selectInput and tabs
        tags$div(
          style = "display: flex; align-items: center; gap: 15px;",
          # SelectInput
          tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$span("Choose Year:", style = "color: white; font-weight: bold;"), # Label
            tags$div( style = "margin-top: 15px;",
              selectInput(
                inputId = "year",
                label = NULL,
                choices = c("2023", "2024"),
                selected = "2024",
                width = "120px"
              )
            )
          ),
          # Tabs
          tags$div(
            style = "display: flex; gap: 15px;",
            actionLink(inputId = "tab1", "Noise Overview", class = "nav-link", style = "color: white; text-decoration: none;"),
            actionLink(inputId = "tab2", "Day/Night Analysis", class = "nav-link", style = "color: white; text-decoration: none;"),
            actionLink(inputId = "tab3", "Types of sounds", class = "nav-link", style = "color: white; text-decoration: none;")
          )
        )
      ),

      ########################
      ### Dashboard Sidebar ##
      ########################
      sidebar = bs4DashSidebar(disable = TRUE), # Disable sidebar for simplicity



      ########################
      #### Dashboard Body ###
      ########################
      body = bs4DashBody(
        uiOutput("dynamic_content")
      )
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
    # For example, you can add shinyalert::useShinyalert()
  )
}

