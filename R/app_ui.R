#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

if (FALSE) {
  library(curl)
  library(CentinelaPatagonia)  # Your custom package, if needed
  # And any other packages that are not auto-detected
}

library(golem)
library(shiny)
library(tablerDash)
library(bs4Dash)
library(bslib)
library(waiter)
library(fresh)

Theme <- create_theme(
  bs4dash_status(
    primary = "#111184",
    secondary = "#1ABC9C",
    light = "#FFFFFF"
  )
)

app_ui <- function(request) {
  tagList(
    # External resources (CSS, JS, etc.)
    golem_add_external_resources(),
    use_theme(Theme),

    # Custom CSS for fixed footer, navbar alignment, enhanced nav links, and fullscreen icon size
    tags$head(
      tags$style(HTML("
         /* Fixed footer styling */
         .fixed-footer {
           position: fixed;
           bottom: 0;
           left: 0;
           width: 100%;
           margin: 0;
           z-index: 9999;
         }
         /* Force the entire bs4Dash navbar to bottom-align items */
         .main-header .navbar {
           display: flex !important;
           align-items: flex-end !important;
         }
         .main-header .navbar-nav {
           align-items: flex-end !important;
         }
         /* Enhanced nav links styling */
         .nav-link {
           color: white !important;
           font-size: 20px !important;
           padding: 10px 16px;
           border-radius: 4px;
           cursor: pointer;
           transition: background-color 0.3s ease, box-shadow 0.3s ease, transform 0.2s ease;
         }
         .nav-link:hover {
           background-color: rgba(255,255,255,0.25);
           box-shadow: 0 2px 6px rgba(0,0,0,0.3);
           transform: scale(1.02);
           text-decoration: underline;
         }
         .nav-link:active {
           background-color: rgba(255,255,255,0.3);
           transform: scale(0.98);
         }
         .nav-link:focus {
           outline: 2px solid white;
           outline-offset: 2px;
         }
         /* Increase the fullscreen icon size */
         .navbar-nav > li > a[data-widget='fullscreen'] i {
           font-size: 16px !important;
           margin-right: 10px;
         }
      "))
    ),

    bs4DashPage(
      fullscreen = TRUE,
      dark = NULL,
      help = NULL,  # No built-in help icon

      ########################
      ### Dashboard Header ###
      ########################
      header = bs4DashNavbar(
        skin = "primary",
        border = TRUE,

        # Left side: Logo and Year selector column
        title = tags$div(
          style = "display: flex; align-items: flex-start;",
          # Logo
          tags$img(
            src = "www/Centinela_logo.png",
            style = "height:120px; margin-right:10px;"
          ),
          # Vertical column for "Choose Year:" and selectInput
          tags$div(
            style = "display: flex; flex-direction: column; justify-content: flex-end; margin-top: 20px;",
            tags$span("Choose Year:", style = "color:white; font-weight:bold; font-size:20px;"),
            selectInput(
              inputId = "year",
              label = NULL,
              choices = c("2023", "2024"),
              selected = "2024",
              width = "100px",
              selectize = FALSE
            )
          )
        ),

        # Center: Tab links with enhanced professional styling
        tags$div(
          style = "display: flex; align-items: flex-end; gap: 15px; font-size:20px;",
          actionLink(
            inputId = "tab1",
            "Noise Overview",
            class = "nav-link",
            style = "color:white;"
          ),
          actionLink(
            inputId = "tab2",
            "Day/Night Analysis",
            class = "nav-link",
            style = "color:white;"
          ),
          actionLink(
            inputId = "tab3",
            "Types of sounds",
            class = "nav-link",
            style = "color:white;"
          )
        )

      ),

      ########################
      ### Dashboard Sidebar ##
      ########################
      sidebar = bs4DashSidebar(disable = TRUE),

      ########################
      #### Dashboard Body ####
      ########################
      body = bs4DashBody(
        uiOutput("dynamic_content")
      )
    ),

    # Custom footer outside bs4DashPage
    tags$footer(
      class = "fixed-footer",
      style = "background-color:#111184; color:white; padding:10px 20px; display:flex; justify-content:space-between; align-items:center;",
      tags$div(
        "© 2025 Centinela Patagonia - Made with ",
        tags$img(src = "www/Rlogo.png", style = "height:20px; vertical-align:middle;"),
        " by Davide Bittelli"
      ),
      tags$div(
        "GitHub Repository: ",
        tags$a(
          href = "https://github.com/DavideBITMEX/ShinyAPP.CentinelaPatagonia",  # Replace with your GitHub URL
          target = "_blank",
          tags$img(src = "www/github_logo.png", style = "height:20px; vertical-align:middle;")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external resources inside the Shiny application.
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
    favicon(
      ico = "favicon",
      rel = "shortcut icon",
      resources_path = "www",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ShinyAPP.CentinelaPatagonia"
    ),
    waiter::useWaiter()
  )
}
