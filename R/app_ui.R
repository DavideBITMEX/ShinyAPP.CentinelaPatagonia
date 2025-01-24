#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(golem)
library(shiny)

app_ui <- function(request) {
  tagList(
    # Add external resources
    golem_add_external_resources(),

    # Application UI
    fluidPage(
      titlePanel("Simple Golem Shiny App"),

      sidebarLayout(
        sidebarPanel(
          actionButton("btn_counter", "Click me!")
        ),

        mainPanel(
          h3("Counter:"),
          textOutput("counter")
        )
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
    # for example, you can add shinyalert::useShinyalert()
  )
}
