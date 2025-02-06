#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny leaflet
#' @noRd

library(leaflet)
library(pryr)

app_server <- function(input, output, session) {


  # Upload Environment.RData file
  local({
    # print("Before cleanup:")
    # print(ls(.GlobalEnv))

    rm(list = ls(.GlobalEnv), envir = .GlobalEnv)

    # print("After cleanup:")
    # print(ls(.GlobalEnv))


    env <- new.env()
    load(app_sys("R/Environment.RData"), envir = env)
    for (name in ls(env)) {
      assign(name, get(name, envir = env), envir = .GlobalEnv)
    }

    # To see how much memory the app uses
    cat("Memory used when the app is running in mod_FH23:", pryr::mem_used(), "bytes\n")
  })


  ##########################################
  # Reactive value to track the active tab #
  ##########################################
  {
  active_tab <- reactiveVal("tab1")
  active_year <- reactiveVal("2024")
  }

  ############################################
  # Update the active values based on clicks #
  ############################################
  { # update tab
  observeEvent(input$tab1, {
    active_tab("tab1")
  })
  observeEvent(input$tab2, {
    active_tab("tab2")
  })
  observeEvent(input$tab3, {
    active_tab("tab3")
  })
  # update year
  observeEvent(input$year, {
    active_year(input$year)
  })

  }

  ######################################################################
  # Render dynamic content based on the active tab AND the active year #
  ######################################################################
  {
  output$dynamic_content <- renderUI({
    # Check the year
    ### 2023 ###
    if (active_year() == "2023") {
      # Check the active tab
      if (active_tab() == "tab1") {
        h3("Content for Tab 1 and 2023")
      } else if (active_tab() == "tab2") {
        h3("Content for Tab 2 and 2023")
      } else if (active_tab() == "tab3") {
        h3("Content for Tab 3 and 2023")
      }
    }
    ### 2024 ###
    else if (active_year() == "2024") {
      # Check the active tab
      if (active_tab() == "tab1") {
        mod_2024_Tab1_ui("2024_Tab1_1")
      } else if (active_tab() == "tab2") {
        mod_2024_Tab2_ui("2024_Tab2_1")
      } else if (active_tab() == "tab3") {
        h3("Content for Tab 3 and 2024")
      }
    }
  })

  }


  ######################################################################
  # Call the various module's servers
  mod_2024_Tab1_server("2024_Tab1_1")
  mod_2024_Tab2_server("2024_Tab2_1")
  ######################################################################




  # Reactive to handle the selected year
  observeEvent(input$campaign_selector, {
    if (input$campaign_selector == "2024") {
      mod_map_2024_server("map_2024_1")   # Call the map module for 2024
      mod_2024_server("2024_1")          # Call the tab module for 2024
    }
    # Add handling for 2023 if needed
  })

  # Descriptive stats
  output$descriptive_stats <- renderText({
    "Just some space to put the descriptive statistics for 2024"
  })
}




