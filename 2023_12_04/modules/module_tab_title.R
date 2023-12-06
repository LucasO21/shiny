# UI DESIGN MODULES ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("2023_12_04", "modules"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(shinyjs)

# * Data View ----
# data_sample_tbl <- read_csv("data/online_retail_II.csv") %>% head(5)


# *****************************************************************************
# **** ----
# TAB HEADER ----
# *****************************************************************************
module_tab_title_UI <- function(id, 
                                title = "Tabset 1 Title",
                                subtitle = "Tabset 1 Subtitle...",
                                col_width = 10,
                                col_offset = 1) {
    ns <- NS(id)
    
    fluidRow(
        column(
            width = col_width, offset = col_offset,
            div(
                class = "page-header",
                h1(title),
                h3(subtitle),
                actionButton(ns("info"), "More Info", icon = icon("circle-info"))
            )
        )
    )
}


module_tab_title_Server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        observeEvent(eventExpr = input[["info"]], handlerExpr = {
            showModal(
                modalDialog(
                    title = h3("Tab1 Additional Info"),
                    "Here is some additional information",
                    
                    easyClose = TRUE,
                    footer = modalButton("Close/Esc")
                )
            )
        })
        
    })
}

# *****************************************************************************
# **** ----
# WELL PANEL INPUTS (HORIZONTAL) ----
# *****************************************************************************

module_wellPanel_input_UI <- function(id) {
    
    # NS Setup
    ns <- NS(id)
    
    # Widgets
    #action_toggle <- actionButton(ns("toggle_tab1"), "Toggle Inputs", icon = icon("toggle-on"))
    
  # Taglist    
  tagList(
      shinyjs::useShinyjs(),
      
      fluidRow(
          column(
              width = 10, offset = 1,
              wellPanel(
                  fluidRow(
                      div(
                          actionButton(ns("toggle"), "Toggle Inputs", icon = icon("toggle-on"))
                      ),
                      br(),
                      div(
                          id = ns("inputs"),
                          div(
                              class = "row",
                              div(
                                  div(
                                      class = "col-md-2",
                                      pickerInput(
                                          inputId  = ns("customer_id"),
                                          label    = "Customer ID",
                                          choices  = NULL,
                                          multiple = TRUE,
                                          selected = NULL,
                                          options  = list(
                                              `actions-box` = TRUE,
                                              `deselect-all-text` = "Deselect All",
                                              `select-all-text` = "Select All",
                                              `none-selected-text` = "Nothing Selected",
                                              `selected-text-format` = "count > 1"
                                          )
                                      )
                                  ),
                                  div(
                                      class = "col-md-2",
                                      pickerInput(
                                          inputId  = ns("cohort_id"),
                                          label    = "Purchase Cohort",
                                          choices  = NULL,
                                          multiple = TRUE,
                                          selected = NULL,
                                          options  = list(
                                              `actions-box` = TRUE,
                                              `deselect-all-text` = "Deselect All",
                                              `select-all-text` = "Select All",
                                              `none-selected-text` = "Nothing Selected",
                                              `selected-text-format` = "count > 1"
                                          )
                                      )
                                  ),
                                  div(
                                      class = "col-md-2",
                                      dateRangeInput(
                                          inputId = ns("date_info"),
                                          label   = "Invoice Date Range",
                                          start   = NULL,
                                          end     = NULL,
                                          min     = NULL,
                                          max     = NULL
                                      )
                                      
                                  ),
                                  div(
                                      class = "col-md-2",
                                      pickerInput(
                                          inputId  = ns("country_id"),
                                          label    = "Country",
                                          choices  = NULL,
                                          multiple = TRUE,
                                          selected = NULL,
                                          options  = list(
                                              `actions-box` = TRUE,
                                              `deselect-all-text` = "Deselect All",
                                              `select-all-text` = "Select All",
                                              `none-selected-text` = "Nothing Selected",
                                              `selected-text-format` = "count > 1"
                                          )
                                      )
                                  )
                              )
                          ),
                          div(
                              actionButton(ns("apply"), "Apply", icon = icon("play"), width = "140px"),
                              actionButton(ns("reset"), "Reset", icon = icon("sync"), width = "140px"),
                              downloadButton(ns("download_data"), "Download Data", icon = icon("download"), width = "140px"),
                              actionButton(ns("mtd"), "Metadata", icon = icon("info-circle"), width = "140px")
                          )
                      ) %>% shinyjs::hidden()
                  )
              )
          )
      )
  )
}

module_wellPanel_input_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
        
        shinyjs::onclick(id = "toggle", {
            shinyjs::toggle(id = "inputs", anim = TRUE, animType = "slide")
        })
        
        # observeEvent(eventExpr = input[["toggle"]], handlerExpr = {
        #     shinyjs::toggle(sessions$ns
        #                     ("inputs"))
        # })
      
    }
  )
}






# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
