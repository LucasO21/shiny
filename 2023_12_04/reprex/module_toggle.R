

# LIBRARIES ----
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)


# MODULES ----
module_wellPanel_input_UI <- function(id) {
    
    # NS Setup
    ns <- NS(id)
    
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
                                    )
                                )
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
        #shinyjs::useShinyjs(),
        id,
        function(input, output, session) {
            
            shinyjs::onclick(id = "toggle", {
                shinyjs::toggle(id = "inputs", anim = TRUE, animType = "slide")
            })
            
        }
    )
}



# APP UI ----
ui <- tagList(
    useShinydashboard(),
    shinyjs::useShinyjs(),
    
    navbarPage(
        title = "My Shiny App",
        
        tabPanel(
            title = "Home", icon = icon("home", lib = "font-awesome")
        ),
        
        tabPanel(
            title = "Panel 1",
            
            module_wellPanel_input_UI(id = "tab1_inputs")
        )
    )
)


# APP SERVER ----
server <- function(input, output) {
    
    # Tab 1 Inputs
    module_wellPanel_input_Server(id = "tab1_inputs")
    
    # shinyjs::onclick(id = "tab1_inputs-toggle", {
    #     shinyjs::toggle(id = "tab1_inputs-inputs", anim = TRUE, animType = "slide")
    # })
    
}


# RUN APP ----
shinyApp(ui, server)