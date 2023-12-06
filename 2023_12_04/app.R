

# LIBRARIES ----
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)


# MODULES ----
source("modules/module_tab_title.R")


# UI ----
ui <- tagList(
    useShinydashboard(),
    useShinyjs(),
    
    navbarPage(
        title = "My Shiny App",
        
        tabPanel(
            title = "Home", icon = icon("home", lib = "font-awesome")
        ),
        
        tabPanel(
            title = "Panel 1",
            
            module_tab_title_UI(id = "tab1_header"),
            
            module_wellPanel_input_UI(id = "tab1_input")
        ),
        
        tabPanel(
            title = "Panel 2",
            
            module_tab_title_UI(
                id = "tab2_header",
                title = "Tab 2 Title",
                subtitle = "Tab 2 Subtitle"
            ),
        )
    )
)


# SERVER ----
server <- function(input, output) {
    
    # Tab 1 Header
    module_tab_title_Server(id = "tab1_header")
    
    # Tab 1 Inputs
    module_wellPanel_input_UI(id = "tab1_input")
    
    # Tab 2 Header
    module_tab_title_Server(id = "tab2_header")
    

    
    
}


# RUN APP ----
shinyApp(ui, server)