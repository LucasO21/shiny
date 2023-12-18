

# Libraries ----
library(tidyverse)
library(timetk)
library(highcharter)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)

# Source ----
source("modules/module_value_box.R")
source("functions/custom_valuebox.R")
source("functions/custom_hc_theme.R")

# UI ----
ui <- tagList(
    useShinydashboard(),
    useShinyjs(),
    
    # Include custom CSS for box shadow
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    navbarPage(
        title = "Shiny App",
        
        tabPanel(
            title = "Panel 1",
            # Add a custom class to the fluidRow for applying the shadow
           div(
               class = "shadow-row", style = "padding-left: 0px; margin-left: 0px;",
               fluidRow(
                   column(
                       width = 10, offset = 1,
                       
                       module_vb_UI("vb1")
                       
                       # value_box_custom(
                       #     value = "1,456",
                       #     title = "Sales",
                       #     sparkobj = hc,
                       #     subtitle = tagList(HTML("&uarr;"), "25% Since Last Day"),
                       #     info = "Some info text",
                       #     info_icon = TRUE,
                       #     icon = icon("code"),
                       #     width = 4,
                       #     color = "green",
                       #     href = NULL
                       # )
                   )
               )
           )
        )
    )
)

# SERVER ----
server <- function(input, output) {
    
    # Data 
    df <- reactive({
        timetk::m4_daily %>% 
            filter(id == "D10") %>% 
            arrange(date) %>% 
            head(100)
    })

    hc <- reactive({
        hchart(df(), "line", hcaes(date, value), name = "Daily Sales") %>%
            hc_size(height = 100) %>%
            hc_add_theme(hc_theme_sparkline_vb()) %>%
            hc_credits(enable = FALSE)
    })
    
    
    module_vb_Server("vb1", sparkobj = hc)
    
}

# RUN APP ----
shinyApp(ui, server)