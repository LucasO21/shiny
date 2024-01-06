

# LIBRARIES ----
library(tidyverse)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyjs)
library(bslib)
library(shinyBS)
library(htmlwidgets)
library(shinyWidgets)
library(shinythemes)

# MODULES ----
table <- "mtcars"

# UI ----
ui <- tagList(
    useShinydashboard(),
    shinyjs::useShinyjs(),
    
    navbarPage(
        title = "My Shiny App",
        theme = shinytheme("cerulean"),
        
        tabPanel(
            title = "Home", icon = icon("home", lib = "font-awesome")
        ),
        
        tabPanel(
            title = "Panel 1",
            
            # * Tab Header ----
            fluidRow(
                column(
                    width = 12,
                    div(
                        class = "page-header",
                        style = "margin-left: 50px; margin-right: 50px;",
                        h1("Tab 1 Header"),
                        h4("Tab 1 Subheader")
                    )
                )
            ),
            
            # * Tab Inputs/DB Preview ----
            fluidRow(
                column(
                    width = 12,
                    div(
                        style = "margin-left: 50px; margin-right: 50px;",
                        wellPanel(
                         fluidRow(
                             div(
                                 actionButton("toggle-main", "Toggle Inputs", icon = icon("caret-down")),
                             ),
                             br(),
                             div(
                                 id = "inputs",
                                 style = "margin-left: 50px; margin-right: 50px;",
                                 
                                 # ** DB Preview ----
                                 div(
                                     style = "border: 1px solid black; padding: 10px;",
                                     fluidRow(
                                         div(
                                             div(
                                                 style = "margin-left: 15px;",
                                                 actionButton("toggle-db", "Toggle DB Preview", icon = icon("database")),
                                             ),
                                             br(),
                                             div(
                                                 id = "db-preview",
                                                 div(
                                                     class = "col-md-4",
                                                     box(
                                                         title = "Select/Load Table",
                                                         status = "primary",
                                                         solidHeader = FALSE,
                                                         collapsible = TRUE,
                                                         width = NULL,
                                                         height = "300px",
                                                         shiny::selectInput(
                                                             inputId  = "db-preview-table",
                                                             label    = "Select Table",
                                                             choices  = c("mtcars", "oj", "titanic"), 
                                                             selected = "mtcars", 
                                                             multiple = FALSE,
                                                             width    = "50%"
                                                         ),
                                                         hr(),
                                                         actionButton("load-table", "Load Table", icon = icon("download")),
                                                         br(), 
                                                         footer = fluidRow(
                                                             column(
                                                                 width = 12,
                                                                 # descriptionBlock(
                                                                 #     header = uiOutput("con"),
                                                                 #     text = HTML(str_glue("<span class='text-red'>{table}</span>")),
                                                                 #     #rightBorder = TRUE,
                                                                 #     marginBottom = FALSE
                                                                 # )
                                                                 uiOutput("con")
                                                             )
                                                         )
                                                     ),
                                                     br()
                                                 ),
                                                 div(
                                                     class = "col-md-4",
                                                     box(
                                                         title = "Table Preview",
                                                         status = "primary",
                                                         solidHeader = FALSE,
                                                         collapsible = TRUE,
                                                         width = 12,
                                                         height = "300px",
                                                         tableOutput("head_mtcars")
                                                     )
                                                 )
                                             ) %>% shinyjs::hidden()
                                         )
                                     )
                                 ),
                                 
                                 # * Inputs ----
                                 div(
                                     style = "border: 1px solid black; padding: 10px;",
                                     fluidRow(
                                         div(
                                             div(
                                                 style = "margin-left: 15px;",
                                                 actionButton("toggle-inputs", "Toggle Inputs", icon = icon("caret-down")),
                                             ),
                                             div(
                                                 id = "sub-inputs",
                                                 "Test"
                                             ) %>% shinyjs::hidden()
                                         )
                                     )
                                 )
                                 
                             ) %>% shinyjs::hidden()
                          )
                        )
                    )
                )
            )
        )
    )
)


# SERVER ----
server <- function(input, output) {
    
    # * Main Toggle ----
    shinyjs::onclick(id = "toggle-main", {
        shinyjs::toggle(id = "inputs", anim = TRUE, animType = "slide")
    })
    
    # * DB Preview Toggle ----
    shinyjs::onclick(id = "toggle-db", {
        shinyjs::toggle(id = "db-preview", anim = TRUE, animType = "slide")
    })
    
    # * Inputs Toggle ----
    shinyjs::onclick(id = "toggle-inputs", {
        shinyjs::toggle(id = "sub-inputs", anim = TRUE, animType = "slide")
    })
    
    
    # * Data ----
    mtcars_tbl <- reactive({
        mtcars %>% 
            rownames_to_column("car") %>% 
            mutate(car = as.character(car))
    })
    
    output$head_mtcars <- renderTable({
        mtcars_tbl() %>% head() %>% select(-car)
    })
    
    output$table_info <- renderText({
   
    })
    
    
    # * DB Connection Checkmark ----
    output$con <- renderUI({
        descriptionBlock(
            HTML(paste("DB Connection", tags$span(class = "glyphicon glyphicon-remove", style = "color: red;"))),
            text = HTML(str_glue("<span class='text-red'>No Table Loaded</span>")),
            marginBottom = FALSE
        )
    })
    
    observeEvent(eventExpr = input[["load-table"]], handlerExpr = {
        output$con <- renderUI({
            descriptionBlock(
                HTML(paste("DB Connection", tags$span(class = "glyphicon glyphicon-ok", style = "color: green;"))),
                text = HTML(str_glue("Table Name: <span class='text-green'>{table}</span>")),
                marginBottom = FALSE
            )
        })
    })
    
    
    
    
}


# RUN APP ----
shinyApp(ui, server)