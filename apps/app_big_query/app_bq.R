# BIG QUERY APP ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----

# * Libraries ----
library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

# *****************************************************************************
# **** ----
# DATA ----
# *****************************************************************************
# bq_projects_tbl <- tibble(
#     name = c(
#         "Mastering DBT",
#         "Fabulous Mart",
#         "Olist E-Commerce Database"
#     ),
#     id = c(
#         "mastering-dbt-394415",
#         "fabulous-mart",
#         "olist-e-commerce-database"
#     )
# )
# 
# project_id <- bq_projects_tbl[bq_projects_tbl$name == "Mastering DBT", ]$id
# 
# dataset_list <- bq_project_datasets(project_id) %>% 
#     map(
#         .f = function(x) {
#             x$dataset
#         }
#     ) %>% unlist()

# *****************************************************************************
# **** ----
# UI ----
# *****************************************************************************
ui <- dashboardPage(
    dashboardHeader(
        title = "Big Query App"
    ),
    
    # ---- Sidebar Panel ---- #
    dashboardSidebar(
        br(),
        
        # radioGroupButtons(
        #     inputId = "con",
        #     label = "Database Connection",
        #     choices = c("Open" = "open", "Closed" = "closed"),
        #     selected = "closed"
        # ),
        materialSwitch(
            inputId = "con",
            label = "Database Connection",
            status = "primary",
            right = FALSE
        ),
        
        hr(),
        
        pickerInput(
            inputId  = "project",
            label    = "Project",
            choices  = NULL,
            multiple = FALSE,
            selected = NULL
        ),
        
        hr(),
        
        pickerInput(
            inputId = "dataset",
            label = "Dataset",
            choices = NULL, selected = NULL, multiple = FALSE
        ),
        
        hr(),
        
        pickerInput(
          inputId = "tables",
          label = "Tables",
          choices = NULL, selected = NULL, multiple = FALSE
        ),
        
        br(), hr(), br(),
        
        div(
            style = "display: inline-block;",
            actionButton("apply", "Apply", icon = icon("play"), style = "display: inline-block;"),
            actionButton("reset", "Reset", icon = icon("sync"), style = "display: inline-block;")
        )
        
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        
        div(
            class = "page-header",
            h1("Output Section")
        ),
        
        hr(),
        
        h3("Connection:"),
        
        verbatimTextOutput("con"),
        uiOutput("connection_status"),
        
        hr(),
        
        h3("Sample Data:"),
        
        tableOutput("sample_data")
    )
)




# *****************************************************************************
# **** ----
# SERVER ----
# *****************************************************************************
server <- function(input, output) {
    
    # projects 
    project_list <- reactive({
        tibble(
            name = c(
                "Mastering DBT",
                "Fabulous Mart",
                "Olist E-Commerce Database"
            ),
            id = c(
                "mastering-dbt-394415",
                "fabulous-mart",
                "olist-e-commerce-database"
            )
        )
    })
    
    # datasets
    datasets_reactive <- reactive({
        
        if (is.null(input$project)) {
            return(NULL) # Return NULL if input$project is NULL
        }
        
        project_id <- project_list()[project_list()$name == input$project, ]$id
        dataset_list <- bq_project_datasets(project_id) %>%
            map(
                .f = function(x) {
                    x$dataset
                }
            ) %>% unlist()
        dataset_list
    })

    # update inputs
    observe({
        
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "project", # Added inputId
            choices = unique(project_list()$name),
            selected = unique(project_list()$name[1])
        )
        
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "dataset",
            choices = unique(datasets_reactive()),
            selected = unique(datasets_reactive())[1]
        )
        
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "tables",
            choices = unique(tables_reactive()),
            selected = unique(tables_reactive())[1]
        )
     
    })
    
    # connection
    con_reactive <- reactive({
        # Only proceed if the apply button has been clicked at least once
        #req(input$apply)
        req(input$con == TRUE)
        
        if (is.null(input$project) || is.null(input$dataset)) {
            return(NULL)
        }
        
        DBI::dbConnect(
            bigrquery::bigquery(),
            project = project_list()[project_list()$name == input$project, ]$id,
            dataset = input$dataset,
            billing = project_list()[project_list()$name == input$project, ]$id
        )
    })
    
    observe({
        if (input$con == FALSE && !is.null(con_reactive())) {
            dbDisconnect(con_reactive())
        }
    })
    
    # connection message
    output$connection_status <- renderUI({
        msg_closed = str_glue("Connection closed! 
                              Toggle the Database Connection to establish connection to Big Query")
        
        msg_open = str_glue("Connection Open! 
                             Project: {input$project}, Dataset: {input$dataset}")
        
        if (input$con == TRUE) {
            # Connection is open
            tags$div(
                tags$span(style = "color: green;", icon("check")),
                tags$span(msg_open)
            )
        } else {
            # Connection is closed
            tags$div(
                tags$span(style = "color: red;", icon("times")),
                tags$span(msg_closed)
            )
        }
    })
    
    
  
    
    # tables
    tables_reactive <- reactive({

        req(con_reactive())
   
        DBI::dbListTables(con_reactive())
        
    })
    
    
    # reset button
    observeEvent(input$reset, {
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "project", # Added inputId
            choices = unique(project_list()$name),
            selected = unique(project_list()$name[1])
        )
        
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "dataset",
            choices = unique(datasets_reactive()),
            selected = unique(datasets_reactive())[1]
        )
        
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "tables",
            choices = unique(tables_reactive()),
            selected = unique(tables_reactive())[1]
        )
        
        shinyjs::delay(ms = 300, expr = {
            shinyjs::click(id = "apply")
        })
    })
    
    
    # print connection string
    # output$con <- renderPrint({
    #     if (is.null(con_reactive())) {
    #         return("No connection established.")
    #     }
    #     paste("Connected to project:", input$project, "and dataset:", input$dataset)
    # })
    
    # display sample data
    selected_tbl <- reactive({
        req(input$apply)
        if (is.null(con_reactive()) || is.null(input$tables)) {
            return(NULL)
        }
        
        dplyr::tbl(con_reactive(), input$tables) %>% 
            head(5) 
    })
    
    output$sample_data <- renderTable({
        selected_tbl()
    })
}

shinyApp(ui, server)

# # * View ----
# stg_products_tbl <- dplyr::tbl(con, "stg_ecommerce__products")
# 
# schedules_tbl %>% glimpse()
# 
# 
# # * Collect ----
# product_ids <- select(stg_products_tbl, product_id) %>% 
#     distinct() %>% 
#     collect()




# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
