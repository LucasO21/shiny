# BIG QUERY APP ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----

# * Libraries ----
library(tidyverse)
library(janitor)
library(bigrquery)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

# * Auth ----
# authenticate
bq_auth(path = "../../json/")

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
            label   = "Database Connection",
            status  = "primary",
            right   = FALSE
        ),
        
        hr(),
        
        pickerInput(
            inputId = "project",
            label   = "Project",
            choices = NULL, multiple = FALSE, selected = NULL
        ),
        
        hr(),
        
        pickerInput(
            inputId = "dataset",
            label   = "Dataset",
            choices = NULL, selected = NULL, multiple = FALSE
        ),
        
        hr(),
        
        pickerInput(
          inputId = "tables",
          label   = "Tables",
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
        
        tableOutput("sample_data"),
        
        verbatimTextOutput("id"),
        verbatimTextOutput("datasets"),
        verbatimTextOutput("tables")
    )
)




# *****************************************************************************
# **** ----
# SERVER ----
# *****************************************************************************
server <- function(input, output) {
    
    # projects 
    project_tbl <- reactive({
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
    
    # update projects picker input
    observe({
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "project", 
            choices  = unique(project_tbl()$name),
            selected = unique(project_tbl()$name[1])
        )
    })
   
    # datasets
    datasets_list <- reactive({

        # require apply button
        #req(input$apply)

        if (is.null(input$project)) {
            return(NULL) # Return NULL if input$project is NULL
        }

        id <- project_tbl()[project_tbl()$name == input$project, ]$id

        dataset_list <- bq_project_datasets(id) %>%
            map(
                .f = function(x) {
                    x$dataset
                }
            ) %>% unlist()
        
        dataset_list
    })

    # update dataset picker inputs
    observeEvent(input$project, {
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "dataset",
            choices  = unique(datasets_list()),
            selected = unique(datasets_list())[1]
        )
    })
    
    # connection open
    con <- reactive({
        
        req(input$con == TRUE)

        if (is.null(input$project) || is.null(input$dataset)) {
            return(NULL)
        }

        DBI::dbConnect(
            bigrquery::bigquery(),
            project = project_tbl()[project_tbl()$name == input$project, ]$id,
            dataset = input$dataset,
            billing = project_tbl()[project_tbl()$name == input$project, ]$id
        )
    })
    
    # connection closed
    observe({
        if (input$con == FALSE && !is.null(con())) {
            dbDisconnect(con())
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
    tables_list <- reactive({
        req(con())
        tryCatch({
            DBI::dbListTables(con())
        }, error = function(e) {
            return(NULL)
        })
    })

    
    # update tables picker inputs
    # observeEvent(input$dataset, {
    #     updatePickerInput(
    #         session  = getDefaultReactiveDomain(),
    #         inputId  = "tables",
    #         choices  = unique(tables_list()),
    #         selected = unique(tables_list())[1]
    #     )
    # })
    
    observe({
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "tables",
            choices  = unique(tables_list()),
            selected = unique(tables_list())[1]
        )
    })
    
    id <- reactive({project_tbl()[project_tbl()$name == input$project, ]$id})

    output$id <- renderPrint({id()})
    output$datasets <- renderPrint({datasets_list()})
    output$tables <- renderPrint({tables_list()})


    # reset button
    observeEvent(input$reset, {

        #updateActionButton(session, "apply", value = 0)

        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "project", # Added inputId
            choices = unique(project_tbl()$name),
            selected = unique(project_tbl()$name[1])
        )

        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "dataset",
            choices = unique(datasets_list()),
            selected = unique(datasets_list())[1]
        )

        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "tables",
            choices = unique(tables_list()),
            selected = unique(tables_list())[1]
        )

        shinyjs::delay(ms = 300, expr = {
            shinyjs::click(id = "apply")
        })
    })

    # display sample data
    # selected_tbl <- reactive({
    #     req(input$apply)
    #     if (is.null(con()) || is.null(input$tables)) {
    #         return(NULL)
    #     }
    # 
    #     dplyr::tbl(con(), input$tables) %>% head(5)
    # })
    
    selected_tbl <- eventReactive(eventExpr = input$apply, {
        if (is.null(con()) || is.null(input$tables)) {
                    return(NULL)
                }

                dplyr::tbl(con(), input$tables) %>% head(5)
    }) 

    output$sample_data <- renderTable({

        if (input$apply == 0) {
            return(data.frame(Message = "Click 'Apply' to view sample data"))
        }

        selected_tbl()
    })
}

shinyApp(ui, server)


