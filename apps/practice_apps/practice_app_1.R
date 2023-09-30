

# Packages ----
source(file = "../global_functions/load_packages.R")
source(file = "functions/default_inputs.R")
get_working_directory("apps")
get_packages()

# Modules ----
input_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("paid_new_campaign_list"), "Campaign List", NULL),
    textInput(ns("paid_new_clicks"), "Clicks", NULL),
    textInput(ns("paid_new_leads"), "Leads", NULL),
    textInput(ns("paid_new_cost"), "Cost", NULL),
    textInput(ns("multiply"), "Multiply", NULL),
    hr(),
    actionButton(ns("apply"), "Apply")
  )
}

input_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # default inputs
      default_inputs <- reactive({get_inputs()})
      
      # data
      # campaign_tbl <- reactive({read_rds("practice_apps/data/campaign_data.rds")})
      
      # update inputs with defaults
      observe({
        updateTextInput(session, "paid_new_campaign_list", value = paste(default_inputs()$paid_new_campaign_list, collapse = ", "))
        updateTextInput(session, "paid_new_clicks", value = paste(default_inputs()$paid_new_clicks, collapse = ", "))
        updateTextInput(session, "paid_new_leads", value = paste(default_inputs()$paid_new_leads, collapse = ", "))
        updateTextInput(session, "paid_new_cost", value = paste(default_inputs()$paid_new_cost, collapse = ", "))
        updateTextInput(session, "multiply", value = paste(default_inputs()$multiply, collapse = ", "))
      })
      
      # apply button
      updated_inputs <- eventReactive(input[["apply"]], {
        list(
          campaign_list = stringr::str_split(input[["paid_new_campaign_list"]], ",")[[1]],
          clicks = as.numeric(unlist(str_split(input[["paid_new_clicks"]], ","))),
          multiply = as.numeric(unlist(str_split(input[["multiply"]], ",")))
        
        )
      })
      
      # print updated inputs
      output$default_list <- renderPrint({default_inputs()$paid_new_campaign_list})
      output$updated_list <- renderPrint({ updated_inputs()$campaign_list })
      output$clicks <- renderPrint({ updated_inputs()$clicks })
      output$calc <- renderTable({
        tibble(
          campaign = updated_inputs()$campaign_list,
          clicks   = updated_inputs()$clicks,
          multiply = updated_inputs()$multiply
        ) %>% 
          mutate(res = clicks * multiply)
      })
      
      # return
      return(updated_inputs)
      
    }
  )
}

combined_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tableOutput(ns("calculation_table"))
  )
}

data_UI <- function(id) {
  ns <- NS(id)
  tableOutput(ns("campaign_data"))
}


combined_Server <- function(id, updated_inputs) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # data
      campaign_tbl <- reactive({read_rds("data/campaign_data.rds")})
      
      # calculation table
      output$calculation_table <- renderTable({
        tibble(
          campaign = updated_inputs()$campaign_list,
          clicks   = updated_inputs()$clicks
        )
      })
      
      # calculation from data
      output$campaign_data <- renderTable({
        # campaign_tbl() %>% head()
        get_ppc_metrics(
          campaign_tbl(), NULL, "campaign_total"
        )
      })
      
    }
  )
}




# App ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(input_UI("input")),
    mainPanel(
      p("Default Inputs"),
      textOutput("input-default_list"),
      hr(),
      p("Updated Inputs"),
      textOutput("input-updated_list"),
      textOutput("input-clicks"),
      br(),
      tableOutput("input-calc"),
      hr(),
      p("Calculations"),
      combined_UI("combined"),
      hr(),
      p("Calculations From Data:"),
      data_UI("campaign_data")
    )
  )
)
server <- function(input, output, session) {
  
  updated_inputs <- input_Server("input")
  
  combined_Server("combined", updated_inputs)
  
  combined_Server("campaign_data", updated_inputs)
}

shinyApp(ui, server)




