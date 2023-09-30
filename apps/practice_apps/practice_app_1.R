

# Packages ----
source(file = "../global_functions/load_packages.R")
get_working_directory("apps")
get_packages()

# Modules ----
input_UI <- function(id) {
  ns <- NS(id)
  tagList(
      textInput(ns("paid_new_campaign_list"), "Campaign List", NULL),
      textInput(ns("paid_new_clicks"), "Clicks", NULL),
      textInput(ns("paid_new_leads"), "Leads", NULL),
      textInput(ns("paid_new_cost"), "Cost", NULL)
  )
}

input_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
        
        # default inputs
        default_inputs <- reactive({get_inputs()})
        
        # update inputs with defaults
        observe({
            updateTextInput(session, "paid_new_campaign_list", value = paste(default_inputs()$paid_new_campaign_list, collapse = ", "))
            updateTextInput(session, "paid_new_clicks", value = paste(default_inputs()$paid_new_clicks, collapse = ", "))
            updateTextInput(session, "paid_new_leads", value = paste(default_inputs()$paid_new_leads, collapse = ", "))
            updateTextInput(session, "paid_new_cost", value = paste(default_inputs()$paid_new_cost, collapse = ", "))
        })
      
    }
  )
}

# App Function ----
practice_app_1 <- function(filter = is.numeric) {
    ui <- fluidPage(
        sidebarLayout(
            sidebarPanel(input_UI("input")),
            mainPanel()
        )
    )
    server <- function(input, output, session) {
        input_Server("input")
        #output$out <- renderPrint(var(), width = 40)
    }
    shinyApp(ui, server)
}

practice_app_1()
