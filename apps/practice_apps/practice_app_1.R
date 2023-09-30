

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
      
      # update inputs with defaults
      observe({
        updateTextInput(session, "paid_new_campaign_list", value = paste(default_inputs()$paid_new_campaign_list, collapse = ", "))
        updateTextInput(session, "paid_new_clicks", value = paste(default_inputs()$paid_new_clicks, collapse = ", "))
        updateTextInput(session, "paid_new_leads", value = paste(default_inputs()$paid_new_leads, collapse = ", "))
        updateTextInput(session, "paid_new_cost", value = paste(default_inputs()$paid_new_cost, collapse = ", "))
      })
      
      # apply button
      updated_inputs <- eventReactive(input[["apply"]], {
        list(
          campaign_list = stringr::str_split(input[["paid_new_campaign_list"]], ", ")[[1]]
        )
      })
      
      # print updated inputs
      output$default_list <- renderPrint({default_inputs()$paid_new_campaign_list})
      output$updated_list <- renderPrint({ updated_inputs()$campaign_list })
      
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
      textOutput("input-updated_list")
    )
  )
)
server <- function(input, output, session) {
  input_Server("input")
  #output$out <- renderPrint(var(), width = 40)
}

shinyApp(ui, server)

