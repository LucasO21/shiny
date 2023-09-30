# MODULES ----
# *** ----

# Examples from https://mastering-shiny.org/scaling-modules.html

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Source Setup ----
source(file = "global_functions/load_packages.R")

# * Working Dir Setup ----
get_working_directory("apps")

# * Package Setup ----
get_packages()

# *****************************************************************************
# **** ----
# APP 1 ----
# *****************************************************************************

# * Module ----
histogramUI <- function(id) {
    tagList(
        selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
        numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
        plotOutput(NS(id, "hist"))
    )
}

histogramServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- reactive(mtcars[[input$var]])
        output$hist <- renderPlot({
            hist(data(), breaks = input$bins, main = input$var)
        }, res = 96)
    })
}

# * App function ----
histogramApp <- function() {
    ui <- fluidPage(
        histogramUI("hist1")
    )
    server <- function(input, output, session) {
        histogramServer("hist1")
    }
    shinyApp(ui, server)  
}

histogramApp()




# *****************************************************************************
# **** ----
# APP 2 ----
# *****************************************************************************

# * Module ----
datasetInput <- function(id, filter = NULL) {
    names <- ls("package:datasets")
    if (!is.null(filter)) {
        data <- lapply(names, get, "package:datasets")
        names <- names[vapply(data, filter, logical(1))]
    }
    
    selectInput(NS(id, "dataset"), "Pick a dataset", choices = names)
}

datasetServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive(get(input$dataset, "package:datasets"))
    })
}

# * App Function ----
datasetApp <- function(filter = NULL) {
    ui <- fluidPage(
        datasetInput("dataset", filter = filter),
        tableOutput("data")
    )
    server <- function(input, output, session) {
        data <- datasetServer("dataset")
        output$data <- renderTable(head(data()))
    }
    shinyApp(ui, server)
}

datasetApp()



# *****************************************************************************
# **** ----
# APP 3 ----
# *****************************************************************************

# * Module ----
selectVarInput <- function(id) {
    selectInput(NS(id, "var"), "Variable", choices = NULL) 
}

find_vars <- function(data, filter) {
    names(data)[vapply(data, filter, logical(1))]
}

selectVarServer <- function(id, data, filter = is.numeric) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectInput(session, "var", choices = find_vars(data(), filter))
        })
        
        reactive(data()[[input$var]])
    })
}

selectVarApp <- function(filter = is.numeric) {
    ui <- fluidPage(
        datasetInput("data", is.data.frame),
        selectVarInput("var"),
        verbatimTextOutput("out")
    )
    server <- function(input, output, session) {
        data <- datasetServer("data")
        var <- selectVarServer("var", data, filter = filter)
        output$out <- renderPrint(var())
    }
    
    shinyApp(ui, server)
}

selectVarApp()



# *****************************************************************************
# **** ----
# APP 4 ----
# *****************************************************************************

# * Module ----
selectDataVarUI <- function(id) {
    tagList(
        datasetInput(NS(id, "data"), filter = is.data.frame),
        selectVarInput(NS(id, "var"))
    )
}

selectDataVarServer <- function(id, filter = is.numeric) {
    moduleServer(id, function(input, output, session) {
        data <- datasetServer("data")
        var <- selectVarServer("var", data, filter = filter)
        var
    })
}

# * App Function ----
selectDataVarApp <- function(filter = is.numeric) {
    ui <- fluidPage(
        sidebarLayout(
            sidebarPanel(selectDataVarUI("var")),
            mainPanel(verbatimTextOutput("out"))
        )
    )
    server <- function(input, output, session) {
        var <- selectDataVarServer("var", filter)
        output$out <- renderPrint(var(), width = 40)
    }
    shinyApp(ui, server)
}

selectDataVarApp()

# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************

# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************

# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
