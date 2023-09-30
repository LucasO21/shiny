

# Set Working Dir ----
get_working_directory <- function(folder) {
    setwd(here::here(folder))
}

# Load Packages ----
get_packages <- function() {
    
    packages <- c(
        # * Core
        "tidyverse",
        "janitor",
        # "DT",
        # "data.table",
        # "dtplyr",
        "scales",
        # * Shiny 
        "shiny",
        "shinyWidgets",
        "shinyjs",
        "shinydashboard",
        "shinythemes",
        "bslib",
        "shinyBS",
        "htmlwidgets"
    )
    
    installed_packages <- packages %in% rownames(installed.packages())
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }
    
    invisible(lapply(packages, library, character.only = TRUE))
    
}