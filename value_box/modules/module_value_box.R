

# Source ----
source("functions/custom_valuebox.R")


# VB UI ----
module_vb_UI <- function(id, value = "1,345", title = "Title", sparkobj = NULL, 
                         subtitle = "Subtitle", info_icon = TRUE,
                         info = NULL, icon = NULL, color = "aqua", width = 4, href = NULL) {
    ns <- NS(id)
    value_box_custom(value, title, sparkobj, subtitle, info_icon,
                     info, icon, color, width, href)
}


# VB Server ----
module_vb_Server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Server-side logic (if any)
    })
}


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




# SPARKLINES ----
# https://api.highcharts.com/highcharts/series

# df <- timetk::m4_daily %>% filter(id == "D10") %>% arrange(date) %>% head(100)
# 
# hc <- hchart(df, "line", hcaes(date, value), name = "Daily Sales") %>% 
#     hc_size(height = 100) %>% 
#     hc_add_theme(hc_theme_sparkline_vb()) %>% 
#     hc_credits(enable = FALSE)
# 
# hc %>% hc_chart(backgroundColor = "#DD4B39")
    
