

# Source ----
source("functions/custom_valuebox.R")


# VB UI ----
module_vb_UI <- function(id) {
    ns <- NS(id)
    valueBoxOutput(ns("vb"))
}

# VB Server ----
module_vb_Server <- function(id, value = "1,563", title = "Sales", 
                             #subtitle = tagList(HTML("&uarr;"), "25% Since Last Day"), 
                             subtitle = "Subtitile",
                             sparkobj = NULL, info_icon = TRUE, info = NULL,
                             icon = "code", color = "blue", href = NULL,
                             width = 4) {
    moduleServer(
        id,
        function(input, output, session) {
            output$vb <- renderValueBox({
                # valueBox(
                #     value     = value,
                #     title     = title,
                #     subtitle  = subtitle,
                #     sparkobj  = sparkobj,
                #     info_icon = info_icon,
                #     info      = info,
                #     icon      = icon(icon),
                #     width     = width,
                #     color     = color,
                #     href      = href
                # )
                value_box_custom(
                    value     = value,
                    title     = title,
                    subtitle  = subtitle,
                    sparkobj  = sparkobj,
                    info_icon = info_icon,
                    info      = info,
                    icon      = icon(icon),
                    width     = width,
                    color     = color,
                    href      = href
                )
            })
        }
    )
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
    
