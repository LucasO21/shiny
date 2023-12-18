# Custom Value Box ----
value_box_custom <- function(value, title, sparkobj = NULL, subtitle, info_icon = TRUE,
                             info = NULL, icon = NULL, color = "aqua", width = 4, 
                             href = NULL){
    
    shinydashboard:::validateColor(color)
    
    if (!is.null(icon))
        shinydashboard:::tagAssert(icon, type = "i")
    
    if (info_icon) {
        info_icon <- tags$small(
            tags$i(
                class = "fa fa-info-circle fa-lg",
                title = info,
                `data-toggle` = "tooltip",
                style = "color: rgba(255, 255, 255, 0.75);"
            ),
            class = "pull-right"
        )
    } else {
        info_icon <- NULL
    }
    
    boxContent <- div(
        class = paste0("small-box bg-", color),
        div(
            class = "inner",
            tags$span(class = "custom-vb-title", title),
            if (!is.null(sparkobj)) info_icon,
            h3(value),
            if (!is.null(sparkobj)) sparkobj,
            p(subtitle)
        ),
        if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
    )
    
    if (!is.null(href)) 
        boxContent <- a(href = href, boxContent)
    
    div(
        class = if (!is.null(width)) paste0("col-sm-", width), 
        boxContent
    )
}