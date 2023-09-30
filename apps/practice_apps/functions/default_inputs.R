
# Get Default Inputs ----
get_inputs <- function(...) {
    
    # default inputs 
    defaults <- list(
        # paid new inputs
        paid_new_campaign_list             = c("linkedin", "spotify"),
        paid_new_clicks                    = c(1000, 1000000),
        paid_new_leads                     = c(75, 1500),
        paid_new_cost                      = c(6000, 17000)
    )
    
    # args list
    args <- modifyList(defaults, list(...))
    
    paid_new_campaign_list = args$paid_new_campaign_list
    paid_new_clicks = args$paid_new_clicks
    paid_new_leads = args$paid_new_leads
    paid_new_cost = args$paid_new_cost
    
    # return
    return(list(
        paid_new_campaign_list = paid_new_campaign_list,
        paid_new_clicks = paid_new_clicks,
        paid_new_leads = paid_new_leads,
        paid_new_cost = paid_new_cost
    ))
    
    
}

get_inputs()
