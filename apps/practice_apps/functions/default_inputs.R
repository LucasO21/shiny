
# Get Default Inputs ----
get_inputs <- function(...) {
    
    # default inputs 
    defaults <- list(
        # paid new inputs
        paid_new_campaign_list             = c("linkedin", "spotify"),
        paid_new_clicks                    = c(1000, 2000),
        paid_new_leads                     = c(75, 1500),
        paid_new_cost                      = c(6000, 17000),
        multiply = c(0.75, 1.10)
    )
    
    # args list
    args <- modifyList(defaults, list(...))
    
    paid_new_campaign_list = args$paid_new_campaign_list
    paid_new_clicks = args$paid_new_clicks
    paid_new_leads = args$paid_new_leads
    paid_new_cost = args$paid_new_cost
    multiply = args$multiply
    
    # return
    return(list(
        paid_new_campaign_list = paid_new_campaign_list,
        paid_new_clicks = paid_new_clicks,
        paid_new_leads = paid_new_leads,
        paid_new_cost = paid_new_cost,
        multiply = multiply
    ))
    
    
}

# get_inputs()


# Get PPC Metrics ----
get_ppc_metrics <-
    function(data, campaign_name = NULL, campaign_level) {
        
        # grouping column setup
        if (campaign_level == "campaign_total") {
            group_var = "campaign"
        } else if (campaign_level == "campaign_id") {
            group_var = "campaign_id"
        }
        
        # data setup
        data <- data %>% as_tibble()
        
        # data filtering
        if (is.null(campaign_name)) {
            df <- data 
        } else {
            df <- data %>% 
                filter(str_detect(str_to_lower(campaign), {{campaign_name}}))
        }
        
        # aggregate calculation
        df <- df %>% 
            mutate(total = "Total") %>% 
            select(total, {{group_var}}, cost, clicks, impressions, conversions) %>% 
            pivot_longer(
                cols = c(total, {{group_var}}),
                names_to = "key", values_to = "campaign"
            ) %>% 
            summarise(
                across(
                    is.numeric,
                    ~ sum(.x, na.rm = TRUE),
                    .names = "total_{.col}"
                ),
                .by = campaign
            ) %>% 
            arrange(campaign)
        
        # factor reorder for sorting campaign column
        if (df$campaign[1] == "AW_Apr17") {
            desired_order <- c("AW_Apr17", "AW_May17", "AW_Jun17", "total")
            df <- df %>% 
                arrange(factor(campaign, levels = desired_order))
        } else if (df$campaign[1] == "FB_Apr17") {
            desired_order <- c("FB_Apr17", "FB_May17", "FB_Jun17", "total")
            df <- df %>% 
                arrange(factor(campaign, levels = desired_order))
        }
        
        # metrics calculation
        ret <- df %>% 
            rename(total_leads = total_conversions) %>% 
            mutate(
                ctr = total_clicks / total_impressions,
                cpc = total_cost / total_clicks,
                ctl_cvr = total_leads / total_clicks
            )
        
        # return
        return(ret)
        
    }