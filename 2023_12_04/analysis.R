

# WORKING DIR ----
setwd(here::here("2023_12_04"))

# LIBRARIES ----
library(tidyverse)


# DATA IMPORT ----
df <- read_csv("data/online_retail_II.csv") %>% 
    janitor::clean_names() %>% 
    sample_n(size = 10000)

df_cleaned <- df %>% 
    mutate(invoice_date = lubridate::date(invoice_date)) %>% 
    drop_na()
    

