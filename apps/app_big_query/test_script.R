
# TESTING R & BIGQUERY ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("apps", "app_big_query"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(bigrquery)

# *****************************************************************************
# **** ----
# DB CONNECTION ----
# *****************************************************************************

# * Create Connection ----
con <- DBI::dbConnect(
    bigrquery::bigquery(),
    project   = "mastering-dbt-394415",
    dataset   = "dbt_lessons",
    #billing   = Sys.getenv("BIG_QUERY_BILLING_ID")
    billing   = "mastering-dbt-394415",
)


# * Authentication ----
bq_auth(path = NULL)


# * List / Inspect Tables ----
dbListTables(con)

DBI::dbReadTable(con, "stg_ecommerce__products", n_max = 5)


# * View ----
stg_products_tbl <- dplyr::tbl(con, "stg_ecommerce__products") %>% head()

schedules_tbl %>% glimpse()


# * Collect ----
product_ids <- select(stg_products_tbl, product_id) %>% 
    distinct() %>% 
    collect()

sample <- stg_products_tbl %>% collect()


# * Data Filtering ----
filtered_product_ids <- product_ids %>% 
    sample_n(size = 100)


# * Uploade to Big Query ----
job <- insert_upload_job(
    values = filtered_product_ids,
    project = "mastering-dbt-394415",
    dataset = "test_upload",
    table   = "test_product_id",
    create_disposition = "CREATE_IF_NEEDED"
)

job$status

# * List Big Query Projects ----
projects <- bq_projects()

projects

purrr::map_chr(projects, "projectId")

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
