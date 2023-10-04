
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

# * Auth ----
#bq_auth(email = "lokwudishu@gmail.com")

# * List Projects ----
projects_list <- bq_projects()

# * Datasets List ----
dataset_list <- bq_project_datasets(projects_list[4])

# * Dataset Table ----
get_project_datasets <- function(project_id) {
    
    if (is_empty(bq_project_datasets(project_id))) {
        return(
            tibble(
                project = c(NA), dataset = c(NA)
            )
        )
    }
    
    tbl <- bigrquery::bq_project_datasets(project_id) %>% 
        map(.f = function(x) {
            x$dataset
        }) %>% unlist()
    
    tibble(
        dataset = c(tbl)
    ) %>% 
        mutate(project = project_id, .before = dataset)
}

get_project_datasets(project_id = projects_list[1])

# * Project & Dataset Table ----
get_project_dataset_table <- function(projects) {
    
    projects %>% 
        map(
            .f = function(x) {
                get_project_datasets(x)
            }
        ) %>% 
        bind_rows() %>% 
        filter(! is.na(project))

}

projects_datasets_tbl <- get_project_dataset_table(projects_list)


# * Create Connection ----
selected_project <- "mastering-dbt-394415"
dataset <- projects_datasets_tbl[projects_datasets_tbl$project == selected_project, ]$dataset[1]

con <- DBI::dbConnect(
    bigrquery::bigquery(),
    project   = selected_project,
    dataset   = dataset,
    billing   = selected_project
)


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
