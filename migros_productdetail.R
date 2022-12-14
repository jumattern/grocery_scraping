library(tidyverse)
library(RSelenium)
library(glue)
library(here)
library(conflicted)
library(lubridate)

# Load functions to set up Selenium browser and navigate with it
source(here("selenium_functions.R"))

# Start up the browser and wait until ready
browser <- get_browser()

# Load all products
products <- read_rds(here("data", "migros", "product_ids_2022-10-09.rds"))

# The same product can be in multiple categories, get unique products
products_unique <- products %>% 
  distinct(product_id)
n_products <- nrow(products_unique)

# Function to create a new directory of the current date if it does not exist
# yet
create_current_date_dir <- function() {
  # Create a product page folder
  if (!dir.exists(here("data", "migros", "product_pages")))
    dir.create(here("data", "migros", "product_pages"))
  if (!dir.exists(here("data", "migros", "product_pages", today())))
    dir.create(here("data", "migros", "product_pages", today()))
  if (!dir.exists(here("data", "migros", "product_pages", today(), "de")))
    dir.create(here("data", "migros", "product_pages", today(), "de"))
  if (!dir.exists(here("data", "migros", "product_pages", today(), "fr")))
    dir.create(here("data", "migros", "product_pages", today(), "fr"))
  if (!dir.exists(here("data", "migros", "product_pages", today(), "it")))
    dir.create(here("data", "migros", "product_pages", today(), "it"))
  if (!dir.exists(here("data", "migros", "product_pages", today(), "en")))
    dir.create(here("data", "migros", "product_pages", today(), "en"))
}

# Download a product detail page and store it
download_product <- function(lang = "de", current_product_id) {
  # Go to product overview page
  navigate_safely(browser, glue("https://www.migros.ch/{lang}/product/",                          
                                "{current_product_id}"), wait_after = 4)
  
  # Download the whole result page 
  whole_page <- browser$getPageSource()
  
  # Create  new directory of the current date if needed
  create_current_date_dir()
  
  # Store it
  write.table(whole_page,
              file = here("data", "migros", "product_pages", today(), lang, 
                          glue("{current_product_id}.html")),
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE
  )
  
}

# Loop through every product and download the result page
for (prod_idx in 1:n_products) { # seq_len(n_products)) { # 1:n_products) {
  current_product_id <- products_unique$product_id[prod_idx]
  
  # Console status
  print(glue("Scraping product {current_product_id}", 
             " ({prod_idx}/{n_products})..."))
  
  # Download French and German product detail pages
  download_product(lang = "de", current_product_id)
  download_product(lang = "fr", current_product_id)
  download_product(lang = "en", current_product_id)
  download_product(lang = "it", current_product_id)
}

