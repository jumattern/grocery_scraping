library(tidyverse)
library(RSelenium)
library(glue)
library(here)
library(conflicted)
library(lubridate)

# Load all products
products <- read_rds(here("data", "migros", "product_ids_2022-10-09.rds"))

# The same product can be in multiple categories, get unique products
products_unique <- products %>% 
  distinct(product_id)
n_products <- nrow(products_unique)

# Set up the browser
driver <- rsDriver(browser = c("firefox"),
                   port = 4572L, )
remote_driver <- driver[["client"]]
remote_driver$open()

# Create a product page folder
if (!dir.exists(here("data", "migros", "product_pages")))
  dir.create(here("data", "migros", "product_pages"))
if (!dir.exists(here("data", "migros", "product_pages", "fr")))
  dir.create(here("data", "migros", "product_pages", "fr"))
if (!dir.exists(here("data", "migros", "product_pages", "de")))
  dir.create(here("data", "migros", "product_pages", "de"))

# Download a product detail page and store it
download_product <- function(lang = "de", current_product_id) {
  # Go to product overview page
  remote_driver$navigate(glue("https://www.migros.ch/{lang}/product/",                          
                              "{current_product_id}"))
  
  # Wait for the page to load...
  Sys.sleep(4)
  
  # Download the whole result page 
  whole_page <- remote_driver$getPageSource()
  
  # Store it
  write.table(whole_page,
              file = here("data", "migros", "product_pages", lang, 
                          glue("{current_product_id}.html")),
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE
  )

}


# Loop through every product and download the result page
for (prod_idx in 1:n_products) {
  current_product_id <- products_unique$product_id[prod_idx]
  
  # Console status
  print(glue("Scraping product {current_product_id}", 
             " ({prod_idx}/{n_products})..."))
  
  # Download French and German product detail pages
  download_product(lang = "de", current_product_id)
  download_product(lang = "fr", current_product_id)
}
