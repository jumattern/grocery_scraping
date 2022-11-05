library(tidyverse)
library(RSelenium)
library(glue)
library(here)
library(conflicted)
library(lubridate)

## Go through the specified product pages of https://www.migros.ch and 
## download a list of product ids per category. A product id is e.g. 
## "120983100000" and can be used to directly access a product detail page:
## https://www.migros.ch/de/product/120983100000

# Load functions to set up Selenium browser and navigate with it
source(here("selenium_functions.R"))

# Function to download a HTML category page with product IDs
download_product_ids <- function(browser) {
  # Search all the product list items
  products <- browser$findElement(using = "class", 
                                  value = "ng-star-inserted")
  # Download the HTML
  products <- products$getElementAttribute("innerHTML")
  
  # Wait for the page to load...
  Sys.sleep(3)
  
  # Extract the numerical product ids of the selected page
  product_ids <- products %>%  
    unlist() %>% 
    str_extract_all("(?<=\\/de\\/product\\/)([0-9]*)(?=\\\")") %>% 
    unlist()
  
  return(tibble(category = current_cat,
                product_id = product_ids))
}

# Function to scroll to the bottom of the page 
scroll_down_page <- function(browser) {
  body <- browser$findElement("css", "body")
  body$sendKeysToElement(list(key = "end"))
}

# Set up the browser
browser <- get_browser()

# All the categories that should be scraped
categories <- c("health/glutenfrei", 
                "health/laktosefrei",
                "lifestyle/vegetarisch",
                "lifestyle/vegan",
                "obst-gemuse", 
                "fleisch-fisch",
                "milchprodukte-eier-frische-ferti", 
                "brot-backwaren", 
                "susse-lebensmittel", 
                "salzige-lebensmittel", 
                "tiefkuhlprodukte", 
                "getranke-kaffee-tee", 
                "wein-bier-spirituosen", 
                "baby-kind", 
                "hygiene-kosmetik", 
                "waschmittel-putzmittel", 
                "tierbedarf", 
                "haus-hobby", 
                "bekleidung-accessoires")


# Init result object
result_prod <- tibble()

# Loop through categories and extract all the product IDs of each category
for (cat_idx in 1:length(categories)) {
  current_cat <- categories[cat_idx]
  # Console status
  print(glue("Scraping {current_cat}..."))
  
  # Go to product overview page
  # browser <- 
  navigate_safely(browser, glue("https://www.migros.ch/de/",
                                # Some special categories without "category"
                                # in the URL
                                if_else(str_detect(current_cat, 
                                                   "health|lifestyle"), 
                                        "", 
                                        "category/"), 
                                "{current_cat}"), wait_after = 3)
  
  # Click away cookie message
  button_cookies <- find_element_safely(browser, using = "class", 
                                     value = ".mat-button-base")
  # Click the more button
  button_cookies$clickElement() 
  
  # As long as there is a button "X more products", click it and get all 
  # products displayed before we download them
  while (check_if_element_exists(browser, "btn-view-more")) {
    print("Asking for more products...")
    # Get the "more" button and click it
    button_more <- find_element_safely(browser, using = "class", 
                                       value = "btn-view-more")
    # Scroll to the bottom
    scroll_down_page(browser)
    
    # Click the more button
    button_more$clickElement() 
    
    # Wait for the page to load...
    Sys.sleep(3)
    
    # Scroll to the bottom
    scroll_down_page(browser)
    
    # Wait for the page to load...
    Sys.sleep(1)
    
    # Scroll to the bottom
    scroll_down_page(browser)
  }
  
  # Download the products of the page
  result_prod <- result_prod %>%
    bind_rows(download_product_ids(browser))
  
  # Console status
  print(glue("Ended scraping of {current_cat}."))
  
}

# Unique product IDs
result_prod %>% 
  distinct(product_id) %>% 
  nrow() #22618 (2022-10-09)

# Save the file
result_prod %>%  
  write_rds(here("data", "migros", glue("product_ids_{today()}.rds")), 
            compress = "xz")
