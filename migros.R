library(tidyverse)
library(rvest)
library(RSelenium)
library(glue)


# rvest::read_html("https://www.migros.ch/de/category/wein-bier-spirituosen/bier-cidre") %>% 
#   html_text()
# 
# 
# read_html("https://www.migros.ch/de/product/121321000000") %>% 
#   html_nodes("script") 


# https://thatdatatho.com/tutorial-web-scraping-rselenium/

# Set up the browser
driver <- rsDriver(browser = c("firefox"), 
                   port = 4571L,)
remote_driver <- driver[["client"]]
remote_driver$open()

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

# Function to download a HTML category page with product IDs
download_product_ids <- function() {
  # Search all the product list items
  products <- remote_driver$findElement(using = "class", 
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

# Helper function that returns a boolean indicating whether an element exists
# or not
check_if_element_exists <- function(remote_driver, element_class) {
  suppressWarnings(suppressMessages(out <- tryCatch({
    remote_driver$findElement(using = "class", value = element_class)
    return(TRUE)
  },
  error = function(cond) {
    return(FALSE)
  })))
  return(FALSE)
}

# Init result object
result_prod <- tibble()

# Loop through categories and extract all the product IDs of each category
for (cat_idx in 1:length(categories)) {
  current_cat <- categories[cat_idx]
  # Console status
  print(glue("Scraping {current_cat}..."))
  
  # Go to product overview page
  remote_driver$navigate(glue("https://www.migros.ch/de/",
                              # Some special categories without "category"
                              # in the URL
                              if_else(str_detect(current_cat, 
                                                 "health|lifestyle"), 
                                      "", 
                                      "category/"), 
                              "{current_cat}"))
    # Wait for the page to load...
  Sys.sleep(3)
  
  # As long as there is a button "X more products", click it and get all 
  # products displayed before we download them
  while (check_if_element_exists(remote_driver, "btn-view-more")) {
    print("Asking for more products...")
    # Get the "more" button and click it
    button_more <- remote_driver$findElement(using = "class", 
                                             value = "btn-view-more")
    button_more$clickElement() 
    
    # Wait for the page to load...
    Sys.sleep(3)
  }
  
  # Download the products of the page
  result_prod <- result_prod %>%
    bind_rows(download_product_ids())
  
  # Console status
  print(glue("Ended scraping of {current_cat}."))
  
}













