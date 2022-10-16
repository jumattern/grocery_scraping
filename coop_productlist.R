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

# https://thatdatatho.com/tutorial-web-scraping-rselenium/

# Set up the browser
driver <- rsDriver(browser = c("firefox"),
                   port = 4571L, )
remote_driver <- driver[["client"]]
remote_driver$open()

# # All the categories that should be scraped
# categories <- tibble(maincat = c("Lebens"))

# Function to download a HTML category page with product IDs
download_product_ids <- function() {
  # Search all the product list items
  products <- remote_driver$findElement(using = "class", 
                                        value = "list-page__content")
  # Download the HTML
  products <- products$getElementAttribute("innerHTML")
  
  # Wait for the page to load...
  # Sys.sleep(3)
  
  # Extract the numerical product ids of the selected page
  product_ids <- products %>%  
    unlist() %>% 
    str_extract_all("(?<=product-tile-)([0-9]*)(?=\\\")") %>% 
    unlist()
  
  return(tibble(product_id = product_ids))
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

# https://www.coop.ch/de/lebensmittel/c/m_9753


# Function to extract the title of the current product overview page as well 
# as the hierarchy of it
extract_title_hierarchy <- function() {
  # Get the category title
  cat_title <- NA
  if (check_if_element_exists("spacing-bottom-lg-up-30")) {
    cat_title <- remote_driver$findElement(using = "class", 
                                           value = "spacing-bottom-lg-up-30")
  } else {
    cat_title <- remote_driver$findElement(using = "class", 
                                           value = "cmsHeroImage-content-title")
  }
  
  # Extract the HTML
  cat_title <- cat_title$getElementAttribute("innerHTML") %>% 
    unlist() %>% 
    str_trim()
  
  # Hierarchy
  cat_hierarchy <- remote_driver$findElement(using = "class", 
                                             value = "spacing-top-15")
  # Extract the HTML
  cat_hierarchy <- cat_hierarchy$getElementAttribute("innerHTML") %>% 
    unlist()
  # Extract the numerical product ids of the selected page
  category_lvl_0 <- cat_hierarchy %>%  
    str_extract_all("(?<=a-udo-cat0=\\\"\\\">)(.*)(?=</span>)") %>% 
    unlist() %>% 
    str_trim() %>% 
    # In level 0, first element is "Startseite", take only second element
    nth(2)
  category_lvl_1 <- cat_hierarchy %>%  
    str_extract_all("(?<=a-udo-cat1=\\\"\\\">)(.*)(?=</span>)") %>% 
    unlist() %>% 
    str_trim()
  category_lvl_2 <- cat_hierarchy %>%  
    str_extract_all("(?<=a-udo-cat2=\\\"\\\">)(.*)(?=</span>)") %>% 
    unlist() %>% 
    str_trim()
  category_lvl_3 <- cat_hierarchy %>%  
    str_extract_all("(?<=a-udo-cat3=\\\"\\\">)(.*)(?=</span>)") %>% 
    unlist() %>% 
    str_trim()  
  tibble(
    category_lvl_0 = category_lvl_0, 
    category_lvl_1 = if_else(length(category_lvl_1) != 0, category_lvl_1, 
                             as.character(NA)), 
    category_lvl_2 = if_else(length(category_lvl_2) != 0, category_lvl_2, 
                             as.character(NA)),
    category_lvl_3 = if_else(length(category_lvl_3) != 0, category_lvl_3, 
                             as.character(NA)),
    category_title = cat_title) 
}

# Init result object
result_prod <- tibble()


max_category_id <- 99999 

for (category_id in 1:max_category_id) {
  category_id_char <- str_pad(category_id, width = 4, pad = "0")
  print(glue("Loading category page {category_id_char}..."))
  
  # As long as there is a button "X more products", click it and get all 
  # products displayed before we download them
  pagination_idx <- 1
  product_list_url <- glue("https://www.coop.ch/de/lebensmittel/c/m_{category_id_char}?q=%3AtopRated&sort=name-asc&pageSize=500&page={pagination_idx}")
  remote_driver$navigate(glue("{product_list_url}"))
  # Wait for the page to load...
  Sys.sleep(10)
  
  
  # Download the products of the page
  result_prod <- result_prod %>%
    bind_rows(
      tibble(
        extract_title_hierarchy(), 
        download_product_ids()
      )
    )
  
  while (check_if_element_exists(remote_driver, "pagination__next")) {
    print("Asking for more products...")
    pagination_idx <- pagination_idx + 1
    product_list_url <- glue("https://www.coop.ch/de/lebensmittel/c/m_{category_id_char}?q=%3AtopRated&sort=name-asc&pageSize=500&page={pagination_idx}")
    remote_driver$navigate(glue("{product_list_url}"))
    
    # Wait for the page to load...
    Sys.sleep(10)  
    
    # Download the products of the page
    result_prod <- result_prod %>%
      bind_rows(
        tibble(
          extract_title_hierarchy(), 
          download_product_ids()
        )
      )
  }
  
  # Console status
  print(glue("Ended scraping of {category_id_char}."))
  
}


stop("finish")




# Click on "Alle ansehen"
# cmsTeaserRow-controls__see-all-text

# Get the "more" button and click it
button_see_all <- remote_driver$findElement(using = "class", 
                                            value = "btn-view-more")
button_see_all$clickElement() 

# Wait for the page to load...
Sys.sleep(3)

# Search all the product list items
sub_cats <- remote_driver$findElement(using = "class", 
                                      value = "cmsList__itemLink")



# Download the HTML
sub_cats <- sub_cats$getElementAttribute("innerHTML")

sub_cats %>% 
  unlist()



# # Getting all sub-categories (Früchte & Gemüse, Fleisch & Fisch etc.) under 
# # the main categories (Lebensmittel, Weine etc)
# 
# # # Init the hardcoded main categories
# # main_categories <- tibble(
# #   name = c(
# #     "Lebensmittel",
# #     "Weine",
# #     "Haushalt & Tier",
# #     "Kosmetik & Gesundheit",
# #     "Baby & Kind",
# #     "Lounge",
# #     "Inspiration & Geschenke",
# #     "Aktionen"
# #   ),
# #   url = c(
# #     "https://www.coop.ch/de/lebensmittel/fruechte-gemuese/c/m_0001", 
# #     "https://www.coop.ch/de/weine/c/m_0222", 
# #     "https://www.coop.ch/de/haushalt-tier/c/m_0277", 
# #     "https://www.coop.ch/de/kosmetik-gesundheit/c/m_0333", 
# #     "https://www.coop.ch/de/baby-kind/c/m_0368", 
# #     "https://www.coop.ch/de/weine/lounge/c/m_0224",
# #     "https://www.coop.ch/de/inspiration-geschenke/c/m_0927",
# #     "https://www.coop.ch/de/aktionen/c/m_1000"
# #   )
# # )


# https://www.coop.ch/de/lebensmittel/fruechte-gemuese/c/m_0001

# Loop through the main categories and download all the sub-categories
for (main_cat_idx in 1:nrow(main_categories)) {
  main_cat_name <- main_categories$name[main_cat_idx]
  main_cat_url <- main_categories$url[main_cat_idx]
  print(glue("Getting sub-categories in {main_cat_name}..."))
  
  remote_driver$navigate(glue("{main_cat_url}"))
  
  # Click on "Alle ansehen"
  # cmsTeaserRow-controls__see-all-text
  
  # Get the "more" button and click it
  button_see_all <- remote_driver$findElement(using = "class", 
                                              value = "btn-view-more")
  button_see_all$clickElement() 
  
  # Wait for the page to load...
  Sys.sleep(3)
  
  # Search all the product list items
  sub_cats <- remote_driver$findElement(using = "class", 
                                        value = "cmsList__itemLink")
  
  
  
  # Download the HTML
  sub_cats <- sub_cats$getElementAttribute("innerHTML")
  
  sub_cats %>% 
    unlist()
  
  # Wait for the page to load...
  Sys.sleep(3)
  
  # Extract the numerical product ids of the selected page
  product_ids <- products %>%  
    unlist() %>% 
    str_extract_all("(?<=\\/de\\/product\\/)([0-9]*)(?=\\\")") %>% 
    unlist()
  
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

# Unique product IDs
result_prod %>% 
  distinct(product_id) %>% 
  nrow() #22618 (2022-10-09)

# Save the file
result_prod %>%  
  write_rds(here("data", "migros", glue("product_ids_{today()}.rds")), 
            compress = "xz")
