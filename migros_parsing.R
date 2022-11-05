library(tidyverse)
library(glue)
library(here)
library(conflicted)
library(lubridate)
library(rvest)
library(jsonlite)

# Conflict preferences
conflict_prefer("filter", "dplyr")


# Function to parse the HTML of one product file and return a tibble with the
# extracted data
parse_file <- function(file_path, file_name, lang_version) {
  # Meta info of the file
  file_info <- file.info(file_path)
  
  # When the file was modified last time is the time of scraping
  scrape_datetime <- as_datetime(file_info$mtime) 
  
  # Get the HTML nodes of the file
  page <- read_html(file_path)
  
  # Extract product ID
  prod_id <- file_name %>% 
    str_remove_all("[^0-9]+")
  
  ## Price(s)/Discount info at scraping date
  
  # E.g. "24%"
  discount_info <- page %>% 
    html_elements(css = ".badge-promo") %>% 
    html_text()
  is_in_discount <- length(discount_info) != 0
  if (length(discount_info) == 0)
    discount_info <- NA
  
  # Regional price? 
  is_regional_price <- page %>% 
    html_elements(css = ".regional-price") %>% 
    length() == 1
  
  # Currently "active" price - can be a discount price
  prices <- page %>% 
    html_elements(css = ".sale-price") %>% 
    html_text() %>% 
    str_split("statt|au lieu de|invece di|was") %>% 
    unlist() %>% 
    str_remove_all("\\.–")
  
  # Get the active price and in case of dicount, take the dicounted price as 
  # active price and the normal price as "before_price"
  active_price <- as.numeric(str_trim(prices[1]))
  before_price <- NA
  if (is_in_discount)
    before_price <- as.numeric(str_trim(prices[2]))
  
  # Product name in the language of the page
  prod_name <- page %>% 
    html_elements("meta") %>%
    as.character() %>% 
    as_tibble() %>%
    filter(str_detect(value, "<meta name=\"description\" content=\"")) %>% 
    str_extract("(?<=content=\")(.*)(?=\")")
  
  # Product categories in three levels in the language of the page
  categories <- page %>% 
    html_elements(css = ".breadcrumb") %>%
    html_elements("li") %>% 
    html_text()
  cat_level_1 <- categories[2] # E.g. "Süsse Lebensmittel"
  cat_level_2 <- categories[3] # E.g. "Schokolade & Süssigkeiten"
  cat_level_3 <- categories[4] # E.g. "Tafelschokoladen"
  
  # Product label(s), e.g. aha!, Swissness. Store semicolon-separated. 
  labels <- page %>% 
    html_elements("mo-product-detail-brand-labels") %>% 
    html_elements(css = ".mat-tooltip-trigger") %>% 
    as.character() %>% 
    str_extract("(?<=alt=\")(.*)(?=\" data-fallback-src)") %>% 
    paste(collapse = ";")
  if (labels == "NA")
    labels <- NA
  
  # # Nutritional characteristics (e.g. "Laktosefrei")
  # nutritional_characteristics <- page %>%  
  #   html_elements(".nutritional-characteristic") %>% 
  #   html_text() %>% 
  #   paste(collapse = ";")
  
  # Extract dynamically whole box of information
  info_titles <- page %>%  
    html_elements("mo-product-information-item") %>% 
    html_elements("dt") %>% 
    html_text()
  info_content <- page %>%  
    html_elements("mo-product-information-item") %>% 
    html_elements("dd") %>% 
    html_text(trim = TRUE) 
  info_table <- tibble(info_title = info_titles, 
                       info_content = info_content)
  
  # Extract rating information
  # When there are ratings available, extract them
  rating_raw <- info_table %>% 
    filter(info_title %in% 
             c("Bewertung", "Évaluation", "Valutazione", "Rating"))
  # No ratings yet available, set n to 0
  if (str_detect(rating_raw$info_content, 
                 "Noch keine|Pas encore|Nessuna|No ratings")) {
    rating_n <- 0  
  } else { # At least one rating available
    rating_n <- rating_raw %>% 
      mutate(info_content = str_remove_all(info_content, "[^0-9.,]+"), 
             info_content = as.numeric(info_content)) %>% 
      pull(info_content)
  }
  
  # Extract the rating score if available
  rating_score <- page %>%  
    html_elements("mo-product-detail-rating") %>% 
    html_elements("rect") %>% 
    nth(3) %>% 
    as.character() %>% 
    str_extract("(?<=width=\")(.*)(?=\")") %>% 
    as.numeric()
  if (length(rating_score) == 0)
    rating_score <- NA
  
  # nutritional-characteristic ng-star-inserted
  prod_row <- tibble(product_id = prod_id, 
                     prod_name = prod_name, 
                     active_price = active_price,
                     before_price = before_price, 
                     is_in_discount = is_in_discount, 
                     is_regional_price = is_regional_price, 
                     discount_info = discount_info, 
                     category_level_1 = cat_level_1, 
                     category_level_2 = cat_level_2, 
                     category_level_3 = cat_level_3, 
                     labels = labels, 
                     # Store the info table as JSON object
                     info_table = as.character(toJSON(info_table)),  
                     rating_score = rating_score, 
                     rating_n = rating_n, 
                     lang_version = lang_version, 
                     scrape_datetime = scrape_datetime) 
  if (nrow(prod_row) != 1)
    stop(glue("Product {prod_id} returned <> 1 row."))
  return(prod_row)
}

# Get date-named directories (scraping dates)
scrape_dates <- list.files(here("data", "migros", "product_pages"))

# Init results object
parsed_product_data <- tibble()

# Through the directories of scraping dates
for (scrape_date_idx in seq_len(length(scrape_dates))) {
  # E.g. "2022-10-09"
  scrape_date_dir <- scrape_dates[scrape_date_idx]
  # Languages available for that date
  current_langs <- list.files(here("data", "migros", "product_pages", 
                                   scrape_date_dir))
  # Through each of the available languages in this scraping directory
  for (scrape_date_lang_idx in seq_len(length(current_langs))) {
    # E.g. "de"
    current_lang <- current_langs[scrape_date_lang_idx]
    products <- list.files(here("data", "migros", "product_pages", 
                                scrape_date_dir, current_lang))
    # For each product HTML (of this scraping date and language)
    for (prod_idx in seq_len(length(products))) {
      product_filename <- products[prod_idx]
      # product_filename <- "130904900000.html"
      print(glue("Parsing {current_lang}/{scrape_date_dir} ({prod_idx}/", 
                 "{length(products)}): {product_filename}..."))
      # Put together the complete file path of the file to be parse
      file_path <- here("data", "migros", "product_pages", scrape_date_dir, 
                        lang_version, product_filename)
      # Parse the file, extract the data
      file_data <- parse_file(file_path = file_path, 
                              file_name = product_filename, 
                              lang_version = current_lang)
      # Parse the file and add it to the parsed results
      parsed_product_data <- parsed_product_data %>% 
        bind_rows(file_data)
    }
  }
}

# Export parsed data
parsed_product_data %>% 
  write_csv(here("data", "migros", glue("Parsed_products_{today()}.csv")))
parsed_product_data %>% 
  write_rds(here("data", "migros", glue("Parsed_products_{today()}.rds")), 
            compress = "xz")

