library(tidyverse)
library(glue)
library(here)
library(conflicted)
library(lubridate)
library(rvest)
library(jsonlite)

# Conflict preferences
conflict_prefer("filter", "dplyr")

lang_version <- "de"
# Load the product page
file_path <- here("data", "migros", "product_pages", "2022-10-09", lang_version, 
                  "100176800000.html")

# Meta info of the file
file_info <- file.info(file_path)
# When the file was modified last time is the time of scraping
scrape_date <- as_datetime(file_info$mtime) 

# Get the HTML nodes of the file
page <- read_html(file_path)

# Extract product information
prod_id <- "100178800000"

# Price at scraping date
price <- page %>% 
  html_elements(css = ".sale-price") %>% 
  html_text() %>% 
  as.numeric()

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
rating_n <- info_table %>% 
  filter(info_title %in% 
           c("Bewertung", "Évaluation", "Valutazione", "Rating")) %>% 
  mutate(info_content = str_remove_all(info_content, "[^0-9.,]+"), 
         info_content = as.numeric(info_content)) %>% 
  pull(info_content)
rating_score <- page %>%  
  html_elements("mo-product-detail-rating") %>% 
  html_elements("rect") %>% 
  nth(3) %>% 
  as.character() %>% 
  str_extract("(?<=width=\")(.*)(?=\")")

# nutritional-characteristic ng-star-inserted
tibble(product_id = prod_id, 
       prod_name = prod_name, 
       price = price, 
       category_level_1 = cat_level_1, 
       category_level_2 = cat_level_2, 
       category_level_3 = cat_level_3, 
       labels = labels, 
       # Store the info table as JSON object
       info_table = toJSON(info_table),  
       rating_score = rating_score, 
       rating_n = rating_n, 
       lang_version = lang_version, 
       scrape_date = scrape_date) %>% View()
  
