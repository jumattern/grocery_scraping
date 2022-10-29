# Grocery shop scraping

Project to automatically scrape product information of large Swiss retailers like Coop, Migros, Denner etc. in order to build a comprehensive data base and track developments over time. 

Browsing the webpages and executing JS is done with [RSelenium](https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf), parsing with [rvest](https://rvest.tidyverse.org/). 

*Work in progress*

### Migros
- Download and parse product list: product IDs of all products in `migros_productlist.R`
- Download product detail HTML page for every product ID in 4 languages and store them in `migros_productdetail.R`

Author: Julius Mattern
