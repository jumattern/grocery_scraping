# Grocery shop scraping

Project to automatically scrape product information of large Swiss retailers like Coop, Migros, Denner etc. in order to build a comprehensive data base and track developments over time. 

Browsing the webpages and executing JS is done with [RSelenium](https://github.com/ropensci/RSelenium), parsing with [rvest](https://rvest.tidyverse.org/). RSelenium required [Java](https://www.java.com/de/download/manual.jsp) being installed on the local machine. 

*Work in progress*

### Migros
- Download and parse product list: product IDs of all products in `migros_productlist.R`
- Download product detail HTML page for every product ID in 4 languages and store them in `migros_productdetail.R`

Author: Julius Mattern
