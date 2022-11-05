library(tidyverse)
library(RSelenium)
library(glue)
library(here)
library(conflicted)
library(lubridate)

## Functions to set up Selenium browser and navigate with it

# Function to set up the connection to a Selenium browser for a given engine/
# port configuration
setup_browser <- function(engine = "firefox", port = 4571L) {
  driver <- tryCatch({
    rsDriver(browser = engine, port = port)
  },
  error = function(cond) {
    return(as.character(cond))
  })
  return(driver)
}

# Function to return a Selenium browser on a new port 
get_browser <- function(engine = "firefox", 
                        # waiting_time = 30, 
                        max_tries = 20) {
  # Get the connection to the browser
  port <- 4571L
  # print(glue("Waiting for {waiting_time} seconds for startup..."))
  try_num <- 1
  driver <- setup_browser(engine = engine, port = port)
  # Sys.sleep(waiting_time)
  # If the port is already in use, take the next one until it works
  if (is.character(driver)) {
    print(glue("Problem encountered: {driver}"))
    try_num <- try_num + 1
    if (str_detect(driver, "is already in use")) {
      try_next_port <- TRUE
      # Try new ports until we reached the max. number of tries
      while (try_next_port & try_num <= max_tries) {
        print(glue("Try {try_num}: Trying another port ({port + 1})..."))
        if (is.character(driver)) {
          if (str_detect(driver, "is already in use")) {
            port <- as.integer(port + 1)
            driver <- setup_browser(engine = engine, port = port)
            if (!is.character(driver)) {
              # Successful connection, break
              try_next_port <- FALSE
            }
            # Sys.sleep(waiting_time)
          } else {
            # Other error that port error
            print(glue("New error showed up: {driver}"))
            try_next_port <- FALSE
          }
        } else {
          # Successful connection, break
          try_next_port <- FALSE
        }
        try_num <- try_num + 1
      }
    }
  }
  # Get client and return it
  driver_client <- driver[["client"]]
  # driver_client$open()
  return(driver_client)
}


# Navigate to an URL and wait some second - when the browser is not available, 
# start a new one
navigate_safely <- function(browser, 
                            target_url = "",
                            wait_after = 3) {
  tryCatch({
    # Navigate to page to page
    browser$navigate(target_url)
    # Wait for the page to load...
    Sys.sleep(wait_after)
  },
  error = function(cond) {
    # Set up a new browser
    browser <- get_browser()
  })
}

# Search for a specific element on the browser page and return it, restart 
# browser if needed
find_element_safely <- function(browser, using = "class", value = "") {
  element <- tryCatch({
    browser$findElement(using = using, 
                        value = value)
  },
  error = function(cond) {
    # Set up a new browser when it crashed
    msg <- as.character(cond)
    if (str_detect(msg, "Failed to connect to localhost") |
        str_detect(msg, "A session is either terminated or not started") |
        str_detect(msg, "Failed to connect to localhost") |
        str_detect(msg, "error")) {
      browser <- get_browser()
      # Some seconds for startup needed
      Sys.sleep(5)
      # Recursively call function again
      return(find_element_safely(browser, using, value))
    }
    # Unable to locate element
    return(cond)
  })
  return(element)
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
