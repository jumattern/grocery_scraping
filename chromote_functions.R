library(tidyverse)
library(glue)
library(here)
library(conflicted)
library(chromote)
library(hayalbaz)

## Functions to set up chromote browser and navigate with it
## Ref: https://rstudio.github.io/chromote/

# Hayalbaz (https://github.com/rundel/hayalbaz)
# Puppeteer in a different language - this R package provides a puppeteer 
# inspired interface to the Chrome Devtools Protocol using chromote.

# remotes::install_github("rundel/hayalbaz")


# Function to set up the connection to a Selenium browser for a given engine/
# port configuration
setup_session <- function() {
  session <- tryCatch({
    ChromoteSession$new()
  },
  error = function(cond) {
    return(as.character(cond))
  })
  return(session)
}

# Function to return a chromote session
get_session <- function(waiting_time = 0,
                        max_tries = 20, 
                        open_viewer = FALSE) {
  try_num <- 1
  session <- setup_session()
  
  # If configured, open a viewer for the headless browser
  if (open_viewer) {
    session$view()
  }
  
  # Get the connection to the session
  print(glue("Waiting for {waiting_time} seconds for startup..."))
  Sys.sleep(waiting_time)
  
  # # If the port is already in use, take the next one until it works
  # if (is.character(driver)) {
  #   print(glue("Problem encountered: {driver}"))
  #   try_num <- try_num + 1
  #   if (str_detect(driver, "is already in use")) {
  #     try_next_port <- TRUE
  #     # Try new ports until we reached the max. number of tries
  #     while (try_next_port & try_num <= max_tries) {
  #       if (is.character(driver)) {
  #         if (str_detect(driver, "is already in use")) {
  #           port <- as.integer(port + 1)
  #           print(glue("Try {try_num}: Trying another port ({port})..."))
  #           driver <- setup_browser(engine = engine, port = port)
  #           if (!is.character(driver)) {
  #             # Successful connection, break
  #             try_next_port <- FALSE
  #           } else {
  #             print(glue("Still getting error: {driver}"))
  #             Sys.sleep(waiting_time)
  #           }
  #         } else {
  #           # Other error than port error
  #           print(glue("New error showed up: {driver}"))
  #           try_next_port <- FALSE
  #         }
  #       } else {
  #         # Successful connection, break
  #         print("Successful connection!")
  #         try_next_port <- FALSE
  #       }
  #       try_num <- try_num + 1
  #     }
  #   } else {
  #     # Other error, return the error message
  #     return(driver)
  #   }
  # } 
  # # Get client and return it
  # driver_client <- driver[["client"]]
  # # driver_client$open()
  return(session)
}

#' #' click on the element
#' chromote.click <- function(session, selector) {
#'   doc = session$DOM$getDocument()
#'   node = session$DOM$querySelector(doc$root$nodeId, selector)
#'   box <- session$DOM$getBoxModel(node$nodeId)
#'   left <- box$model$content[[1]]
#'   top <- box$model$content[[2]]
#'   x <- left + (box$model$width / 2)
#'   y <- top + (box$model$height / 2)
#'   session$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button="left")
#'   session$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button="left")
#' }

# Navigate to an URL and wait some second - when the session is not available, 
# start a new one
navigate_safely <- function(session, 
                            target_url = "",
                            wait_after = 3) {
  tryCatch({
    # Navigate to page to page
    session$Page$navigate(target_url)
    # Wait for the page to load...
    Sys.sleep(wait_after)
  },
  error = function(cond) {
    # Set up a new browser
    session <- get_session()
  })
}

# Search for a specific element on the session page and return it, restart 
# session if needed
find_element_safely <- function(session, using = "class", value = "") {
  element <- tryCatch({
    session$findElement(using = using, 
                        value = value)
  },
  error = function(cond) {
    # Set up a new session when it crashed
    msg <- as.character(cond)
    if (str_detect(msg, "Failed to connect to localhost") |
        str_detect(msg, "A session is either terminated or not started") |
        str_detect(msg, "Failed to connect to localhost") |
        str_detect(msg, "error")) {
      session <- get_session()
      # Some seconds for startup needed
      Sys.sleep(5)
      # Recursively call function again
      return(find_element_safely(session, using, value))
    }
    # Unable to locate element
    return(cond)
  })
  return(element)
}


# Helper function that returns a boolean indicating whether an element exists
# or not
check_if_element_exists <- function(session, selector) {
  doc <- session$DOM$getDocument()
  node_id <- session$DOM$querySelector(doc$root$nodeId, selector)
  if (node_id$nodeId == 0)
    return(FALSE)
  return(TRUE)
}
