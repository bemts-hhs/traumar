###_____________________________________________________________________________
### For a single variable in a df
###_____________________________________________________________________________

normalize <- function(x, method = c("min_max", "z_score")) {

if(!"tidyverse" %in% installed.packages()) {

install.packages("tidyverse")
}

if(!"package:tidyverse" %in% search()) {

library(tidyverse)

}

  if (!is.numeric(x) && !is.integer(x)) {
    
    cli_abort("Input must be {.cls numeric} or {.cls integer}. You supplied an object of class {.cls {class(x)}} to {.fn normalize}.")
    
  }
  
  if(length(method) > 1) {
    
    method <- "min_max"
    
    cli::cli_alert_info("As no method was supplied, {.fn normalize} will default to min-max normalization methods.")
  }
  
  if(method == "min_max") {
  
  normalized_data <-
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
  } else if(method == "z_score") {
    
    mean_x <- mean(x, na.rm = T)
    
    std_dev_x <- sd(x, na.rm = T)
    
    x_minus_mean <- x - mean_x
    
    normalized_data <- x_minus_mean / std_dev_x
    
  }
  
  return(normalized_data)
  
}
