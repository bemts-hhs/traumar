###_____________________________________________________________________________
# Classify rows by their season based on a date in a data.frame
###_____________________________________________________________________________

season <- function(input_date) {
  #check if lubridate is installed and loaded
  if (!isNamespaceLoaded("lubridate")) {
    install.packages("lubridate")
    
  }
  
  if ("package:lubridate" %not_in% search()) {
    library(lubridate)
    
  }
  
  #check if tidyverse is installed and loaded
  if (!isNamespaceLoaded("tidyverse")) {
    install.packages("tidyverse")
    
  }
  
  if ("package:tidyverse" %not_in% search()) {
    library(tidyverse)
    
  }
  
  #check if the value supplied is in fact Date or POSIXct
  if (!is.Date(input_date) & !is.POSIXct(input_date)) {
    
    cli_abort(
      paste0(
        "The input to {.var input_date} must be an object of class {.cls Date} or {.cls POSIXct}, but you supplied an object of class {.cls {class(input_date)}}.",
        "i" = "Supply a {.cls Date} object to {.fn season}."
      )
    )
    
  }
  #create the month boundaries of the season based on
  #https://www.weather.gov/dvn/Climate_Astronomical_Seasons
  winter_months <- c(12, 1, 2)
  spring_months <- c(3, 4, 5)
  summer_months <- c(6, 7, 8)
  fall_months <- c(9, 10, 11)
  
  #conduct the logical test of the values to assign the season
  
  as_factor(if_else(
    month(input_date) %in% winter_months,
    "Winter",
    if_else(
      month(input_date) %in% spring_months,
      "Spring",
      if_else(
        month(input_date) %in% summer_months,
        "Summer",
        if_else(
          month(input_date) %in% fall_months,
          "Fall",
          if_else(is.na(month(input_date)), "Undetermined", "Undetermined")
        )
      )
    )
  ))
  
}
