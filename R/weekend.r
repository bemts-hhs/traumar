###_____________________________________________________________________________
# Classify rows as a weekday or weekend based on a date in a data.frame
###_____________________________________________________________________________

weekend <- function(input_date) {
  
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
        "i" = "Supply a {.cls Date} object to {.fn weekend}."
      )
    )
    
  }
  
  #create the weekday / weekend boundaries of the days
  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  weekend_days <- c("Saturday", "Sunday")

  #conduct the logical test of the values to assign the weekend/weekday label
  
  as_factor(
  if_else(wday(input_date, label = T, abbr = F) %in% weekdays, "Weekday",
          if_else(wday(input_date, label = T, abbr = F) %in% weekend_days, "Weekend", "Undetermined",
                  missing = "Undetermined"
                                )
                          )
  )
  
}
