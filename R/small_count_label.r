###_____________________________________________________________________________
# Function to deal with small counts
# If you need to mask values due to some policy stating you must,
# this is a convenience function to help you do that.
###_____________________________________________________________________________

small_count_label <- function(var, cutoff, replacement) {
  
  if("tidyverse" %not_in% installed.packages()) {
    
    install.packages("tidyverse")
    
  } 
  
  if("package:tidyverse" %not_in% search()) {
    
    library(tidyverse)
    
  } 
  
  if(!is.character(replacement)) {
  
  output <- if_else(var < cutoff, replacement, var)
    
  } else if(is.character(replacement)) {
    
  output <- if_else(var < cutoff, replacement, as.character(var))
  
    
  }
  
  return(output)
  
}
