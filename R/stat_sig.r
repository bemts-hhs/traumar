###_____________________________________________________________________________
# stat_sig() function to call mutate() and get statistical significance based on
# p values
###_____________________________________________________________________________

stat_sig <- function(data, p_val_col, p_message = FALSE) {
  
  if("tidyverse" %not_in% installed.packages()) {
    
    install.packages("tidyverse")
    
  } 
  
  if("broom" %not_in% installed.packages()) {
    
    install.packages("broom")
    
  }
  
  if("package:tidyverse" %not_in% search()) {
    
    library(tidyverse)
    
  } 
  
  if("package:broom" %not_in% search()) {
    
    library(broom)
    
  }
    
model_out <- data %>% 
    mutate(significance_value = if_else({{p_val_col}} <= 0.05 & {{p_val_col}} > 0.01, "*",
                                              if_else({{p_val_col}} <= 0.01 & {{p_val_col}} > 0.001, "**",
                                                      if_else({{p_val_col}} <= 0.001, "***",
                                                              if_else({{p_val_col}} <= 0.1 & {{p_val_col}} > 0.05, ".", "<>")
                                                              )
                                                      )
                                              )
           )
  
  if(p_message == T) {
  
  cli_h1("P Value Guidance")
  
  cli_inform(c("i" = "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘<>’ 1",
                "*" = "The p-value represents the probability of obtaining results as extreme as, or more extreme than, the observed results under the assumption that the null hypothesis is true. In simpler terms, it tells us the likelihood of seeing the observed data if there is truly no effect or relationship in the population. For example, let's take VARIABLE with a p-value of 0.001. This means that if there were truly no difference in the observed cases in the population (null hypothesis is true), there is a 0.1% chance of observing the difference we see in our data or even more extreme differences."
                )
          )
  
  } else {
    
    cli_h1("P Value Guidance")
    
    cli_inform(c("i" = "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘<>’ 1"))
    
  }

cli_h2("Model Output")

return(model_out)

}
