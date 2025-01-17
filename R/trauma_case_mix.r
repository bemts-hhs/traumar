##################################################
### View the current patient population case mix #
### compared to the MTOS case mix. ###############
##################################################

trauma_case_mix <- function(df, Ps_col, outcome_col) {
  
  ### Ensure dplyr, rlang, and cli are installed and loaded
  if (!"dplyr" %in% installed.packages()) {
    install.packages("dplyr")
  }
  if (!"package:dplyr" %in% search()) {
    library(dplyr)
  }
  
  if (!"rlang" %in% installed.packages()) {
    install.packages("rlang")
  }
  if (!"package:rlang" %in% search()) {
    library(rlang)
  }
  
  # Evaluate column names passed in
  Ps_col <- rlang::enquo(Ps_col)
  outcome_col <- rlang::enquo(outcome_col)
  
  # Check if the dataframe is valid
  if (!is.data.frame(df)) {
    cli::cli_abort("The first argument must be a dataframe.")
  }
  
  # Check if the outcome_col is binary
  binary_data <- df %>%
    dplyr::select(!!outcome_col) %>%
    dplyr::pull()
  
  binary_col <- length(unique(na.omit(binary_data)))
  
  if(binary_col > 2 | binary_col < 2) {
    
    cli::cli_abort("The {.var outcome_col} must be binary, such as 1/0, TRUE/FALSE, 'Yes'/'No', etc.  Check the column and ensure a binary structure.")
    
  }
  
  # Check if Ps column is numeric
  Ps_data <- df %>% dplyr::pull(!!Ps_col)
  if (!is.numeric(Ps_data)) {
    cli::cli_abort("The probability of survival (Ps) column must be numeric.")
  }
  
  # Check if Ps column is continuous (values between 0 and 1 or 0 and 100)
  if (any(Ps_data < 0 | Ps_data > 100)) {
    cli::cli_abort("The probability of survival (Ps) values must be between 0 and 100.")
  }
  
  # Notify the user if any conversions were made
  if (any(Ps_data > 1)) {
    cli::cli_alert_info("Some Probability of survival (Ps) values will be divided by 100 to convert to decimal format.")
  }
  
  # Convert ##.## format to decimal if needed (rowwise operation but vectorized)
  Ps_data <- if_else(Ps_data > 1, Ps_data / 100, Ps_data)
  
  # Check if Ps column is continuous (values between 0 and 1 or 0 and 100)
  if (any(Ps_data < 0 | Ps_data > 100)) {
    cli::cli_abort("The probability of survival (Ps) values must be between 0 and 100.")
  }
  
  Ps_range_order <- c("0.96 - 1.00", "0.91 - 0.95", "0.76 - 0.90", "0.51 - 0.75", "0.26 - 0.50", "0.00 - 0.25")
  
  # Define the MTOS Ps distribution
  MTOS_distribution <- dplyr::tibble(Ps_range = factor(c("0.96 - 1.00", 
                                                  "0.91 - 0.95", 
                                                  "0.76 - 0.90", 
                                                  "0.51 - 0.75", 
                                                  "0.26 - 0.50", 
                                                  "0.00 - 0.25"
  ), levels = Ps_range_order
  ),
  MTOS_distribution = c(0.842, 0.053, 0.052, 0, 0.043, 0.01)
  )
  
  # Bin patients into Ps ranges and calculate current fractions
  fractions_set <- df %>%
    dplyr::filter(!is.na(!!Ps_col) & !is.na(!!outcome_col)) %>%
    dplyr::mutate(
      !!Ps_col := Ps_data,
      Ps_range = case_when(
        !!Ps_col >= 0.96 ~ "0.96 - 1.00",
        !!Ps_col >= 0.91 ~ "0.91 - 0.95",
        !!Ps_col >= 0.76 ~ "0.76 - 0.90",
        !!Ps_col >= 0.51 ~ "0.51 - 0.75",
        !!Ps_col >= 0.26 ~ "0.26 - 0.50",
        TRUE ~ "0.00 - 0.25"
      )
    ) %>%
    dplyr::summarize(current_fraction = n() / nrow(df), 
              .by = Ps_range
    ) %>%
    dplyr::left_join(MTOS_distribution, by = "Ps_range") %>%
    dplyr::arrange(Ps_range)
  
  return(fractions_set)
  
}
