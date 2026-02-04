#' @title Assign Significance Codes Based on P-Values
#'
#' @description
#'
#' This function assigns significance codes to a p-value vector based on commonly
#' accepted significance thresholds. The significance codes are:
#' - `"***"` for p-values <= 0.001
#' - `"**"` for p-values <= 0.01 and > 0.001
#' - `"*"` for p-values <= 0.05 and > 0.01
#' - `"."` for p-values <= 0.1 and > 0.05
#' - `"<>"` for p-values > 0.1
#'
#' @param p_val_data A numeric vector representing the p-values to be
#'   categorized. The vector should contain p-values between 0 and 1.
#'
#' @returns A character vector with the assigned significance codes for each
#'   p-value.
#'
#' @export
#'
#' @examples
#' # Example usage of the stat_sig function
#' data <- data.frame(p_value = c(0.001, 0.03, 0.12, 0.05, 0.07))
#'
#' data |>
#'   dplyr::mutate(significance = stat_sig(p_val_data = p_value))
#'
#' @author Nicolas Foss, Ed.D., MS
#'
stat_sig <- function(p_val_data) {
  # Check if the input is numeric
  validate_numeric(
    input = p_val_data,
    min = 0,
    max = 1,
    type = "error",
    null_ok = FALSE
  )

  # Assign significance codes based on p-value thresholds
  significance_values <- ifelse(
    # Assign NA if p_val_data is NA or NULL
    is.na(p_val_data) | is.null(p_val_data),
    NA,
    # Check if p-value is less than or equal to 0.001
    ifelse(
      p_val_data <= 0.001,
      "***", # Assign "***" for p-values <= 0.001
      ifelse(
        p_val_data <= 0.01,
        "**", # Assign "**" for p-values <= 0.01
        ifelse(
          p_val_data <= 0.05,
          "*", # Assign "*" for p-values <= 0.05
          ifelse(
            p_val_data <= 0.1,
            ".", # Assign "." for p-values <= 0.1
            "<>" # Assign "<>" for p-values > 0.1
          )
        )
      )
    )
  )

  # End function
  return(significance_values)
}
