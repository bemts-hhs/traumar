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
  if (!is.numeric(p_val_data)) {
    cli::cli_abort(
      "The input {.var p_val_data} must be a numeric vector, but you supplied an object of class {.cls {class(p_val_data)}}."
    )
  }

  # Check if the p-values are between 0 and 1
  if (any(p_val_data < 0 | p_val_data > 1, na.rm = T)) {
    cli::cli_abort("The p-values in {.var p_val_data} must be between 0 and 1.")
  }

  # Assign significance codes based on p-value thresholds
  significance_values <- ifelse(
    p_val_data <= 0.05 & p_val_data > 0.01,
    "*",
    ifelse(
      p_val_data <= 0.01 & p_val_data > 0.001,
      "**",
      ifelse(
        p_val_data <= 0.001,
        "***",
        ifelse(p_val_data <= 0.1 & p_val_data > 0.05, ".", "<>")
      )
    )
  )

  return(significance_values)
}
