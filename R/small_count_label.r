#' @title Label Small Counts Based on a Cutoff
#'
#' @description
#'
#' This function labels values in a vector as a replacement string if they are
#' smaller than a specified cutoff. The input can be numeric, and the function
#' will return either a modified version of the input vector with small values
#' replaced by a given label, or it will keep the original values otherwise.
#'
#' @param var A numeric vector. This represents the variable to be checked
#'   against the cutoff.
#' @param cutoff A numeric value representing the threshold. Values in `var`
#'   smaller than this value will be replaced.
#' @param replacement A string or a numeric value. If the value in `var` is
#'   smaller than the `cutoff`, this value will replace it. If a string is
#'   provided, it will replace the numeric values with the string. If a numeric
#'   value is provided, the replacement will also be numeric.
#'
#' @returns A vector with values from `var`. Values smaller than the `cutoff`
#'   will be replaced by the `replacement`. If `replacement` is a string, the
#'   return type will be character, otherwise, it will remain numeric.
#'
#' @export
#'
#' @examples
#' # Example usage of the small_count_label function
#' small_count_label(c(1, 5, 10), 5, "Below Cutoff")
#' small_count_label(c(1, 5, 10), 5, 0)
#'
#' @author Nicolas Foss, Ed.D., MS
#'
small_count_label <- function(var, cutoff, replacement) {
  # Check if var is numeric ----
  validate_numeric(input = var, type = "error")

  # Check if cutoff is numeric ----
  validate_numeric(input = cutoff, type = "error")

  # Ensure cutoff has length >= 1 ----
  validate_length(input = cutoff, exact_length = 1, type = "error")

  # Check if replacement is of a valid type ----
  validate_class(
    input = replacement,
    class_type = c("numeric", "character", "logical"),
    logic = "or",
    type = "error",
    na_ok = TRUE
  )

  # Perform the replacement based on the cutoff ----
  # Check if all values in 'var' are greater than or equal to the `cutoff`
  if (all(var >= cutoff, na.rm = TRUE)) {
    # If true, keep the original `var` as `output`
    output <- var
  } else {
    # Otherwise, replace values in `var` that are less than `cutoff` with
    # `replacement`
    output <- ifelse(var < cutoff, replacement, var)
  }

  # End function ----
  return(output)
}
