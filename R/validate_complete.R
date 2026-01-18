#' @title Validate Complete Input
#'
#' @description
#' This function checks if the input contains any missing values (NA). Depending
#' on the specified type, it will either throw an error, issue a warning, or
#' send a message. It also checks for NULL values based on the specified
#' parameters.
#'
#' @inheritParams validate_numeric
#'
#' @return NULL. The function is used for its side effects.
#'
#' @examples
#' # Synthetic data
#' data <- data.frame(
#'   a = c(1, 2, NA, 4, 5),
#'   b = c(1, NA, 3, 4, 5),
#'   c = c(1, 2, 3, 4, 5)
#' )
#'
#' # Validate the data frame
#' validate_complete(data, type = "warning", null_ok = FALSE)
#'
#' # Validate a vector with missing values
#' validate_complete(c(1, 2, NA, 4, 5), type = "error", null_ok = TRUE)
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_complete <- function(
  input,
  type = c("error", "warning", "message"),
  null_ok = TRUE
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Get the input name
  input_name <- deparse(substitute(input))

  # Check if the input is NULL
  if (is.null(input)) {
    if (!null_ok) {
      validate_error_type(
        input = input_name,
        message = "must not be NULL.",
        type = "error"
      )
    }
  }

  # Check for missing values
  missing_count <- sum(is.na(input))

  # Get total values
  total_values <- sum(!is.na(input)) + missing_count

  # Proportion missing
  prop_missing <- paste0(
    round(missing_count / total_values, digits = 4) * 100,
    "%"
  )

  if (missing_count > 0) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "missing values detected. Found {missing_count} missing value(s) out of {total_values} total values for {prop_missing} global missingness."
      ),
      type = type
    )
  }
}
