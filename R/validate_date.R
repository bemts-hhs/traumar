#' @title Validate Date input
#'
#' @description
#' This function checks if an input contains valid dates and optionally checks
#' if the values are within a specified range. Depending on the specified type,
#' it will either throw an error, issue a warning, or send a message. Additional
#' arguments allow for checking NA values and NULL values.
#'
#' @inheritParams validate_numeric
#'
#' @return NULL. The function is used for its side effects.
#'
#' @examples
#' # Synthetic data
#' data <- data.frame(
#'   Date_input = as.Date(c("2023-01-01", "2023-06-15", "2023-12-31", NA))
#' )
#'
#' # Validate the Date_input
#' validate_date(data$Date_input,
#'   min = as.Date("2023-01-01"),
#'   max = as.Date("2023-12-31"),
#'   na_ok = TRUE,
#'   null_ok = FALSE,
#'   type = "warning"
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
validate_date <- function(
  input,
  na_ok = TRUE,
  null_ok = TRUE,
  type = c("error", "warning", "message"),
  var_name = NULL
) {
  # Validate the type argument
  type <- match.arg(type, choices = c("error", "warning", "message"))

  # Get the input name, optionally using var_name
  if (is.null(var_name)) {
    input_name <- deparse(substitute(input))
  } else {
    # Validate var_name
    validate_character_factor(input = var_name, type = "error")

    # Initialize input_name using var_name
    input_name <- var_name
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not contain NA values.",
      type = "e"
    )
  }

  # Check if the input is NULL
  if (is.null(input)) {
    if (!null_ok) {
      validate_error_type(
        input = input_name,
        message = "must not be NULL.",
        type = "error"
      )
    }
    return(NULL)
  }

  # Check if the input is a valid date
  if (!all(lubridate::is.Date(input), na.rm = TRUE)) {
    validate_error_type(
      input = input_name,
      message = "must be a valid {.cls Date}.",
      type = type
    )
  }
}
