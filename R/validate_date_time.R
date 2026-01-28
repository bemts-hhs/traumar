#' @title Validate a Date-Time input
#'
#' @description
#' This function checks if an input contains valid date-time objects. Depending
#' on the specified type, it will either throw an error, issue a warning, or
#' send a message. It also checks for NULL and NA values based on the specified
#' parameters.
#'
#' @inheritParams validate_numeric
#'
#' @return NULL. The function is used for its side effects.
#'
#' @examples
#' # Synthetic data
#' data <- data.frame(
#'   Event_Time = c(
#'     "2023-01-01 12:00:00",
#'     "2023-01-02 13:00:00",
#'     NA,
#'     "2023-01-04 15:00:00"
#'   )
#' )
#'
#' # Validate the Event_Time input
#' validate_date_time(
#'   data$Event_Time,
#'   type = "warning",
#'   na_ok = FALSE,
#'   null_ok = FALSE
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_date_time <- function(
  input,
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE,
  var_name = NULL
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Get the input name, optionally using var_name
  if (is.null(var_name)) {
    input_name <- deparse(substitute(input))
  } else {
    # Validate var_name
    validate_character_factor(input = var_name, type = "error")

    # Initialize input_name using var_name
    input_name <- var_name
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

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not contain NA values.",
      type = "error"
    )
  }

  # Check if the input contains valid date-time objects
  if (!all(lubridate::is.POSIXct(input))) {
    # Call the validate_error_type function to handle the message display
    validate_error_type(
      input = input_name,
      message = "must contain valid {.cls date-time} objects.",
      type = type
    )
  }
}
