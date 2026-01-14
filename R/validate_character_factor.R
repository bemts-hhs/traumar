#' @title Validate a Character or Factor Column
#'
#' @description
#' This function checks if a column is of type character or factor. Depending on
#' the specified type, it will either throw an error, issue a warning, or send a
#' message. It also checks for NULL and NA values based on the specified
#' parameters.
#'
#' @inheritParams validate_numeric
#'
#' @return NULL. The function is used for its side effects.
#'
#' @examples
#' # Synthetic data
#' data <- data.frame(
#'   Trauma_Type = c("Blunt", "Penetrating", "Blunt", "Unknown"),
#'   Patient_Age_Years = c(30, 60, 45, 50),
#'   RTS = c(7.84, 6.90, 7.00, 6.50),
#'   ISS = c(10, 25, 15, 20)
#' )
#'
#' # Validate the Trauma_Type column
#' validate_character_factor(data$Trauma_Type, type = "warning", na_ok = FALSE, null_ok = FALSE)
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_character_factor <- function(
  column,
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Get the column name
  column_name <- deparse(substitute(column))

  # Check if the column is NULL
  if (is.null(column)) {
    if (!null_ok) {
      validate_error_type(
        column = column_name,
        message = "must not be NULL.",
        type = "error"
      )
    }
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(column))) {
    validate_error_type(
      column = column_name,
      message = "must not contain NA values.",
      type = "error"
    )
  }

  # Check if the column is character or factor
  if (!is.character(column) && !is.factor(column)) {
    # Call the validate_error_type function to handle the message display
    validate_error_type(
      column = column_name,
      message = "must be of type {.cls character} or {.cls factor}.",
      type = type
    )
  }
}
