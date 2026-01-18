#' @title Validate a Character or Factor Input
#'
#' @description
#' This function checks if an input is of type character or factor. Depending on
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
#' # Validate the Trauma_Type input
#' validate_character_factor(
#'   data$Trauma_Type,
#'   type = "warning",
#'   na_ok = FALSE,
#'   null_ok = FALSE
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_character_factor <- function(
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
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not contain NA values.",
      type = "error"
    )
  }

  # Check if the input is character or factor
  if (!is.character(input) && !is.factor(input)) {
    # Call the validate_error_type function to handle the message display
    validate_error_type(
      input = input_name,
      message = "must be of type {.cls character} or {.cls factor}.",
      type = type
    )
  }
}
