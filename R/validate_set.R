#' @title Validate Set Equality
#'
#' @description
#' This function checks if all elements of an input are within a specified set
#' of valid values. Depending on the specified type, it will either throw an
#' error, issue a warning, or send a message.
#'
#' @inheritParams validate_numeric
#' @param valid_set A vector of valid values.
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
#' validate_set(data$Trauma_Type,
#' valid_set = c("Blunt", "Penetrating"),
#' type = "warning",
#' na_ok = FALSE,
#' null_ok = FALSE
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_set <- function(
  input,
  valid_set,
  type = c("error", "warning", "message"),
  na_ok = TRUE,
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

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not contain NA values.",
      type = "error"
    )
  }

  # Check if all elements of the input are within the valid set
  invalid_values <- setdiff(x = unique(input), y = valid_set)

  # Check to ensure the invalid_values are not empty
  if (length(invalid_values) > 0) {
    # Call the validate_error_type function to handle the message display
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "contains invalid values: {cli::col_grey(paste0(invalid_values, collapse = ', '))}. Valid values are: {cli::col_blue(paste0(valid_set, collapse = ', '))}"
      ),
      type = type
    )
  }
}
