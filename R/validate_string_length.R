#' @title Validate String Length
#'
#' @description
#' This function checks if the length of strings in an input is within a
#' specified range. Depending on the specified type, it will either throw an
#' error, issue a warning, or send a message. It also checks for NULL and NA
#' values based on the specified parameters.
#'
#' @inheritParams validate_numeric
#' @param min_length The minimum length of the strings.
#' @param max_length The maximum length of the strings.
#'
#' @return NULL. The function is used for its side effects.
#'
#' @examples
#' # Synthetic data
#' data <- data.frame(
#'   Names = c("Alice", "Bob", "Charlie", "David")
#' )
#'
#' # Validate the Names input
#' validate_string_length(
#'   data$Names,
#'   min_length = 3,
#'   max_length = 7,
#'   type = "warning",
#'   na_ok = FALSE,
#'   null_ok = FALSE
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_string_length <- function(
  input,
  min_length,
  max_length,
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

  # Get required range
  required_range <- glue::glue("[{min_length}, {max_length}]")

  # Check if the string lengths are within the specified range
  if (any(nchar(input) < min_length | nchar(input) > max_length)) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "must have string lengths within range {cli::col_blue(required_range)}."
      ),
      type = type
    )
  }
}
