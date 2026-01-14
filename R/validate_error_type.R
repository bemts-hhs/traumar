#' @title Validate Error Type
#'
#' @description
#' This function displays an error, warning, or message based on the specified
#' type.
#'
#' @inheritParams validate_numeric
#' @param message The message to be displayed.
#'
#' @return NULL. The function is used for its side effects.
#'
validate_error_type <- function(
  input,
  message,
  type = c("error", "warning", "message")
) {
  # Validate the type argument
  type <- match.arg(type, choices = c("error", "warning", "message"))

  # Create the dynamic message using glue
  dynamic_message <- glue::glue("{cli::col_cyan(input)}: {message}")

  # Depending on the type, use the appropriate cli function
  if (type == "error") {
    cli::cli_abort(message = dynamic_message)
  } else if (type == "warning") {
    cli::cli_warn(message = dynamic_message)
  } else if (type == "message") {
    cli::cli_inform(message = dynamic_message)
  }
}
