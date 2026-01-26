#' @title Validate Class
#'
#' @description
#' This function checks if the input is of the specified class type(s).
#' Depending on the specified type, it will either throw an error, issue a
#' warning, or send a message. It also checks for NULL and NA values based on
#' the specified parameters.
#'
#' @inheritParams validate_numeric
#' @inheritParams validate_data_structure
#' @param class_type A vector of class types to check. Possible values are
#' "numeric", "integer", "logical", "character", "factor", "complex", "raw".
#' @param finite Logical. If TRUE, only finite values are allowed. Default is
#' FALSE.
#'
#' @return NULL. The function is used for its side effects.
#'
#' @examples
#' # Synthetic data
#' data <- data.frame(
#'   a = 1:5,
#'   b = 1:10,
#'   c = 1:15
#' )
#'
#' # Validate the class of the data frame columns
#' validate_class(
#'   data$a,
#'   class_type = c("numeric"),
#'   type = "warning",
#'   na_ok = FALSE,
#'   null_ok = FALSE,
#'   finite = TRUE
#' )
#'
#' validate_class(
#'   data$b,
#'   class_type = c("integer"),
#'   type = "error",
#'   na_ok = TRUE,
#'   null_ok = TRUE,
#'   finite = FALSE
#' )
#'
#' validate_class(
#'   data$c,
#'   class_type = c("factor"),
#'   type = "message",
#'   na_ok = TRUE,
#'   null_ok = TRUE,
#'   finite = FALSE
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_class <- function(
  input,
  class_type = c(
    "numeric",
    "integer",
    "logical",
    "character",
    "factor",
    "complex",
    "raw"
  ),
  logic = c("and", "or"),
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE,
  finite = FALSE,
  var_name = NULL
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Validate the class_type argument
  class_type <- match.arg(
    arg = class_type,
    choices = c(
      "numeric",
      "integer",
      "logical",
      "character",
      "factor",
      "complex",
      "raw"
    ),
    several.ok = TRUE
  )

  # Validate the logic argument
  logic <- match.arg(arg = logic, choices = c("and", "or"))

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

  # Check for finite values if finite is TRUE and the input is numeric or integer
  if (
    finite &&
      (is.numeric(input) || is.integer(input)) &&
      any(!is.finite(input), na.rm = TRUE)
  ) {
    validate_error_type(
      input = input_name,
      message = "must contain only finite values.",
      type = "error"
    )
  }

  # Perform the checks for each specified class type
  checks <- sapply(class_type, function(type) {
    switch(
      type,
      "numeric" = is.numeric(input),
      "integer" = is.integer(input),
      "logical" = is.logical(input),
      "character" = is.character(input),
      "factor" = is.factor(input),
      "complex" = is.complex(input),
      "raw" = is.raw(input)
    )
  })

  # Combine the results based on the logic argument
  is_valid <- if (logic == "and") {
    sum(checks) == length(checks)
  } else {
    sum(checks) >= 1
  }

  # If the input is not valid, display an error, warning, or message
  if (!is_valid) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "must be of class {cli::col_blue(paste0('(', paste(class_type, collapse = ', '), ')'))}."
      ),
      type = type
    )
  }
}
