#' @title Validate Non-Empty Input
#'
#' @description
#' This function checks if the input is non-empty. Depending on the specified
#' type, it will either throw an error, issue a warning, or send a message. It
#' also checks for NULL and NA values based on the specified parameters.
#'
#' @inheritParams validate_numeric
#' @inheritParams validate_data_structure
#' @param data_type A vector of data structure or class types to check. Possible
#' values are "data.frame", "tibble", "matrix", "list", "array", "atomic
#' vector", "numeric", "integer", "logical", "character", "factor", "complex",
#' "raw", "tbl_df", "tbl".
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
#' # Validate the data frame
#' validate_non_empty(
#'   data,
#'   data_type = c("data.frame", "tibble"),
#'   logic = "or",
#'   type = "warning",
#'   na_ok = FALSE,
#'   null_ok = FALSE
#' )
#'
#' # Validate an empty vector
#' validate_non_empty(
#'   character(0),
#'   data_type = c("vector"),
#'   logic = "or",
#'   type = "error",
#'   na_ok = TRUE,
#'   null_ok = TRUE
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_non_empty <- function(
  input,
  data_type = c(
    "data.frame",
    "tibble",
    "matrix",
    "list",
    "array",
    "atomic vector",
    "numeric",
    "integer",
    "logical",
    "character",
    "factor",
    "complex",
    "raw",
    "tbl_df",
    "tbl"
  ),
  logic = c("and", "or"),
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Validate the data_type argument
  data_type <- match.arg(
    arg = data_type,
    choices = c(
      "data.frame",
      "tibble",
      "matrix",
      "list",
      "array",
      "atomic vector",
      "numeric",
      "integer",
      "logical",
      "character",
      "factor",
      "complex",
      "raw",
      "tbl_df",
      "tbl"
    ),
    several.ok = TRUE
  )

  # Validate the logic argument
  logic <- match.arg(arg = logic, choices = c("and", "or"))

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

  # Perform the checks for each specified data structure type
  checks <- sapply(data_type, function(type) {
    switch(
      type,
      "data.frame" = nrow(input) > 0 && ncol(input) > 0,
      "matrix" = nrow(input) > 0 && ncol(input) > 0,
      "list" = length(input) > 0,
      "array" = length(input) > 0,
      "atomic vector" = length(input) > 0,
      "numeric" = length(input) > 0,
      "integer" = length(input) > 0,
      "logical" = length(input) > 0,
      "character" = length(input) > 0,
      "factor" = length(input) > 0,
      "complex" = length(input) > 0,
      "raw" = length(input) > 0,
      "tbl_df" = nrow(input) > 0 && ncol(input) > 0,
      "tbl" = nrow(input) > 0 && ncol(input) > 0
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
        "must be non-empty and of type {cli::col_blue(paste0('(', paste0(data_type, collapse = ', '), ')'))}."
      ),
      type = type
    )
  }
}
