#' @title Validate Column Names
#'
#' @description
#' This function checks if all column names of a data frame or tibble are within
#' a specified set of valid values. Depending on the specified type, it will
#' either throw an error, issue a warning, or send a message.
#'
#' @inheritParams validate_numeric
#' @param input A data.frame or tibble. validate_names() will run
#' colnames(input) to get the expected column names.
#' @param check_names A vector of column names as strings to check against
#' input.
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
#' # Validate the column names
#' validate_names(
#'   input = data,
#'   check_names = c("Trauma_Type", "Patient_Age_Years", "RTS", "ISS"),
#'   type = "warning",
#'   na_ok = FALSE,
#'   null_ok = FALSE
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_names <- function(
  input,
  check_names,
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE,
  var_name = NULL
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Get the input name, optionally using var_name
  if (is.null(var_name)) {
    input_name <- rlang::as_name(rlang::enquo(check_names))
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
      message = "must not be a missing value.",
      type = "error"
    )
  }

  # Validate the valid_set input
  validate_data_structure(
    input = input,
    structure_type = c("data.frame", "tibble"),
    logic = "or",
    type = "error"
  )

  # Validate check_names, ensure it has class character
  validate_character_factor(input = check_names, type = "error")

  # Get the column names of the target data
  valid_set <- colnames(input)

  # Check if all column names are within the valid set
  invalid_values <- setdiff(x = check_names, y = valid_set)

  # Check to ensure the invalid_values are not empty
  if (length(invalid_values) > 0) {
    if (length(valid_set) <= 10) {
      # Call the validate_error_type function to handle the message display
      # For small valid_set
      validate_error_type(
        input = input_name,
        message = glue::glue(
          "contains invalid column names: {cli::col_grey(paste0('(', paste0(invalid_values, collapse = ', '), ')'))}. Valid column names are: {cli::col_blue(paste0('(', paste0(valid_set, collapse = ', '), ')'))}"
        ),
        type = type
      )
    } else {
      # Call the validate_error_type function to handle the message display
      # For large valid_set

      # Clip valid_set down to a length of <= 10
      valid_set <- head(valid_set, 10)

      # Modified messaging
      validate_error_type(
        input = input_name,
        message = glue::glue(
          "contains invalid column names: {cli::col_grey(paste0('(', paste0(invalid_values, collapse = ', '), ')'))}. Some examples of valid column names are: {cli::col_blue(paste0('(', paste0(valid_set, collapse = ', '), ',...', ')'))}"
        ),
        type = type
      )
    }
  }
}
