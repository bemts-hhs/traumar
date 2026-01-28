#' @title Validate Integer input
#'
#' @description
#' This function checks if an input is integer and optionally checks if the
#' values are within a specified range. Depending on the specified type, it will
#' either throw an error, issue a warning, or send a message. Additional
#' arguments allow for checking NA values, NULL values, and finite values.
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
#' # Validate the Patient_Age_Years input
#' validate_integer(data$Patient_Age_Years,
#'   min = 0,
#'   max = 100,
#'   na_ok = TRUE,
#'   null_ok = FALSE,
#'   finite = FALSE,
#'   type = "warning"
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_integer <- function(
  input,
  min = NULL,
  max = NULL,
  na_ok = TRUE,
  null_ok = TRUE,
  finite = FALSE,
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

  # Check if the input is integer
  if (!is.integer(input)) {
    validate_error_type(
      input = input_name,
      message = "must be {.cls integer}.",
      type = type
    )
  }

  # Check for finite values if finite is TRUE
  if (finite && any(!is.finite(input), na.rm = TRUE)) {
    validate_error_type(
      input = input_name,
      message = "must contain only finite values.",
      type = "e"
    )
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not contain NA values.",
      type = "e"
    )
  }

  # Get descriptive statistics on input to provide information in a message
  # only take descriptive statistics if limits are requested
  if (!is.null(min) || !is.null(max)) {
    # Define the required range
    required_range <- glue::glue("[{min}, {max}]")

    # Get unique values of the input
    unique_input <- unique(input)

    # Check the length of the unique values
    if (length(unique_input) > 1) {
      # get minimum
      observed_min <- min(input, na.rm = TRUE)

      # get max
      observed_max <- max(input, na.rm = TRUE)

      # create a pretty range
      observed_range <- glue::glue("[{observed_min}, {observed_max}]")

      # dynamnic text
      dynamic_text <- "Range"
    } else {
      # If only one unique value, use that value for the message
      observed_range <- unique_input

      # dynamnic text
      dynamic_text <- "Value"
    }
  }

  # Check if the input values are within the specified range when only min is
  # provided
  if (!is.null(min) && is.null(max) && any(input < min, na.rm = TRUE)) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "values must be greater than or equal to {cli::col_blue(min)}. {dynamic_text} of this input was {cli::col_grey(observed_range)}."
      ),
      type = type
    )
  }

  # Check if the input values are within the specified range when only max is
  # provided
  if (!is.null(max) && is.null(min) && any(input > max, na.rm = TRUE)) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "values must be less than or equal to {cli::col_blue(max)}. {dynamic_text} of this input was {cli::col_grey(observed_range)}."
      ),
      type = type
    )
  }

  # Check if the input values are within the specified range when min and max
  # are provided
  if (
    !is.null(min) &&
      !is.null(max) &&
      any(input < min | input > max, na.rm = TRUE)
  ) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "values must be contained within range {cli::col_blue({required_range})}. {dynamic_text} of this input was {cli::col_grey(observed_range)}."
      ),
      type = type
    )
  }
}
