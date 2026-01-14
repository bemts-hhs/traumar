#' @title Validate Integer Column
#'
#' @description
#' This function checks if a column is integer and optionally checks if the
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
#' # Validate the Patient_Age_Years column
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
  column,
  min = NULL,
  max = NULL,
  na_ok = TRUE,
  null_ok = TRUE,
  finite = FALSE,
  type = c("error", "warning", "message")
) {
  # Validate the type argument
  type <- match.arg(type, choices = c("error", "warning", "message"))

  # Get the column name
  column_name <- deparse(substitute(column))

  # Check if the column is NULL
  if (is.null(column)) {
    if (!null_ok) {
      validate_error_type(
        column = column_name,
        message = "must not be NULL.",
        type = "e"
      )
    }
  }

  # Check if the column is integer
  if (!is.integer(column)) {
    validate_error_type(
      column = column_name,
      message = "must be {.cls integer}.",
      type = "e"
    )
  }

  # Check for finite values if finite is TRUE
  if (finite && any(!is.finite(column), na.rm = TRUE)) {
    validate_error_type(
      column = column_name,
      message = "must contain only finite values.",
      type = "e"
    )
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(column))) {
    validate_error_type(
      column = column_name,
      message = "must not contain NA values.",
      type = "e"
    )
  }

  # Get descriptive statistics on column to provide information in warning
  # messages

  # only take descriptive statistics if limits are requested
  if (!is.null(min) || !is.null(max)) {
    # Define the required range
    required_range <- glue::glue("[{min}, {max}]")

    # get minimum
    observed_min <- min(column, na.rm = TRUE)

    # get max
    observed_max <- max(column, na.rm = TRUE)

    # create a pretty range
    observed_range <- glue::glue("[{observed_min}, {observed_max}]")
  }

  # Check if the column values are within the specified range when only min is
  # provided
  if (!is.null(min) && is.null(max) && any(column < min, na.rm = TRUE)) {
    validate_error_type(
      column = column_name,
      message = glue::glue(
        "values must be greater than or equal to {cli::col_blue(min)}. Observed range of this column was {cli::col_grey(observed_range)}."
      ),
      type = "w"
    )
  }

  # Check if the column values are within the specified range when only max is
  # provided
  if (!is.null(max) && is.null(min) && any(column > max, na.rm = TRUE)) {
    validate_error_type(
      column = column_name,
      message = glue::glue(
        "values must be less than or equal to {cli::col_blue(max)}. Observed range of this column was {cli::col_grey(observed_range)}."
      ),
      type = "w"
    )
  }

  # Check if the column values are within the specified range when min and max
  # are provided
  if (
    !is.null(min) &&
      !is.null(max) &&
      any(column < min | column > max, na.rm = TRUE)
  ) {
    validate_error_type(
      column = column_name,
      message = glue::glue(
        "values must be contained within range {cli::col_blue({required_range})}. Observed range of this column was {cli::col_grey(observed_range)}."
      ),
      type = "w"
    )
  }
}
