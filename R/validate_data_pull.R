#' @title Validate Data Extraction
#'
#' @description
#' This function extracts a column from a data frame or tibble and returns it as
#' a vector. If the column does not exist or an error occurs, it returns a clean
#' error message using the cli package.
#'
#' @param data A data frame or tibble.
#' @param col The column to be extracted.
#' @param var_name Optional. The name of the variable for error messaging.
#' @param calls Optional. The number of callers to go back in the call stack for
#' error messaging.
#'
#' @return The extracted column as a vector.
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
#' # Validate and extract the Trauma_Type column
#' validate_data_pull(
#'   data,
#'   Trauma_Type,
#'   var_name = "Trauma_Type",
#'   calls = 4
#' )
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_data_pull <- function(data, col, var_name = NULL, calls = NULL) {
  # Get the column name, optionally using var_name
  if (is.null(var_name)) {
    col_name <- deparse(substitute(col))
  } else {
    col_name <- var_name
  }

  # Define number of callers to go back
  calls <- ifelse(is.null(calls), 4, calls)

  # Extract the column and handle errors
  result <- tryCatch(
    {
      data |> dplyr::pull({{ col }})
    },
    error = function(e) {
      cli::cli_abort(
        message = "It was not possible to validate `{cli::col_blue(col_name)}`, please check this column in the function call.",
        call = rlang::caller_call(n = calls)
      )
    }
  )

  # Return extracted vector
  return(result)
}
