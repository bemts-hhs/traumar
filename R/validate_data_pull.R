#' @title Validate Data Extraction
#'
#' @description
#' This function extracts a column from a data frame or tibble and returns it as
#' a vector. If the column does not exist or an error occurs, it returns a clean
#' error message using the cli package.
#'
#' @param input A data frame or tibble.
#' @param col The column to be extracted.
#' @param var_name Optional. The name of the variable for error messaging.
#' @param calls Optional. The number of callers to go back in the call stack for
#' error messaging. If NULL, will default to 5.
#'
#' @return The extracted column as a vector.
#'
#' @details
#' This function is designed to validate and extract a specified column from a
#' data frame or tibble. When using `validate_data_pull()` within custom
#' functions, it is necessary to call a given bare column name  using tidy
#' evaluation (e.g., `{{ col }}`). This allows the function to correctly capture
#' and evaluate the column name within the custom function. However, when
#' calling this function directly on a data frame, tidy evaluation is not
#' required.
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
#'   calls = 5
#' )
#'
#' # Example of using validate_data_pull() within a custom function
#' custom_function <- function(df, col) {
#'   # Validate and extract the column using tidy evaluation
#' extracted_col <- validate_data_pull(
#'   df,
#'   {{ col }},
#'   var_name = deparse(substitute(col)),
#'   calls = 3
#' )
#'   return(extracted_col)
#' }
#'
#' # Call the custom function with the Trauma_Type column
#' custom_function(df = data, col = Trauma_Type)
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_data_pull <- function(input, col, var_name = NULL, calls = NULL) {
  # Get the column name, optionally using var_name
  if (is.null(var_name)) {
    col_name <- deparse(substitute(col))
  } else {
    col_name <- var_name
  }

  # Define number of callers to go back
  calls <- ifelse(is.null(calls), 5, calls)

  # Extract the column and handle errors
  result <- tryCatch(
    {
      input |> dplyr::pull({{ col }})
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
