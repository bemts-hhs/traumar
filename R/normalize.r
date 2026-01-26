#' @title Normalize a Numeric Vector
#'
#' @description
#'
#' This function normalizes a numeric or integer vector using one of two
#' methods: min-max normalization (scales data to the range (0, 1)) or z-score
#' normalization (centers data around 0 with a standard deviation of 1).
#'
#' @param x A numeric or integer vector to be normalized.
#' @param method A character string specifying the normalization method. Options
#'   are `"min_max"` for min-max normalization or `"z_score"` for z-score
#'   normalization. If no method is provided, the default is `"min_max"`.
#'
#' @returns A numeric vector of the same length as `x`, containing the
#'   normalized values.
#'
#' @export
#'
#' @examples
#' # Example data
#' data <- c(10, 20, 30, 40, 50, NA)
#'
#' # Min-max normalization
#' normalize(data, method = "min_max")
#'
#' # Z-score normalization
#' normalize(data, method = "z_score")
#'
#' # Default behavior (min-max normalization)
#' normalize(data)
#'
#' @author Nicolas Foss, Ed.D., MS
#'
normalize <- function(x, method = c("min_max", "z_score")) {
  # Ensure that x is numeric or integer
  validate_class(input = x, class_type = c("numeric", "integer"), logic = "or")

  # Check for empty input
  if (length(x) < 1) {
    validate_length(input = x, min_length = 1, type = "warning")
    return(numeric(0))
  }
  # Validate method
  method <- match.arg(
    arg = method,
    choices = c("min_max", "z_score"),
    several.ok = FALSE
  )

  # Check if the method is "min_max"
  if (method == "min_max") {
    # Perform min-max normalization
    normalized_data <-
      (x - base::min(x, na.rm = TRUE)) /
      (base::max(x, na.rm = TRUE) - base::min(x, na.rm = TRUE))

    # Check if the method is "z_score"
  } else if (method == "z_score") {
    # Calculate the mean of x, ignoring NA values
    mean_x <- base::mean(x, na.rm = TRUE)

    # Calculate the standard deviation of x, ignoring NA values
    std_dev_x <- stats::sd(x, na.rm = TRUE)

    # Subtract the mean from each value in x
    x_minus_mean <- x - mean_x

    # Perform z-score normalization
    normalized_data <- x_minus_mean / std_dev_x
  }

  # Return the normalized data
  return(normalized_data)
}
