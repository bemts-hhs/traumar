#' Normalize a Numeric Vector
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


  if (!is.numeric(x) && !is.integer(x)) {

    cli::cli_abort("Input must be {.cls numeric} or {.cls integer}. You supplied an object of class {.cls {class(x)}} to {.fn normalize}.")

  }

  # Check for empty input
  if (length(x) == 0) {
    cli::cli_alert_info("Input is empty; returning an empty numeric vector.")
    return(numeric(0))
  }

  if(length(method) > 1) {

    method <- "min_max"

    cli::cli_alert_info("As no method was supplied, {.fn normalize} will default to min-max normalization methods.")
  }

  if(method == "min_max") {

  normalized_data <-
    (x - base::min(x, na.rm = TRUE)) / (base::max(x, na.rm = TRUE) - base::min(x, na.rm = TRUE))

  } else if(method == "z_score") {

    mean_x <- base::mean(x, na.rm = T)

    std_dev_x <- stats::sd(x, na.rm = T)

    x_minus_mean <- x - mean_x

    normalized_data <- x_minus_mean / std_dev_x

  }

  return(normalized_data)

}
