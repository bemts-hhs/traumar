#' @title Impute Numeric Column Values
#'
#' @description Cleans numeric columns by handling extreme values or imputing
#'   missing values. The function supports two main focuses: handling skewed
#'   distributions or imputing missing data.
#'
#' @param x A numeric vector to be cleaned.
#' @param focus A character string indicating the focus. Options are:
#'   - `"skew"`: Handle extreme values using percentile or
#'   IQR methods (default).
#'   - `"missing"`: Impute missing values.
#' @param method A character string specifying the method:
#'   - For `focus = "skew"`:
#'     - `"winsorize"`: Replace values outside specified percentiles (default).
#'     - `"iqr"`: Use IQR to limit extreme values.
#'   - For `focus = "missing"`:
#'     - `"mean"`: Replace missing values with the mean.
#'     - `"median"`: Replace missing values with the median.
#' @param percentile A numeric value (percentile > 0) for winsorization.
#'   If not provided, defaults to 0.01 and 0.99.
#'
#' @return A numeric vector with cleaned or imputed values.
#'
#' @examples
#' x <- c(1, 2, 3, 100, 200, NA)
#' # Winsorize to 1% and 99%
#' impute(x, focus = "skew", method = "winsorize")
#'
#' # Replace missing values with the mean
#' impute(x, focus = "missing", method = "mean")
#'
#' @export
#'
impute <- function(
  x,
  focus = c("skew", "missing"),
  method = c("winsorize", "iqr", "mean", "median"),
  percentile = NULL
) {
  # Check if x is numeric
  validate_numeric(input = x, type = "e")

  # Validate focus
  focus <- match.arg(focus, choices = c("skew", "missing"))

  # Ensure that focus is used correctly
  validate_length(input = focus, exact_length = 1, type = "error")

  # Ensure that method is used correctly
  validate_length(input = method, exact_length = 1, type = "error")

  # Dynamic definition of valid method argument input
  valid_methods <- if (focus == "skew") {
    c("winsorize", "iqr")
  } else if (focus == "missing") {
    c("mean", "median")
  }

  # Validate method
  validate_set(input = method, valid_set = valid_methods, type = "error")

  # Validate percentile for skew focus and winsorize method
  if (focus == "skew" && method == "winsorize" && !is.null(percentile)) {
    validate_numeric(
      input = percentile,
      min = 0,
      max = 1,
      na_ok = FALSE,
      null_ok = TRUE,
      type = "error"
    )
  }

  # Check if the focus is on skewness
  if (focus == "skew") {
    # Check if the method is winsorization and the percentile is not provided
    if (method == "winsorize" && is.null(percentile)) {
      # Calculate the upper limit as the 99th percentile of the data
      upper_limit <- as.numeric(stats::quantile(
        {{ x }},
        na.rm = TRUE, # Remove NA values before calculation
        probs = 0.99 # 99th percentile
      ))

      # Calculate the lower limit as the 1st percentile of the data
      lower_limit <- as.numeric(stats::quantile(
        {{ x }},
        na.rm = TRUE, # Remove NA values before calculation
        probs = 0.01 # 1st percentile
      ))

      # Winsorize the data: replace values above the upper limit with the upper
      # limit, and values below the lower limit with the lower limit
      imputed_x <- dplyr::if_else(
        {{ x }} > upper_limit,
        upper_limit,
        dplyr::if_else({{ x }} < lower_limit, lower_limit, {{ x }})
      )

      # Check if the method is winsorization and the percentile is provided
    } else if (method == "winsorize" && !is.null(percentile)) {
      # Calculate the upper limit as the specified percentile of the data
      upper_limit <- as.numeric(stats::quantile(
        {{ x }},
        na.rm = TRUE, # Remove NA values before calculation
        probs = percentile # Specified percentile
      ))

      # Calculate the lower limit as the complement of the specified percentile
      lower_limit <- as.numeric(stats::quantile(
        {{ x }},
        na.rm = TRUE, # Remove NA values before calculation
        probs = 1 - percentile # Complement of the specified percentile
      ))

      # Winsorize the data: replace values above the upper limit with the upper
      # limit, and values below the lower limit with the lower limit
      imputed_x <- dplyr::if_else(
        {{ x }} > upper_limit,
        upper_limit,
        dplyr::if_else({{ x }} < lower_limit, lower_limit, {{ x }})
      )

      # Check if the method is interquartile range (IQR) for extreme values
    } else if (method == "iqr") {
      # Calculate the IQR of the data
      iqr_x <- stats::IQR({{ x }}, na.rm = TRUE)

      # Calculate the upper quantile (75th percentile) of the data
      upper_quantile <- as.numeric(stats::quantile(
        {{ x }},
        probs = 0.75,
        na.rm = TRUE
      ))

      # Calculate the lower quantile (25th percentile) of the data
      lower_quantile <- as.numeric(stats::quantile(
        {{ x }},
        probs = 0.25,
        na.rm = TRUE
      ))

      # Calculate the upper limit as the upper quantile plus 1.5 times the IQR
      upper_limit <- upper_quantile + (1.5 * iqr_x)
      # Calculate the lower limit as the lower quantile minus 1.5 times the IQR
      lower_limit <- lower_quantile - (1.5 * iqr_x)

      # Clip the data: replace values above the upper limit with the upper
      # limit, and values below the lower limit with the lower limit
      imputed_x <- dplyr::if_else(
        {{ x }} > upper_limit,
        upper_limit,
        dplyr::if_else({{ x }} < lower_limit, lower_limit, {{ x }})
      )
    }
  } else if (focus == "missing") {
    # Check if the focus is on missing values

    if (method == "mean") {
      # If the method is mean imputation for missing values

      # Replace missing values with the mean of the data
      imputed_x <- dplyr::if_else(
        is.na({{ x }}),
        base::mean({{ x }}, na.rm = TRUE),
        {{ x }}
      )
    } else if (method == "median") {
      # If the method is median imputation for missing values

      # Replace missing values with the median of the data
      imputed_x <- dplyr::if_else(
        is.na({{ x }}),
        stats::median({{ x }}, na.rm = TRUE),
        {{ x }}
      )
    }
  }

  return(imputed_x)
}
