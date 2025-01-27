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
impute <- function(x,
                   focus = c("skew", "missing"),
                   method = c("winsorize", "iqr", "mean", "median"),
                   percentile = NULL) {

  # Check if x is numeric
  if (!is.numeric(x)) {
    cli::cli_abort("`x` must be a numeric vector.")
  }

  if (length(focus) > 1) {

    focus <- "skew"
    cli::cli_alert_success("`focus` is set to '{focus}'.")

  }

  if (focus == "skew" && length(method) > 1) {

    method <- "winsorize"

    cli::cli_alert_success("`method` is set to '{method}'.")


  } else if (focus == "missing" && length(method) > 1) {

    method <- "mean"

    cli::cli_alert_success("`method` is set to '{method}'.")


  }

  # Validate focus
  focus <- base::match.arg(focus)

  # Validate method
  valid_methods <- if (focus == "skew") {

    c("winsorize", "iqr")
  } else if (focus == "missing") {

    c("mean", "median")

    }

  if (!method %in% valid_methods) {
    cli::cli_abort("`method` must be one of: {valid_methods} for `focus = '{focus}'`.")
  }

  # Validate percentile for skew focus and winsorize method
  if (focus == "skew" && method == "winsorize") {
    if (!is.null(percentile) && (!is.numeric(percentile) || percentile <= 0)) {
      cli::cli_abort("`percentile` must be a numeric value between 0 and 0.5 (exclusive).")
    }
  }

  if (focus == "skew") {

    if (method == "winsorize" & is.null(percentile)) { # for extreme values

      upper_limit <- as.numeric(stats::quantile({{x}}, na.rm = TRUE, probs = 0.99))
      lower_limit <- as.numeric(stats::quantile({{x}}, na.rm = TRUE, probs = 0.01))

      imputed_x <- dplyr::if_else({{x}} > upper_limit, upper_limit,
                                  dplyr::if_else({{x}} < lower_limit, lower_limit, {{x}})
      )

    } else if (method == "winsorize" & !is.null(percentile)) {

      upper_limit <- as.numeric(stats::quantile({{x}}, na.rm = TRUE, probs = percentile))
      lower_limit <- as.numeric(stats::quantile({{x}}, na.rm = TRUE, probs = 1 - percentile))

      imputed_x <- dplyr::if_else({{x}} > upper_limit, upper_limit,
                                  dplyr::if_else({{x}} < lower_limit, lower_limit, {{x}})
      )

    } else if (method == "iqr") { # for extreme values

      iqr_x <- stats::IQR({{x}}, na.rm = TRUE)

      upper_quantile <- as.numeric(stats::quantile({{x}}, probs = 0.75, na.rm = TRUE))
      lower_quantile <- as.numeric(stats::quantile({{x}}, probs = 0.25, na.rm = TRUE))

      upper_limit <- upper_quantile + (1.5 * iqr_x)
      lower_limit <- lower_quantile - (1.5 * iqr_x)

      imputed_x <- dplyr::if_else({{x}} > upper_limit, upper_limit,
                                  dplyr::if_else({{x}} < lower_limit, lower_limit, {{x}}
                           )
      )

    }

    } else if (focus == "missing") {

      if(method == "mean") { # only for missing values

      imputed_x <- dplyr::if_else(is.na({{x}}), base::mean({{x}}, na.rm = TRUE), {{x}})

    } else if (method == "median") { # only for missing values

      imputed_x <- dplyr::if_else(is.na({{x}}), stats::median({{x}}, na.rm = TRUE), {{x}})

    }
      }

  return(imputed_x)

}
