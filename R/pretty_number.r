#' @title Convert Numbers into Readable Abbreviated Formats
#'
#' @description
#'
#' This function converts large numeric values into readable abbreviated formats
#' (e.g., 1,000 becomes "1k") with options for rounding, decimal precision, and
#' a custom prefix. It supports numbers up to the decillion range.
#'
#' @param x A numeric value or vector to be converted into a readable format.
#' @param n_decimal An integer specifying the number of decimal places to
#'   include in the output. Defaults to `2`.
#' @param prefix An optional character string to prepend to the formatted number
#'   (e.g., "$"). Defaults to `NULL`.
#' @param truncate A logical value indicating whether to truncate the numbers before
#'   formatting. Defaults to `FALSE`.
#'
#' @returns A character vector with the numbers formatted as abbreviated
#'   strings. If `prefix` is provided, it prepends the formatted numbers.
#'
#' @export
#'
#' @examples
#' # Basic usage
#' pretty_number(1234)               # "1.23k"
#' pretty_number(1234567)            # "1.23m"
#' pretty_number(1234567890)         # "1.23b"
#'
#' # Adjusting decimal places
#' pretty_number(1234, n_decimal = 1) # "1.2k"
#'
#' # Adding a prefix
#' pretty_number(1234, prefix = "$")  # "$1.23k"
#'
#' # Without rounding
#' pretty_number(1250, truncate = TRUE) # "1.2k"
#'
#' @author Nicolas Foss, Ed.D., MS
#'
pretty_number <- function(x, n_decimal = 2, prefix = NULL, truncate = FALSE) {
  # Error check: Ensure `x` is numeric or integer
  if (!is.numeric(x) && !is.integer(x)) {
    cli::cli_abort(c(
      "x must be either {.cls numeric} or {.cls integer}.",
      "x is of class {.cls {class(x)}}."
    ))
  }

  # Error check: Ensure `n_decimal` is an integer
  if (
    !is.integer(n_decimal) &&
      !(is.numeric(n_decimal) && n_decimal == floor(n_decimal))
  ) {
    cli::cli_abort(c(
      "n_decimal must be an {.cls integer}.",
      "You supplied a value of class {.cls {class(n_decimal)}} with value {.val {n_decimal}}."
    ))
  }

  # Error check: Ensure that `truncate` is logical
  if (!is.logical(truncate)) {
    cli::cli_abort(c(
      "{.var truncate} must be of class {.cls logical}.",
      "You supplid a value of class {.cls {class(truncate)}} with value {.val {truncate}}."
    ))
  }

  if (!is.null(prefix) && !is.character(prefix)) {
    cli::cli_abort(c(
      "The object you passed to prefix was of class {.cls {class(prefix)}}",
      "i" = "You must supply a {.cls character} vector of length 1 for the prefix argument of {.fn pretty_number} to work."
    ))
  }

  old <- options()
  on.exit(options(old))

  options(scipen = 9999)

  # set values to different orders of magnitude
  thousand <- 1e3
  million <- 1e6
  billion <- 1e9
  trillion <- 1e12
  quadrillion <- 1e15
  quintillion <- 1e18
  sextillion <- 1e21
  septillion <- 1e24
  octillion <- 1e27
  nonillion <- 1e30
  decillion <- 1e33

  # allow a toggle to truncate numbers so you can round something like
  # 555555 to "600k" with
  # > pretty_number(555555, truncate = T, n_decimal = 1)
  #[1] "600k"
  if (truncate) {
    # When truncation is enabled, truncate to significant digits for consistent output
    x <- signif(x, digits = n_decimal)

    # Get the number of characters in the truncated value
    x_length <- nchar(trunc(x))
  } else {
    # When truncation is disabled, do not use round function
    x <- round(x, digits = n_decimal)

    # Get the number of characters in the value
    x_length <- nchar(trunc(x))
  }

  # classify x

  suffix <- c("k", "m", "b", "t", "qd", "qt", "sxt", "spt", "oct", "non", "dec")

  x_val <- ifelse(
    x_length %in% 4:6,
    paste0(round(x / thousand, digits = n_decimal), suffix[1]),
    ifelse(
      x_length %in% 7:9,
      paste0(round(x / million, digits = n_decimal), suffix[2]),
      ifelse(
        x_length %in% 10:12,
        paste0(round(x / billion, digits = n_decimal), suffix[3]),
        ifelse(
          x_length %in% 13:15,
          paste0(round(x / trillion, digits = n_decimal), suffix[4]),
          ifelse(
            x_length %in% 16:18,
            paste0(round(x / quadrillion, digits = n_decimal), suffix[5]),
            ifelse(
              x_length %in% 19:21,
              paste0(round(x / quintillion, digits = n_decimal), suffix[6]),
              ifelse(
                x_length %in% 22:24,
                paste0(round(x / sextillion, digits = n_decimal), suffix[7]),
                ifelse(
                  x_length %in% 25:27,
                  paste0(round(x / septillion, digits = n_decimal), suffix[8]),
                  ifelse(
                    x_length %in% 28:30,
                    paste0(round(x / octillion, digits = n_decimal), suffix[9]),
                    ifelse(
                      x_length %in% 31:33,
                      paste0(
                        round(x / nonillion, digits = n_decimal),
                        suffix[10]
                      ),
                      ifelse(
                        x_length %in% 34:36,
                        paste0(
                          round(x / decillion, digits = n_decimal),
                          suffix[11]
                        ),
                        paste0(round(x, digits = n_decimal))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  if (is.null(prefix)) {
    return(x_val)
  }

  x_val <- paste0(prefix, x_val)

  return(x_val)
}
