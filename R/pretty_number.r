#' @title Convert Numbers into Readable Abbreviated Formats
#'
#' @description
#'
#' This function converts large numeric values into readable abbreviated formats
#' (e.g., 1,000 becomes "1k") with options for rounding, decimal precision, and
#' a custom prefix. It supports numbers up to the decillion range.
#'
#' @param x A numeric value or vector to be converted into a readable format.
#' @param digits Number of decimal places to display. Defaults to 2.
#' @param n_decimal `r lifecycle::badge("deprecated")` Use `digits` instead.
#' @param prefix An optional character string to prepend to the formatted number
#' (e.g., "$"). Defaults to `NULL`.
#' @param truncate A logical value indicating whether to truncate the numbers
#' before formatting. When `TRUE`, the function uses `base::signif()` to
#' truncate the numbers to the specified number of significant digits, making
#' the output more concise. When `FALSE`, the function uses `base::round()` to
#' round the numbers to the specified number of decimal places, preserving the
#' original scale of the number. Defaults to `FALSE`.
#'
#' @returns A character vector with the numbers formatted as abbreviated
#' strings. If `prefix` is provided, it prepends the formatted numbers.
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
#' pretty_number(1234, digits = 1) # "1.2k"
#'
#' # Adding a prefix
#' pretty_number(1234, prefix = "$")  # "$1.23k"
#'
#' # Without rounding
#' pretty_number(1250, truncate = TRUE) # "1.2k"
#'
#' @author Nicolas Foss, Ed.D., MS
#'
pretty_number <- function(
  x,
  digits = 2,
  n_decimal = deprecated(),
  prefix = NULL,
  truncate = FALSE
) {
  # Handle deprecated n_decimal argument
  if (lifecycle::is_present(n_decimal)) {
    # Issue a warning
    lifecycle::deprecate_warn(
      when = "1.2.4",
      what = "pretty_number(n_decimal)",
      with = "pretty_number(digits)"
    )
    # Handle the n_decimal value for back compatability
    digits <- n_decimal
  }

  # Enforce numeric or integer class on x
  validate_class(
    input = x,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error"
  )

  # Error check: Ensure `digits` is an integer
  validate_class(
    input = digits,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error"
  )

  # Error check: Ensure that `truncate` is logical
  validate_class(input = truncate, class_type = "logical", type = "error")

  # Enforce character class on prefix which can be null
  if (!is.null(prefix)) {
    validate_class(input = prefix, class_type = "character", null_ok = TRUE)
  }

  # Save the current options settings to restore them later
  old <- options()

  # Ensure that the original options are restored when the function exits,
  # even if an error occurs
  on.exit(options(old))

  # Set the 'scipen' option to a high value to prevent scientific notation
  # in the output. This ensures that large numbers are displayed in their
  # full numeric form rather than in scientific notation.
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

  # Allow a toggle to truncate numbers so you can round something like
  # 555555 to "600k" with
  # > pretty_number(555555, truncate = TRUE, digits = 1)
  # "600k"
  if (truncate) {
    # When truncation is enabled, use signif() to truncate the numbers to the
    # specified number of significant digits. This ensures that the output is
    # concise and consistent.
    x <- signif(x, digits = digits)

    # Get the number of characters in the truncated value. This helps in
    # determining the appropriate suffix (e.g., "k", "m") based on the length of
    # the number.
    x_length <- nchar(trunc(x))
  } else {
    # When truncation is disabled, use round() to round the numbers to the
    # specified number of decimal places. This preserves the original scale of
    # the number.
    x <- round(x, digits = digits)

    # Get the number of characters in the rounded value. This helps in
    # determining the appropriate suffix (e.g., "k", "m") based on the length of
    # the number.
    x_length <- nchar(trunc(x))
  }

  # Classify the numeric value `x` into readable abbreviated formats

  # Define the suffixes for different orders of magnitude
  suffix <- c("k", "m", "b", "t", "qd", "qt", "sxt", "spt", "oct", "non", "dec")

  # Determine the appropriate suffix and format the value based on the length of
  # `x`
  x_val <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    x_length %in% 4:6 ~
      # If the length of `x` is between 4 and 6 characters, divide by 1 thousand
      # and append the "k" suffix
      paste0(round(x / thousand, digits = digits), suffix[1]),
    x_length %in% 7:9 ~
      # If the length of `x` is between 7 and 9 characters, divide by 1 million
      # and append the "m" suffix
      paste0(round(x / million, digits = digits), suffix[2]),
    x_length %in% 10:12 ~
      # If the length of `x` is between 10 and 12 characters, divide by 1
      # billion and append the "b" suffix
      paste0(round(x / billion, digits = digits), suffix[3]),
    x_length %in% 13:15 ~
      # If the length of `x` is between 13 and 15 characters, divide by 1
      # trillion and append the "t" suffix
      paste0(round(x / trillion, digits = digits), suffix[4]),
    x_length %in% 16:18 ~
      # If the length of `x` is between 16 and 18 characters, divide by 1
      # quadrillion and append the "qd" suffix
      paste0(round(x / quadrillion, digits = digits), suffix[5]),
    x_length %in% 19:21 ~
      # If the length of `x` is between 19 and 21 characters, divide by
      # 1 quintillion and append the "qt" suffix
      paste0(round(x / quintillion, digits = digits), suffix[6]),
    x_length %in% 22:24 ~
      # If the length of `x` is between 22 and 24 characters, divide
      # by 1 sextillion and append the "sxt" suffix
      paste0(round(x / sextillion, digits = digits), suffix[7]),
    x_length %in% 25:27 ~
      # If the length of `x` is between 25 and 27 characters, divide
      # by 1 septillion and append the "spt" suffix
      paste0(round(x / septillion, digits = digits), suffix[8]),
    x_length %in% 28:30 ~
      # If the length of `x` is between 28 and 30 characters,
      # divide by 1 octillion and append the "oct" suffix
      paste0(round(x / octillion, digits = digits), suffix[9]),
    x_length %in% 31:33 ~
      # If the length of `x` is between 31 and 33 characters,
      # divide by 1 nonillion and append the "non" suffix
      paste0(
        round(x / nonillion, digits = digits),
        suffix[10]
      ),
    x_length %in% 34:36 ~
      # If the length of `x` is between 34 and 36 characters,
      # divide by 1 decillion and append the "dec" suffix
      paste0(
        round(x / decillion, digits = digits),
        suffix[11]
      ),
    # If the length of `x` does not match any of the above
    # ranges, round `x` to the specified number of decimal
    # places
    TRUE ~ paste0(round(x, digits = digits))
  )

  # If no prefix is provided, return the formatted value
  if (is.null(prefix)) {
    return(x_val)
  }

  # If a prefix is provided, prepend it to the formatted value
  x_val <- ifelse(is.na(x_val), NA_character_, paste0(prefix, x_val))

  # Return the final formatted value
  return(x_val)
}
