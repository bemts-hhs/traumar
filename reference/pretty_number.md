# Convert Numbers into Readable Abbreviated Formats

This function converts large numeric values into readable abbreviated
formats (e.g., 1,000 becomes "1k") with options for rounding, decimal
precision, and a custom prefix. It supports numbers up to the decillion
range.

## Usage

``` r
pretty_number(
  x,
  digits = 2,
  n_decimal = deprecated(),
  prefix = NULL,
  truncate = FALSE
)
```

## Arguments

- x:

  A numeric value or vector to be converted into a readable format.

- digits:

  Number of decimal places to display. Defaults to 2.

- n_decimal:

  **\[deprecated\]** Use `digits` instead.

- prefix:

  An optional character string to prepend to the formatted number (e.g.,
  "\$"). Defaults to `NULL`.

- truncate:

  A logical value indicating whether to truncate the numbers before
  formatting. When `TRUE`, the function uses
  [`base::signif()`](https://rdrr.io/r/base/Round.html) to truncate the
  numbers to the specified number of significant digits, making the
  output more concise. When `FALSE`, the function uses
  [`base::round()`](https://rdrr.io/r/base/Round.html) to round the
  numbers to the specified number of decimal places, preserving the
  original scale of the number. Defaults to `FALSE`.

## Value

A character vector with the numbers formatted as abbreviated strings. If
`prefix` is provided, it prepends the formatted numbers.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Basic usage
pretty_number(1234)               # "1.23k"
#> [1] "1.23k"
pretty_number(1234567)            # "1.23m"
#> [1] "1.23m"
pretty_number(1234567890)         # "1.23b"
#> [1] "1.23b"

# Adjusting decimal places
pretty_number(1234, digits = 1) # "1.2k"
#> [1] "1.2k"

# Adding a prefix
pretty_number(1234, prefix = "$")  # "$1.23k"
#> [1] "$1.23k"

# Without rounding
pretty_number(1250, truncate = TRUE) # "1.2k"
#> [1] "1.2k"
```
