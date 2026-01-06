# Convert Numbers into Readable Abbreviated Formats

This function converts large numeric values into readable abbreviated
formats (e.g., 1,000 becomes "1k") with options for rounding, decimal
precision, and a custom prefix. It supports numbers up to the decillion
range.

## Usage

``` r
pretty_number(x, n_decimal = 2, prefix = NULL, truncate = FALSE)
```

## Arguments

- x:

  A numeric value or vector to be converted into a readable format.

- n_decimal:

  An integer specifying the number of decimal places to include in the
  output. Defaults to `2`.

- prefix:

  An optional character string to prepend to the formatted number (e.g.,
  "\$"). Defaults to `NULL`.

- truncate:

  A logical value indicating whether to truncate the numbers before
  formatting. Defaults to `FALSE`.

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
pretty_number(1234, n_decimal = 1) # "1.2k"
#> [1] "1.2k"

# Adding a prefix
pretty_number(1234, prefix = "$")  # "$1.23k"
#> [1] "$1.23k"

# Without rounding
pretty_number(1250, truncate = TRUE) # "1.2k"
#> [1] "1.2k"
```
