# Normalize a Numeric Vector

This function normalizes a numeric or integer vector using one of two
methods: min-max normalization (scales data to the range (0, 1)) or
z-score normalization (centers data around 0 with a standard deviation
of 1).

## Usage

``` r
normalize(x, method = c("min_max", "z_score"))
```

## Arguments

- x:

  A numeric or integer vector to be normalized.

- method:

  A character string specifying the normalization method. Options are
  `"min_max"` for min-max normalization or `"z_score"` for z-score
  normalization. If no method is provided, the default is `"min_max"`.

## Value

A numeric vector of the same length as `x`, containing the normalized
values.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Example data
data <- c(10, 20, 30, 40, 50, NA)

# Min-max normalization
normalize(data, method = "min_max")
#> [1] 0.00 0.25 0.50 0.75 1.00   NA

# Z-score normalization
normalize(data, method = "z_score")
#> [1] -1.2649111 -0.6324555  0.0000000  0.6324555  1.2649111         NA

# Default behavior (min-max normalization)
normalize(data)
#> ℹ As no method was supplied, `normalize()` will default to min-max normalization methods.
#> [1] 0.00 0.25 0.50 0.75 1.00   NA
```
