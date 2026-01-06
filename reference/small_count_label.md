# Label Small Counts Based on a Cutoff

This function labels values in a vector as a replacement string if they
are smaller than a specified cutoff. The input can be numeric, and the
function will return either a modified version of the input vector with
small values replaced by a given label, or it will keep the original
values otherwise.

## Usage

``` r
small_count_label(var, cutoff, replacement)
```

## Arguments

- var:

  A numeric vector. This represents the variable to be checked against
  the cutoff.

- cutoff:

  A numeric value representing the threshold. Values in `var` smaller
  than this value will be replaced.

- replacement:

  A string or a numeric value. If the value in `var` is smaller than the
  `cutoff`, this value will replace it. If a string is provided, it will
  replace the numeric values with the string. If a numeric value is
  provided, the replacement will also be numeric.

## Value

A vector with values from `var`. Values smaller than the `cutoff` will
be replaced by the `replacement`. If `replacement` is a string, the
return type will be character, otherwise, it will remain numeric.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Example usage of the small_count_label function
small_count_label(c(1, 5, 10), 5, "Below Cutoff")
#> [1] "Below Cutoff" "5"            "10"          
small_count_label(c(1, 5, 10), 5, 0)
#> [1]  0  5 10
```
