# Assign Significance Codes Based on P-Values

This function assigns significance codes to a p-value vector based on
commonly accepted significance thresholds. The significance codes are:

- `"***"` for p-values \<= 0.001

- `"**"` for p-values \<= 0.01 and \> 0.001

- `"*"` for p-values \<= 0.05 and \> 0.01

- `"."` for p-values \<= 0.1 and \> 0.05

- `"<>"` for p-values \> 0.1

## Usage

``` r
stat_sig(p_val_data)
```

## Arguments

- p_val_data:

  A numeric vector representing the p-values to be categorized. The
  vector should contain p-values between 0 and 1.

## Value

A character vector with the assigned significance codes for each
p-value.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Example usage of the stat_sig function
data <- data.frame(p_value = c(0.001, 0.03, 0.12, 0.05, 0.07))

data |>
  dplyr::mutate(significance = stat_sig(p_val_data = p_value))
#>   p_value significance
#> 1   0.001          ***
#> 2   0.030            *
#> 3   0.120           <>
#> 4   0.050            *
#> 5   0.070            .
```
