# Impute Numeric Column Values

Cleans numeric columns by handling extreme values or imputing missing
values. The function supports two main focuses: handling skewed
distributions or imputing missing data.

## Usage

``` r
impute(
  x,
  focus = c("skew", "missing"),
  method = c("winsorize", "iqr", "mean", "median"),
  percentile = NULL
)
```

## Arguments

- x:

  A numeric vector to be cleaned.

- focus:

  A character string indicating the focus. Options are:

  - `"skew"`: Handle extreme values using percentile or IQR methods
    (default).

  - `"missing"`: Impute missing values.

- method:

  A character string specifying the method:

  - For `focus = "skew"`:

    - `"winsorize"`: Replace values outside specified percentiles
      (default).

    - `"iqr"`: Use IQR to limit extreme values.

  - For `focus = "missing"`:

    - `"mean"`: Replace missing values with the mean.

    - `"median"`: Replace missing values with the median.

- percentile:

  A numeric value (percentile \> 0) for winsorization. If not provided,
  defaults to 0.01 and 0.99.

## Value

A numeric vector with cleaned or imputed values.

## Examples

``` r
x <- c(1, 2, 3, 100, 200, NA)
# Winsorize to 1% and 99%
impute(x, focus = "skew", method = "winsorize")
#> [1]   1.04   2.00   3.00 100.00 196.00     NA

# Replace missing values with the mean
impute(x, focus = "missing", method = "mean")
#> [1]   1.0   2.0   3.0 100.0 200.0  61.2
```
