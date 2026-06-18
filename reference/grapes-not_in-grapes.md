# Check if Elements Are Not in a Vector

**\[deprecated\]**

`%not_in%` has been **deprecated** because base R (\>= 4.6.0) now
provides the `%notin%` operator.

Please use `%notin%` from `base` instead.

## Usage

``` r
x %not_in% y
```

## Arguments

- x:

  A vector of values to be checked.

- y:

  A vector of values to check against.

## Value

A logical vector of the same length as `x`, where `TRUE` indicates the
corresponding element in `x` is not found in `y`, and `FALSE` indicates
it is found in `y`.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r

# Example vectors
x <- c("apple", "banana", "cherry")
y <- c("banana", "grape")

# Check which elements in `x` are not in `y`
x %not_in% y
#> Warning: `%not_in%()` was deprecated in traumar 1.2.7.
#> ℹ Please use `base::%notin%()` instead.
#> ℹ `base` R includes the %notin% operator as of R >= 4.6.0.
#> [1]  TRUE FALSE  TRUE

# Example with numeric values
a <- c(1, 2, 3, 4, 5)
b <- c(2, 4, 6)

a %not_in% b
#> [1]  TRUE FALSE  TRUE FALSE  TRUE
```
