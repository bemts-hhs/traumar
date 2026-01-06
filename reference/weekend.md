# Classify Dates as Weekday or Weekend

This function classifies each date in a vector of dates as either
"Weekday" or "Weekend". The function returns "Weekday" for Monday to
Friday and "Weekend" for Saturday and Sunday.

## Usage

``` r
weekend(input_date)
```

## Arguments

- input_date:

  A vector of `Date` or `POSIXct` objects to classify.

## Value

A character vector with the classification for each date: either
"Weekday" or "Weekend".

## Details

The function checks if the `input_date` is a valid `Date` or `POSIXct`
object. It returns "Weekday" for dates that fall on Monday through
Friday and "Weekend" for dates that fall on Saturday or Sunday. If the
input is not of the correct class, the function will throw an error.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Example 1: Date of a weekend
weekend(as.Date("2025-01-18"))
#> [1] "Weekend"

# Example 2: Date of a weekday
weekend(as.Date("2025-01-15"))
#> [1] "Weekday"

# Example 3: Date of an invalid object
try(
weekend("2025-01-18") # This will throw an error
)
#> Error in weekend("2025-01-18") : 
#>   The input to `input_date` must be an object of class <Date> or
#> <POSIXct>, but you supplied an object of class <character>.Supply a <Date>
#> object to `weekend()`.
```
