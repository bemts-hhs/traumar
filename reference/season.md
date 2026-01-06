# Get Season Based on a Date

This function determines the season (Winter, Spring, Summer, or Fall)
based on an input date.

The seasons are assigned based on geographic regions similar to how
seasons occur in the United States.

The seasons are determined using the month of the year and the
traditional meteorological definition of seasons (Winter: December,
January, February; Spring: March, April, May; Summer: June, July,
August; Fall: September, October, November).

## Usage

``` r
season(input_date)
```

## Arguments

- input_date:

  A Date or POSIXct object representing the date to determine the season
  for. The input must be of class `Date` or `POSIXct`.

## Value

A factor indicating the season corresponding to the input date. The
factor levels are:

- "Winter" for December, January, and February.

- "Spring" for March, April, and May.

- "Summer" for June, July, and August.

- "Fall" for September, October, and November.

- "Undetermined" if the input is not a valid Date or POSIXct object or
  if the month is missing.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Example usage of the season function
season(as.Date("2025-01-15"))
#> [1] Winter
#> Levels: Winter
season(as.POSIXct("2025-07-01 12:00:00"))
#> [1] Summer
#> Levels: Summer
```
