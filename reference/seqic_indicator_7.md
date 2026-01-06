# SEQIC Indicator 7 - Delayed Arrival to Definitive Care

**\[experimental\]**

Computes SEQIC Indicator 7, which measures the proportion of trauma
patients arriving at the definitive care facility trauma centers (level
I–IV) more than 180 minutes after injury. This indicator identifies
delays in definitive care.

## Usage

``` r
seqic_indicator_7(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  time_from_injury_to_arrival,
  transfer_out_indicator,
  groups = NULL,
  calculate_ci = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing trauma incident records.

- level:

  Column indicating the trauma center designation level (e.g., I, II,
  III, IV).

- included_levels:

  Character vector indicating what facility levels to include in the
  analysis. Defaults to `c("I", "II", "III", "IV")`.

- unique_incident_id:

  Unique identifier for each record.

- time_from_injury_to_arrival:

  Column name representing the time in minutes from injury occurrence to
  arrival at the trauma center. Numeric type.

- transfer_out_indicator:

  Column name indicating whether the patient was transferred out of the
  initial trauma center to definitive care. Logical, character, or
  factor type. Values representing "No" (e.g., FALSE, "No") indicate no
  transfer out.

- groups:

  Additional columns passed as a vector of strings to
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
  via the `.by` argument for grouped summaries. Defaults to `NULL`.

- calculate_ci:

  If `NULL`, 95% confidence intervals will not be calculated for the
  performance estimates. Otherwise, options of "wilson" or
  "clopper-pearson" can be supplied to utilize the corresponding methods
  to calculate the confidence intervals for the proportions. Defaults to
  `NULL`.

- ...:

  Arguments passed on to
  [`nemsqar::nemsqa_binomial_confint`](https://bemts-hhs.github.io/nemsqar/reference/nemsqa_binomial_confint.html)

  `conf.level`

  :   Numeric value between 0 and 1 indicating the confidence level.
      Defaults to 0.95 (95% confidence interval).

  `correct`

  :   Logical, indicating whether to apply continuity correction for
      Wilson intervals. Defaults to `TRUE`.

## Value

A tibble summarizing SEQIC Indicator 7 results. Includes numerator,
denominator, and proportion. 95% confidence intervals are included if
requested.

## Details

This function:

- Filters the dataset to trauma center levels I through IV.

- Deduplicates the dataset by `unique_incident_id`.

- Creates a logical flag for arrivals occurring more than 180 minutes
  after injury.

- Identifies definitive care records where the patient arrived greater
  than 180 minutes after the time of injury.

- Returns a summarized tibble with the number of such cases (numerator),
  total eligible records (denominator), and the proportion.

- Optionally includes 95% confidence intervals if `calculate_ci` is
  specified.

## Note

The user must ensure all columns are correctly passed and that time
values are numeric and measured in minutes.

## Author

Nicolas Foss Ed.D., MS

## Examples

``` r
# Packages
library(dplyr)
library(traumar)

# Create test data for Indicator 7
test_data <- tibble::tibble(
  id = as.character(1:10),
  trauma_level = rep(c("I", "II", "III", "IV", "V"), times = 2),
  time_to_arrival = c(200, 100, 220, 150, 400, 181, 90, 179, 240, 178),
  transfer_out = c("No", "No", "No", "No", "Yes", "No", "No", "No", "No",
  "No")
)

# Run the indicator function
traumar::seqic_indicator_7(
  data = test_data,
  level = trauma_level,
  unique_incident_id = id,
  time_from_injury_to_arrival = time_to_arrival,
  transfer_out_indicator = transfer_out
)
#> # A tibble: 1 × 4
#>   data              numerator_7 denominator_7 seqic_7
#>   <chr>                   <int>         <int>   <dbl>
#> 1 population/sample           4             8     0.5
```
