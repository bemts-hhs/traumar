# SEQIC Indicator 2 – Missing Incident Time

**\[experimental\]**

This function calculates System Evaluation and Quality Improvement
Committee (SEQIC) Indicator 2. This indicator evaluates the proportion
of trauma incidents with missing documented incident time across Level
I–IV trauma centers.

## Usage

``` r
seqic_indicator_2(
  data,
  unique_incident_id,
  level,
  included_levels = c("I", "II", "III", "IV"),
  incident_time,
  groups = NULL,
  calculate_ci = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing trauma incident records.

- unique_incident_id:

  Unique identifier for each record.

- level:

  Column indicating the trauma center designation level (e.g., I, II,
  III, IV).

- included_levels:

  Character vector indicating what facility levels to include in the
  analysis. Defaults to `c("I", "II", "III", "IV")`.

- incident_time:

  The time the patient's injury occurred.

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

A tibble summarizing SEQIC Indicator 2 results. Includes numerator,
denominator, and performance rate for the indicator. 95% confidence
intervals are provided optionally.

## Details

This function:

- Filters trauma records to those with a trauma center level of I–IV.

- Deduplicates by `unique_incident_id` to ensure one record per
  incident.

- Calculates the proportion of cases missing `incident_time`.

## Note

Users must ensure appropriate column names are passed and data is
pre-processed to include the necessary fields without missing critical
identifiers or timestamps.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Packages
library(dplyr)
library(traumar)

# Data
data <- tibble::tibble(
  incident_id = as.character(101:106),
  trauma_level = c("I", "II", "III", "IV", "II", "I"),
  incident_time = as.POSIXct(c("2023-01-01 12:00", NA, "2023-01-02 14:15",
                               NA, "2023-01-03 09:30", "2023-01-04 16:45"))
)

# Run the function
traumar::seqic_indicator_2(
  data = data,
  unique_incident_id = incident_id,
  level = trauma_level,
  incident_time = incident_time,
  calculate_ci = "clopper-pearson"
)
#> # A tibble: 1 × 6
#>   data              numerator_2 denominator_2 seqic_2 lower_ci_2 upper_ci_2
#>   <chr>                   <int>         <int>   <dbl>      <dbl>      <dbl>
#> 1 population/sample           2             6   0.333     0.0433      0.777
```
