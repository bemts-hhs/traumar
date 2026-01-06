# SEQIC Indicator 13 – Validation of Trauma Registry Records

**\[experimental\]**

Calculates the proportion of trauma records that meet or exceed a
threshold for data validity among facilities at the specified trauma
center levels. Optionally computes confidence intervals.

## Usage

``` r
seqic_indicator_13(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  validity_score,
  validity_threshold = 85,
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

- validity_score:

  Numeric. The proportion of each trauma registry record that is valid,
  expressed as a percentage (0–100). Typically calculated by the
  registry system.

- validity_threshold:

  Numeric. The minimum acceptable validity percentage threshold for
  records to be counted in the numerator. Defaults to `85`.

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

A tibble summarizing SEQIC Indicator 13 results. Includes numerator,
denominator, and performance rate 95% confidence intervals are included
if requested.

## Details

This function:

- Filters to include only patients treated at trauma centers with levels
  specified in `included_levels` (default: Levels I–IV).

- Deduplicates the dataset using `unique_incident_id` to ensure each
  incident is counted only once.

- Flags records with a `validity_score` greater than or equal to the
  specified `validity_threshold` threshold (default: 85).

- Calculates the proportion of valid records among all included records.

- Optionally calculates binomial confidence intervals using the method
  specified in `calculate_ci` via `nemsqa_binomial_confint()`.

- Adds a "Population/Sample" label unless grouping is applied via
  `groups`.

Users must ensure that appropriate column names are passed using tidy
evaluation (bare column names) and that the input data has been cleaned
and includes no missing or malformed identifiers, trauma level
classifications, or validity scores.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Packages
library(dplyr)
library(traumar)

# Simulated data for SEQIC Indicator 13
test_data <- tibble::tibble(
  id = as.character(1:12),
  trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV", "I", "II",
  "III", "IV"),
  validity = c(90, 80, 88, 92, 86, 75, 89, 70, 95, 85, 83, 87)
)

# Run the function
traumar::seqic_indicator_13(
  data = test_data,
  level = trauma_level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id = id,
  validity_score = validity,
  validity_threshold = 85,
  calculate_ci = "wilson"
)
#> # A tibble: 1 × 6
#>   data              numerator_13 denominator_13 seqic_13 lower_ci_13 upper_ci_13
#>   <chr>                    <int>          <int>    <dbl>       <dbl>       <dbl>
#> 1 population/sample            8             12    0.667       0.354       0.887
```
