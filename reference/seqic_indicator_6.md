# SEQIC Indicator 6 - Delayed Arrival Following Low GCS

**\[experimental\]**

Computes SEQIC Indicator 6 for trauma system quality monitoring. This
indicator measures the proportion of patients presenting with a Glasgow
Coma Scale (GCS) score \< 9 who arrive at a trauma level I–IV center
more than 180 minutes after injury. It excludes patients transferred out
of the facility and focuses on those transferred into a facility.

## Usage

``` r
seqic_indicator_6(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  transfer_out_indicator,
  receiving_indicator,
  low_GCS_indicator,
  time_from_injury_to_arrival,
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

- transfer_out_indicator:

  Column name indicating whether the patient was transferred out of the
  initial trauma center to definitive care. Logical, character, or
  factor type. Values representing "No" (e.g., FALSE, "No") indicate no
  transfer out.

- receiving_indicator:

  Column name indicating whether the patient was transferred into the
  trauma center. Logical, character, or factor type. Values representing
  "Yes" (e.g., TRUE, "Yes") indicate transfer in.

- low_GCS_indicator:

  Column name for identifying patients with a Glasgow Coma Scale score
  less than 9. Logical, character, or factor type.

- time_from_injury_to_arrival:

  Column name representing the time in minutes from injury occurrence to
  arrival at the trauma center. Numeric type.

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

A tibble summarizing SEQIC Indicator 6 results. Includes numerator,
denominator, calculated proportion, and optionally 95% confidence
intervals.

## Details

This function:

- Filters to trauma center records from facilities at trauma levels
  I–IV.

- Deduplicates records using `unique_incident_id`.

- Calculates:

  - Numerator: Patients with low GCS (\< 9) who arrived more than 180
    minutes after injury, were transferred in, and not transferred out.

  - Denominator: All patients with low GCS (\< 9) who were transferred
    in and not transferred out.

- Optionally calculates Wilson or Clopper-Pearson confidence intervals
  for the resulting proportion if `calculate_ci` is specified.

## Note

Users must ensure input columns are appropriately coded and
standardized. Transfer and GCS indicators should use consistent logical
or textual representations.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Packages
library(dplyr)
library(traumar)

# Create test data for Indicator 6
test_data <- tibble::tibble(
  id = as.character(1:10),
  trauma_level = rep(c("I", "II", "III", "IV", "V"), times = 2),
  transfer_out = c("No", "No", "Yes", "No", "No", "No", "No", "No", "No",
  "No"),
  transfer_in = c("Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes",
  "Yes", "Yes"),
  gcs_low = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  time_to_arrival = c(200, 100, 300, 190, 400, 181, 100, 179, 240, 178)
)

# Run the indicator function
traumar::seqic_indicator_6(
  data = test_data,
  level = trauma_level,
  unique_incident_id = id,
  transfer_out_indicator = transfer_out,
  receiving_indicator = transfer_in,
  low_GCS_indicator = gcs_low,
  time_from_injury_to_arrival = time_to_arrival
)
#> # A tibble: 1 × 4
#>   data              numerator_6 denominator_6 seqic_6
#>   <chr>                   <int>         <int>   <dbl>
#> 1 population/sample           3             6     0.5
```
