# SEQIC Indicator 12 - Timeliness of Data Entry Post-Discharge

**\[experimental\]**

Calculates the proportion of trauma cases where data were entered into
the trauma registry within a defined number of days post-discharge. This
measure supports trauma system quality improvement by identifying
facilities meeting timely documentation expectations.

## Usage

``` r
seqic_indicator_12(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  facility_id,
  exclude_facility_list = NULL,
  unique_incident_id,
  data_entry_time,
  data_entry_standard = 60,
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

- facility_id:

  Numeric, character, or factor. Column giving the unique facility
  identifiers in the trauma dataset.

- exclude_facility_list:

  Optional. Numeric, character, or factor. List of facilities to exclude
  from analysis due to known data quality issues or other justifiable
  reasons. Defaults to `NULL`.

- unique_incident_id:

  Unique identifier for each record.

- data_entry_time:

  Numeric. Column representing the time in days between patient
  discharge and trauma registry data entry.

- data_entry_standard:

  Numeric. The maximum allowable number of days between discharge and
  data entry. Records entered within this threshold are considered
  timely. Default is `60`.

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

A tibble summarizing SEQIC Indicator 12 results. Includes numerator,
denominator, and performance rate. 95% confidence intervals are included
if requested.

## Details

This function:

- Filters to include only patients treated at Level I–IV trauma centers.

- Excludes records from facilities specified by the user, if applicable.

- Deduplicates by `unique_incident_id` to ensure each incident is
  counted once.

- Flags records where data entry occurred within `data_entry_standard`
  days of discharge.

- Optionally calculates confidence intervals using methods from
  `nemsqa_binomial_confint()`.

- Returns a tibble with numerator, denominator, and proportion of timely
  entries, with optional confidence intervals and population/sample
  labels.

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

# Simulated data for SEQIC Indicator 12
test_data <- tibble::tibble(
  id = as.character(1:10),
  trauma_level = c("I", "II", "III", "IV", "II", "I", "IV", "III", "II",
  "I"),
  facility = c("A", "B", "C", "D", "A", "C", "B", "A", "C", "D"),
  data_entry_delay = c(30, 65, 10, 70, 45, 20, 80, 15, 55, 90)
)

# Run the function
traumar::seqic_indicator_12(
  data = test_data,
  level = trauma_level,
  included_levels = c("I", "II", "III", "IV"),
  facility_id = facility,
  unique_incident_id = id,
  exclude_facility_list = c("D"),
  data_entry_time = data_entry_delay,
  data_entry_standard = 60,
  calculate_ci = "wilson"
)
#> # A tibble: 1 × 6
#>   data              numerator_12 denominator_12 seqic_12 lower_ci_12 upper_ci_12
#>   <chr>                    <int>          <int>    <dbl>       <dbl>       <dbl>
#> 1 population/sample            6              8     0.75       0.356       0.955
```
