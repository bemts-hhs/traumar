# SEQIC Indicator 4 - Autopsy and Long LOS Without Autopsy

**\[experimental\]**

Computes SEQIC Indicator 4a and 4b for trauma center performance.
Indicator 4a captures the proportion of deceased trauma patients at
trauma level I–IV facilities who had an autopsy performed. Indicator 4b
identifies deceased trauma patients with a prolonged length of stay (LOS
\> 3 days) but without an autopsy.

## Usage

``` r
seqic_indicator_4(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  ed_disposition,
  ed_LOS,
  hospital_disposition,
  hospital_LOS,
  unique_incident_id,
  autopsy,
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

- ed_disposition:

  Column representing the emergency department disposition. For a record
  to be picked up in this function, the ED dispostion must be documented
  as "Deceased/Expired".

- ed_LOS:

  Column for the calculated ED length of stay, measured in minutes.

- hospital_disposition:

  Column representing the hospital disposition. For a record to be
  picked up in this function, the hospital dispostion must be documented
  as "Deceased/Expired".

- hospital_LOS:

  Column for the calculated hospital length of stay, measured in
  minutes.

- unique_incident_id:

  Unique identifier for each record.

- autopsy:

  Unquoted column name indicating whether an autopsy was performed.
  Expected values: `"Yes"` or `NA`.

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

A tibble summarizing SEQIC Indicator 4a and 4b results. Includes
numerator, denominator, and performance rate for the indicator. 95%
confidence intervals are provided optionally.

## Details

This function:

- Filters trauma records to those with a trauma center level of I–IV.

- Identifies records where the patient died, based on ED or hospital
  disposition.

- Deduplicates by `unique_incident_id` to ensure one record per
  incident.

- For Indicator 4a, calculates the proportion of deceased patients who
  received an autopsy.

- For Indicator 4b, calculates the proportion of deceased patients with
  a hospital or ED length of stay greater than 72 hours (4320 minutes)
  and no autopsy performed.

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

# Create a synthetic test dataset
test_data <- tibble::tibble(
  id = as.character(1:8),
  trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV"),
  ed_disp = c(
    "Operating Room",
    "Admitted",
    "Deceased/Expired",
    "Transferred",
    "Deceased/Expired",
    "Deceased/Expired",
    "Admitted",
    "Deceased/Expired"
  ),
  ed_los = c(120, 200, 5000, 180, 3000, 4321, 60, 4000),
  hosp_disp = c(
    "Deceased/Expired",
    "Deceased/Expired",
    "Deceased/Expired",
    "Discharged",
    "Deceased/Expired",
    "Deceased/Expired",
    "Discharged",
    "Deceased/Expired"
  ),
  hosp_los = c(3000, 4500, 1000, 200, 5000, 4400, 150, 3000),
  autopsy_done = c("Yes", "No", "No", NA, "Yes", "No", NA, "Yes")
)

# Run the indicator function
traumar::seqic_indicator_4(
  data = test_data,
  level = trauma_level,
  ed_disposition = ed_disp,
  ed_LOS = ed_los,
  hospital_disposition = hosp_disp,
  hospital_LOS = hosp_los,
  unique_incident_id = id,
  autopsy = autopsy_done
)
#> # A tibble: 1 × 7
#>   data          numerator_4a denominator_4a seqic_4a numerator_4b denominator_4b
#>   <chr>                <int>          <int>    <dbl>        <int>          <int>
#> 1 population/s…            3              6      0.5            3              4
#> # ℹ 1 more variable: seqic_4b <dbl>
```
