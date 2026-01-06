# SEQIC Indicator 8 - Survival by Risk Group

**\[experimental\]**

Calculates the proportion of patients who survived based on risk groups
existing in the data among trauma patients transported to Level I–IV
trauma centers.

## Usage

``` r
seqic_indicator_8(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  mortality_indicator,
  risk_group,
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

- mortality_indicator:

  A logical, character, or factor variable indicating whether the
  patient died at the trauma center. Accepts values like `TRUE`/`FALSE`
  or `"Yes"`/`"No"`.

- risk_group:

  A character or factor column indicating the patient's risk group
  (e.g., "High", "Moderate", "Low"). See risk definitions below.

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

A named list with two tibbles:

`overall`: A tibble summarizing overall mortality among trauma patients,
grouped by the variables specified in `groups`. Columns include:

- `numerator_8_all` (number of survivors),

- `denominator_8_all` (total number of unique trauma incidents),

- `seqic_8_all` (survival proportion), and optionally

- `lower_ci_8`,

- `upper_ci_8` (confidence interval bounds if `calculate_ci` is
  specified).

`risk_group`: A tibble summarizing mortality stratified by risk group
and any additional grouping variables. Columns include:

- `risk_group` (used for stratification),

- `numerator_8_risk` (survivors per group),

- `denominator_8_risk` (total incidents per group),

- `seqic_8_risk` (survival proportion per group), and optionally

- `lower_ci_8_risk`,

- `upper_ci_8_risk` (confidence interval bounds if `calculate_ci` is
  specified).

## Details

- Filters the dataset to include only trauma center levels I through IV.

- Deduplicates the dataset using `unique_incident_id` to ensure one
  record per incident.

- Accepts a mortality indicator that may be logical, character, or
  factor, and identifies survivors as those with values of `FALSE` or
  `"No"`.

- Requires a predefined `risk_group` variable representing categories
  such as "Low", "Moderate", or "High" risk.

- Calculates overall survival proportions and survival proportions
  stratified by risk group.

- Optionally includes 95% confidence intervals using binomial methods if
  `calculate_ci` is specified.

## Note

This function calculates survival outcomes for patients transported to
trauma centers, stratified by risk of mortality. Risk groups—low,
moderate, and high— are defined by the Iowa System Evaluation and
Quality Improvement Committee (SEQIC) as described below. Users may also
apply alternative risk stratification methods if preferred.

- Abnormal Physiology Criteria: GCS 3–5; Respirations \<5 or \>30 per
  minute; Systolic BP \<60 mm Hg

- High Risk: Probability of Survival \< 0.2; ISS \> 41; ISS \> 24 with
  abnormal physiology

- Moderate Risk: Probability of Survival 0.2–0.5; ISS 16–41

- Low Risk: Probability of Survival \> 0.5; ISS \< 16; Normal physiology

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

# Simulated dataset for SEQIC Indicator 8
test_data <- tibble::tibble(
  id = as.character(1:12),
  trauma_level = c("I", "II", "III", "IV", "V", "II", "I", "III", "IV", "II",
  "I", "III"),
  mortality = c(FALSE, "No", TRUE, "Yes", FALSE, TRUE, "No", FALSE, "Yes",
  FALSE, TRUE, "No"),
  risk = c("High", "High", "Moderate", "Moderate", "Low", "Low", "High",
  "Moderate", "Low", "Moderate", "High", "Low")
)

# Run indicator 8 function
traumar::seqic_indicator_8(
  data = test_data,
  level = trauma_level,
  unique_incident_id = id,
  mortality_indicator = mortality,
  risk_group = risk
)
#> $overall
#> # A tibble: 1 × 4
#>   data              numerator_8_all denominator_8_all seqic_8_all
#>   <chr>                       <int>             <int>       <dbl>
#> 1 population/sample               6                11       0.545
#> 
#> $risk_group
#> # A tibble: 3 × 5
#>   data                    risk  numerator_8_risk denominator_8_risk seqic_8_risk
#>   <chr>                   <chr>            <int>              <int>        <dbl>
#> 1 population/sample risk… High                 3                  4        0.75 
#> 2 population/sample risk… Low                  1                  3        0.333
#> 3 population/sample risk… Mode…                2                  4        0.5  
#> 
```
