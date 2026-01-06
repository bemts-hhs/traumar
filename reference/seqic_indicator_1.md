# SEQIC Indicator 1 – Trauma Team Response Evaluation

**\[experimental\]**

This function calculates System Evaluation and Quality Improvement
Committee (SEQIC) Indicator 1 (subparts a through f). These indicators
assess the timeliness and type of provider response (e.g., surgeon,
mid-level, physician) to trauma alerts based on trauma team activation
level, hospital trauma level, and time to provider presence. Confidence
intervals can optionally be calculated for the proportion, using either
the Wilson or Clopper-Pearson method.

## Usage

``` r
seqic_indicator_1(
  data,
  trauma_team_activation_level,
  trauma_team_physician_service_type,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  response_time,
  trauma_team_activation_provider,
  groups = NULL,
  calculate_ci = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing trauma incident records.

- trauma_team_activation_level:

  Column identifying trauma team activation level (e.g., Level 1, Level
  2).

- trauma_team_physician_service_type:

  Column indicating the type of medical provider (e.g., Surgery/Trauma,
  Emergency Medicine). For indicators 1a, 1b, and 1c,
  `seqic_indicator_1()` will only look for records with the trauma team
  member service type documented as marked as 'Surgery/Trauma'. For
  Indicators 1d, 1e, and 1f, `seqic_indicator_1()` will look for the
  following service types:

  - "Surgery/Trauma",

  - "Emergency Medicine",

  - "Family Practice",

  - "Nurse Practitioner",

  - "Physician Assistant",

  - "Surgery Senior Resident",

  - "Hospitalist",

  - "Internal Medicine"

- level:

  Column indicating the trauma center designation level (e.g., I, II,
  III, IV).

- included_levels:

  Character vector indicating what facility levels to include in the
  analysis. Defaults to `c("I", "II", "III", "IV")`.

- unique_incident_id:

  Unique identifier for each record.

- response_time:

  Numeric variable representing the time (in minutes) to provider
  response.

- trauma_team_activation_provider:

  Column identifying the responding provider for trauma activation.

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

A tibble summarizing SEQIC Indicator 1 results across sub-measures
(1a–1f). Includes numerators, denominators, and performance rate for
each indicator. 95% confidence intervals are provided optionally.

## Details

This function filters and summarizes trauma records to calculate SEQIC
Indicators 1a through 1f:

- 1a: Proportion of Level 1 activations at Level I/II centers with
  surgical response \<= 15 minutes.

- 1b: Same as 1a, but includes Level III centers and uses \<= 30
  minutes.

- 1c: Proportion of Level 1 activations with missing surgical response
  time.

- 1d/e: Response within 5 and 20 minutes, respectively, for specific
  provider types and activation levels, includes level I-IV trauma
  centers.

- 1f: Proportion of missing response times among the group in 1d/e,
  includes level I-IV trauma centers.

## Note

This function:

- Filters trauma records to those with a trauma team activation level of
  "Level 1" and/or "Level 2" based on the indicator.

- Restricts provider type to surgical, physician, and mid-level provider
  roles.

- Filters trauma center levels to I–IV based on the measure.

- Calculates the proportion of cases where the response time is within
  5, 15, or 30 minutes, depending on the indicator.

- Computes proportions for trauma activation times, including missing
  times and within thresholds.

Users must ensure appropriate column names are passed and data is
pre-processed to include the necessary fields without missing critical
identifiers or timestamps.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Packages
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(traumar)

# Data
data <- tibble::tibble(
  incident_id = 1:6,
  activation_level = c("Level 1", "Level 1", "Level 2", "Level 1", "Level 2",
  "Level 1"),
  provider_type = c("Surgery/Trauma", "Emergency Medicine", "Physician
  Assistant", "Surgery/Trauma", "Surgery/Trauma", "Family Practice"),
  trauma_level = c("I", "II", "III", "I", "III", "IV"),
  response_minutes = c(12, 25, 6, NA, 18, 22),
  provider = c("Dr. A", "Dr. B", "PA C", "Dr. D", "Dr. E", "NP F")
)

# Run the function
traumar::seqic_indicator_1(
  data = data,
  trauma_team_activation_level = activation_level,
  trauma_team_physician_service_type = provider_type,
  level = trauma_level,
  unique_incident_id = incident_id,
  response_time = response_minutes,
  trauma_team_activation_provider = provider,
  calculate_ci = "wilson"
) |>
tidyr::pivot_longer(cols = -1,
                    names_to = "Indicator",
                    values_to = "Values"
                    )
#> # A tibble: 30 × 3
#>    data              Indicator      Values
#>    <chr>             <chr>           <dbl>
#>  1 population/sample numerator_1a   1     
#>  2 population/sample denominator_1a 1     
#>  3 population/sample seqic_1a       1     
#>  4 population/sample lower_ci_1a    0.0546
#>  5 population/sample upper_ci_1a    1     
#>  6 population/sample numerator_1b   1     
#>  7 population/sample denominator_1b 1     
#>  8 population/sample seqic_1b       1     
#>  9 population/sample lower_ci_1b    0.0546
#> 10 population/sample upper_ci_1b    1     
#> # ℹ 20 more rows
```
