# SEQIC Indicator 3 - Presence of Probability of Survival Calculations

**\[experimental\]**

This function calculates Indicator 3, a measure of the proportion of
trauma incidents where the probability of survival is recorded. It
filters the data by trauma center level (I-IV), excluding burn cases,
and computes the proportion of incidents with a valid probability of
survival value.

## Usage

``` r
seqic_indicator_3(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  trauma_type,
  unique_incident_id,
  probability_of_survival,
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

- trauma_type:

  A column name indicating the type of trauma. The function filters out
  "Burn" cases.

- unique_incident_id:

  Unique identifier for each record.

- probability_of_survival:

  A column name for the probability of survival for each incident.

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

A tibble summarizing SEQIC Indicator 3 results. Includes numerator,
denominator, and performance rate for the indicator. 95% confidence
intervals are provided optionally.

## Details

This function:

- Filters trauma records to those with a trauma center level of I–IV.

- Excludes records with a trauma type of "Burn".

- Deduplicates by `unique_incident_id` to ensure one record per
  incident.

- Calculates the proportion of records with a non-missing
  `probability_of_survival`.

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
  unique_id = as.character(1:10),
  trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV", "I", "II"),
  trauma_category = c("Blunt", "Penetrating", "Burn", "Blunt", "Penetrating",
                      "Burn", "Blunt", "Penetrating", "Blunt", "Blunt"),
  survival_prob = c(0.95, 0.89, NA, 0.76, NA, 0.92, 0.88, NA, 0.97, 0.91)
)

# Run the indicator function
traumar::seqic_indicator_3(
  data = test_data,
  level = trauma_level,
  trauma_type = trauma_category,
  unique_incident_id = unique_id,
  probability_of_survival = survival_prob,
  groups = "trauma_level"
)
#> # A tibble: 4 × 4
#>   trauma_level numerator_3 denominator_3 seqic_3
#>   <chr>              <int>         <int>   <dbl>
#> 1 I                      2             3   0.667
#> 2 II                     2             2   1    
#> 3 III                    1             1   1    
#> 4 IV                     1             2   0.5  
```
