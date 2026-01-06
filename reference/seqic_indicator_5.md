# SEQIC Indicator 5 - Alcohol and Drug Screening

**\[experimental\]**

Computes SEQIC Indicator 5a–5d for trauma system quality monitoring.
These indicators measure alcohol and drug screening rates among trauma
patients at trauma level I–IV facilities.

## Usage

``` r
seqic_indicator_5(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  blood_alcohol_content,
  drug_screen,
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

- blood_alcohol_content:

  Unquoted column name for blood alcohol concentration. Numeric. A
  non-missing value indicates a test was performed. Values greater than
  zero are considered positive results.

- drug_screen:

  Unquoted column name for the drug screen result. Character or factor.
  May contain keywords (e.g., "opioid", "cocaine", "none"). The keywords
  used in this function correspond to the National Trauma Data Bank
  (NTDB) field values for the corresponding data element.

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

A tibble summarizing SEQIC Indicator 5a–5d results. Includes numerator,
denominator, and calculated proportion for each measure. Optionally
includes 95% confidence intervals.

## Details

This function:

- Filters to trauma records at trauma levels I–IV.

- Deduplicates by `unique_incident_id` to ensure one record per
  incident.

- Calculates four sub-measures:

  - Indicator 5a: Proportion of patients with a blood alcohol test
    performed.

  - Indicator 5b: Among those tested, the proportion with BAC \> 0.

  - Indicator 5c: Proportion of patients with any recorded drug screen
    result.

  - Indicator 5d: Among those with a drug result, the proportion that
    included a known positive drug (e.g., opioids, cocaine, THC).

- Matches drug-related terms using regular expressions for a broad set
  of known substances. Matching is case-insensitive.

## Note

Users must ensure input columns are correctly named and contain
standardized values where applicable. Drug screen values should ideally
use consistent naming or be mapped to recognizable substance terms prior
to function use.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Packages
library(dplyr)
library(traumar)

# Create synthetic test data for Indicators 5a–5d
test_data <- tibble::tibble(
  id = as.character(1:10),
  trauma_level = rep(c("I", "II", "III", "IV", "V"), each = 2),
  bac = c(0.08, NA, 0, 0.02, NA, 0.15, NA, NA, 0, 0),
  drug = c(
    "opioid", "none", "cocaine", "none", NA,
    "benzodiazepine", "alcohol", "thc", "none", NA
  )
)

# Run the indicator function
traumar::seqic_indicator_5(
  data = test_data,
  level = trauma_level,
  unique_incident_id = id,
  blood_alcohol_content = bac,
  drug_screen = drug
) |>
  tidyr::pivot_longer(cols = -1, names_to = "Indicator", values_to =
  "Values")
#> # A tibble: 12 × 3
#>    data              Indicator      Values
#>    <chr>             <chr>           <dbl>
#>  1 population/sample numerator_5a    4    
#>  2 population/sample denominator_5a  8    
#>  3 population/sample seqic_5a        0.5  
#>  4 population/sample numerator_5b    3    
#>  5 population/sample denominator_5b  4    
#>  6 population/sample seqic_5b        0.75 
#>  7 population/sample numerator_5c    7    
#>  8 population/sample denominator_5c  8    
#>  9 population/sample seqic_5c        0.875
#> 10 population/sample numerator_5d    5    
#> 11 population/sample denominator_5d  7    
#> 12 population/sample seqic_5d        0.714
```
