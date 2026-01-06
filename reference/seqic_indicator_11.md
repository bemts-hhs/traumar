# SEQIC Indicator 11 – Overtriage for Minor Trauma Patients

**\[experimental\]**

Calculates SEQIC Indicator 11, which estimates the proportion of minor
trauma patients who were transferred into a trauma center and remained
in the Emergency Department for less than 24 hours. This indicator is
designed to identify potential overtriage events within the trauma
system. Minor trauma patients are identified using the Injury Severity
Score (ISS \< 9). Patients must not have been transferred out and must
have been received at a trauma center level included in
`included_levels`.

## Usage

``` r
seqic_indicator_11(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  transfer_out_indicator,
  receiving_indicator,
  unique_incident_id,
  iss,
  ed_LOS,
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

- transfer_out_indicator:

  Column name indicating whether the patient was transferred out of the
  initial trauma center to definitive care. Logical, character, or
  factor type. Values representing "No" (e.g., FALSE, "No") indicate no
  transfer out.

- receiving_indicator:

  Column name indicating whether the patient was transferred into the
  trauma center. Logical, character, or factor type. Values representing
  "Yes" (e.g., TRUE, "Yes") indicate transfer in.

- unique_incident_id:

  Unique identifier for each record.

- iss:

  Optional numeric column representing the Injury Severity Score.

- ed_LOS:

  Column for the calculated ED length of stay, measured in minutes.

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

A tibble summarizing the numerator, denominator, and proportion of
overtriaged patients (Indicator 11), with optional 95% confidence
intervals.

## Details

This function:

- Filters the dataset to include only patients treated at trauma centers
  designated Level I through IV.

- Excludes patients transferred out and retains only those received by
  the trauma center.

- Deduplicates incident-level records using `unique_incident_id`.

- Classifies patients as low-risk based on the Injury Severity Score
  (ISS \< 9).

- Flags low-risk patients who were discharged from the ED in under 24
  hours.

- Stratifies results by one or more user-defined grouping variables.

- Returns a summarized tibble with the number of eligible low-risk
  short-stay discharges (numerator), all received patients meeting
  inclusion criteria (denominator), and the resulting proportion.

- Optionally includes 95% confidence intervals if `calculate_ci` is
  specified.

Users must ensure appropriate column names are passed and data is
pre-processed to include the necessary fields without missing critical
identifiers or timestamps.

## References

Roden-Foreman JW, Rapier NR, Yelverton L, Foreman ML. Asking a Better
Question: Development and Evaluation of the Need For Trauma Intervention
(NFTI) Metric as a Novel Indicator of Major Trauma. J Trauma Nurs. 2017
May/Jun;24(3):150-157. doi: 10.1097/JTN.0000000000000283. PMID:
28486318.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Packages
library(dplyr)
library(traumar)

# Simulated data for SEQIC Indicator 11
test_data <- tibble::tibble(
  id = as.character(1:10),
  trauma_level = c("I", "II", "III", "IV", "II", "I", "IV", "III", "II",
  "I"),
  transferred_out = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
  FALSE, FALSE),
  received = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  iss = c(4, 8, 10, 6, 5, 7, 6, 15, 3, 2),
  ed_LOS = c(6, 20, 30, 18, 8, 5, 22, 40, 2, 4),
  region = rep(c("East", "West"), each = 5)
)

# Run the function
traumar::seqic_indicator_11(
  data = test_data,
  level = trauma_level,
  included_levels = c("I", "II", "III", "IV"),
  transfer_out_indicator = transferred_out,
  receiving_indicator = received,
  unique_incident_id = id,
  iss = iss,
  ed_LOS = ed_LOS,
  groups = "region",
  calculate_ci = "clopper-pearson"
)
#> # A tibble: 2 × 6
#>   region numerator_11 denominator_11 seqic_11 lower_ci_11 upper_ci_11
#>   <chr>         <int>          <int>    <dbl>       <dbl>       <dbl>
#> 1 East              3              4     0.75       0.194       0.994
#> 2 West              3              4     0.75       0.194       0.994
```
