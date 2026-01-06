# SEQIC Indicator 10 – Trauma Team Activation Appropriateness

**\[experimental\]**

Calculates three trauma system quality indicators related to trauma team
activations where the patient was kept at the facility:

- 10a: Proportion of patients meeting triage criteria (based on Injury
  Severity Score or Need For Trauma Intervention) who received low-level
  or no activation (undertriage).

- 10b: Proportion of patients not meeting triage criteria who received
  highest-level trauma activation (overtriage).

- 10c: Proportion of major trauma patients receiving a full activation
  (undertriage via Peng & Xiang, 2019).

  (10a, 10b, 10c can be based on Injury Severity Score or Need For
  Trauma Intervention based on user choice)

Users may stratify results by one or more grouping variables and
optionally compute confidence intervals.

## Usage

``` r
seqic_indicator_10(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  transfer_out_indicator,
  trauma_team_activation_level,
  iss,
  nfti,
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

- trauma_team_activation_level:

  Column indicating the trauma team activation level (e.g., `"Level 1"`,
  `"Level 2"`, `"Level 3"`, `"Consultation"`). Must be character or
  factor.

- iss:

  Optional numeric column representing the Injury Severity Score.

- nfti:

  Optional column indicating Need For Trauma Intervention classification
  of positive or negative. Should be character, factor, or logical.

- groups:

  Optional character vector of column names used for grouping results.

- calculate_ci:

  Optional; if not `NULL`, must be `"wilson"` or `"clopper-pearson"` to
  compute confidence intervals.

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

A list of two tibbles with counts and proportions for SEQIC Indicators
10a, 10b, and 10c, along with model diagnostics for the Cribari or NFTI
ouputs. The proportions in 10a, 10b, and 10c will optionally include 95%
confidence intervals.

## Details

This function:

- Restricts analysis to Level I–IV trauma centers.

- Removes duplicate incidents using `unique_incident_id`.

- Classifies each record as meeting or not meeting triage criteria based
  on ISS or NFTI logic.

- Optionally computes 95% confidence intervals for each indicator.

Users must ensure appropriate column names are passed and data is
pre-processed to include the necessary fields without missing critical
identifiers or timestamps.

## References

Beam G, Gorman K, Nannapaneni S, Zipf J, Simunich T, et al. (2022) Need
for Trauma Intervention and Improving Under-Triaging in Geriatric Trauma
Patients: undertriaged or Misclassified. Int J Crit Care Emerg Med
8:136. doi.org/10.23937/2474-3674/1510136

Peng J, Xiang H. Trauma undertriage and overtriage rates: are we using
the wrong formulas? Am J Emerg Med. 2016 Nov;34(11):2191-2192. doi:
10.1016/j.ajem.2016.08.061. Epub 2016 Aug 31. PMID: 27615156; PMCID:
PMC6469681.

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

# Simulated data for SEQIC Indicator 10
test_data <- tibble::tibble(
  id = as.character(1:12),
  trauma_level = c("I", "II", "III", "IV", "II", "I", "IV", "III", "II", "I",
  "III", "IV"),
  activation = c("Level 1", "Level 2", "None", "Consultation", "Level 1",
  "Level 1", "None", "Level 3", "Level 1", "Consultation", "None", "Level
  2"),
  acute_transfer = rep("No", 12),
  iss = c(25, 10, 16, 8, 30, 45, 12, 9, 28, 6, 17, 14),
  nfti = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE,
  TRUE, TRUE),
  region = rep(c("East", "West"), each = 6)
)

# Run the function, this will succeed
traumar::seqic_indicator_10(
  data = test_data,
  level = trauma_level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id = id,
  transfer_out_indicator = acute_transfer,
  trauma_team_activation_level = activation,
  iss = iss,
  nfti = NULL,
  groups = "region",
  calculate_ci = "wilson"
)
#> $seqic_10
#> # A tibble: 2 × 17
#>   triage_logic region numerator_10a denominator_10a seqic_10a lower_ci_10a
#>   <chr>        <chr>          <int>           <int>     <dbl>        <dbl>
#> 1 cribari      East               1               3     0.333       0.0177
#> 2 cribari      West               1               5     0.2         0.0105
#> # ℹ 11 more variables: upper_ci_10a <dbl>, numerator_10b <int>,
#> #   denominator_10b <int>, seqic_10b <dbl>, lower_ci_10b <dbl>,
#> #   upper_ci_10b <dbl>, numerator_10c <int>, denominator_10c <int>,
#> #   seqic_10c <dbl>, lower_ci_10c <dbl>, upper_ci_10c <dbl>
#> 
#> $diagnostics
#> # A tibble: 2 × 15
#>   triage_logic region full_minor full_major limited_minor limited_major     N
#>   <chr>        <chr>       <int>      <int>         <int>         <int> <int>
#> 1 cribari      East            0          3             1             1     5
#> 2 cribari      West            0          1             1             1     3
#> # ℹ 8 more variables: sensitivity <dbl>, specificity <dbl>,
#> #   positive_predictive_value <dbl>, negative_predictive_value <dbl>,
#> #   false_negative_rate <dbl>, false_positive_rate <dbl>,
#> #   false_discovery_rate <dbl>, false_omission_rate <dbl>
#> 

# Run the function, this will fail
try(
  traumar::seqic_indicator_10(
  data = test_data,
  level = trauma_level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id = id,
  transfer_out_indicator = acute_transfer,
  trauma_team_activation_level = activation,
  iss = iss,
  nfti = nfti,
  groups = "region",
  calculate_ci = "wilson"
))
#> Error in traumar::seqic_indicator_10(data = test_data, level = trauma_level,  : 
#>   Please supply exactly one of `iss` or `nfti`.
```
