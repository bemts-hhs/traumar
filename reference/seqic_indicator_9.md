# SEQIC Indicator 9 - Emergency Department Transfer Timeliness

**\[experimental\]**

Calculates the proportion of EMS-transferred trauma patients who
experienced delayed transfer from the emergency department (ED) based on
disposition and decision-to-transfer time frames. This includes both
overall rates and stratified results by trauma team activation status,
with optional confidence intervals.

## Usage

``` r
seqic_indicator_9(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  transfer_out_indicator,
  transport_method,
  unique_incident_id,
  trauma_team_activated,
  risk_group,
  ed_LOS,
  ed_decision_LOS,
  ed_decision_discharge_LOS,
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

- transport_method:

  Column identifying the EMS transport method (e.g., ambulance, private
  vehicle). Used to exclude non-qualified modes of arrival.

- unique_incident_id:

  Unique identifier for each record.

- trauma_team_activated:

  Column indicating whether the trauma team was activated (character,
  factor, or logical).

- risk_group:

  A character or factor column indicating the patient's risk group
  (e.g., "High", "Moderate", "Low"). See risk definitions below.

- ed_LOS:

  Column for the calculated ED length of stay, measured in minutes.

- ed_decision_LOS:

  Numeric column representing minutes from ED arrival to decision to
  transfer.

- ed_decision_discharge_LOS:

  Numeric column representing minutes from ED decision to discharge to
  physical discharge.

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

A list of four tibbles, with optional 95% confidence intervals:

- `seqic_9_all`: Proportion of transferred trauma patients with ED
  discharge or decision delays \>2 or \>3 hours, grouped by optional
  variables.

- `seqic_9_activations`: Same proportions as above, further stratified
  by trauma team activation status.

- `seqic_9_risk`: Same proportions as above, further stratified by risk
  groups.

- `seqic_9_activations_risk`: Same proportions as above, further
  stratified by risk groups and trauma team activation status.

Each tibble includes numerators, denominators, proportions, and
(optionally) confidence intervals for:

- 9a: Delayed discharge \>2 hours

- 9b: Delayed discharge \>3 hours

- 9c: Delayed decision \>1 hours

- 9d: Delayed decision \>2 hours

- 9e: Delayed decision to discharge \>1 hour

- 9f: Delayed decision to discharge \>2 hours

## Details

This function:

- Filters the dataset to include only transfers out from trauma centers
  designated Level I through IV.

- Deduplicates records using `unique_incident_id`.

- Flags records where emergency department decision to discharge
  occurred more than 60 or 120 minutes after ED arrival.

- Flags records where physical departure from the ED occurred more than
  120 or 180 minutes after ED arrival.

- Flags records where physical discharge occurred more than 60 or 120
  minutes after ED decision to discharge.

- Stratifies results by trauma team activation status and one or more
  grouping variables.

- Stratifies results by risk groups and one or more grouping variables.

- Returns a summarized tibble with the number of delayed cases
  (numerator), eligible records (denominator), and the proportion for
  each delay threshold.

- Optionally includes 95% confidence intervals if `calculate_ci = TRUE`.

## Note

This function calculates discharge timeliness outcomes for patients
transported to trauma centers, stratified by risk of mortality. Risk
groups—low, moderate, and high— are defined by the Iowa System
Evaluation and Quality Improvement Committee (SEQIC) as described below.
Users may also apply alternative risk stratification methods if
preferred.

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

# Simulated dataset for SEQIC Indicator 9
test_data <- tibble::tibble(
  id = as.character(1:10),
  trauma_level = c("I", "II", "III", "IV", "V", "II", "III", "IV", "I",
  "II"),
  transport = c("Ambulance", "Ambulance", "Private Vehicle", "Ambulance",
  "Helicopter", "Ambulance", "Ambulance", "Ambulance", "Ambulance",
  "Ambulance"),
  activated = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,
  FALSE),
  ed_LOS = c(120, 180, 90, 60, 200, 130, 110, 160, 95, 220),
  ed_decision = c(55, 125, 65, 30, 190, 80, 70, 45, 61, 130),
  ed_discharge = c(130, 185, 110, 65, 150, 160, 95, 180, 70, 210),
  transfer_out = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
  TRUE),
  risk = c("High", "High", "Moderate", "Low", "Moderate", "Low",
           "High", "Low", "Moderate", "High")
)

# Run the function, and store as a list object
seqic_9_result <- traumar::seqic_indicator_9(
  data = test_data,
  level = trauma_level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id = id,
  transport_method = transport,
  transfer_out_indicator = transfer_out,
  ed_LOS = ed_LOS,
  ed_decision_LOS = ed_decision,
  ed_decision_discharge_LOS = ed_discharge,
  trauma_team_activated = activated,
  risk_group = risk
)

# Take a look at the overall output of the function
seqic_9_result$overall |>
tidyr::pivot_longer(cols = -1,
                    names_to = "Indicator",
                    values_to = "Values"
                    )
#> # A tibble: 18 × 3
#>    data              Indicator          Values
#>    <chr>             <chr>               <dbl>
#>  1 population/sample numerator_9a_all    3    
#>  2 population/sample denominator_9a_all  7    
#>  3 population/sample seqic_9a_all        0.429
#>  4 population/sample numerator_9b_all    1    
#>  5 population/sample denominator_9b_all  7    
#>  6 population/sample seqic_9b_all        0.143
#>  7 population/sample numerator_9c_all    5    
#>  8 population/sample denominator_9c_all  7    
#>  9 population/sample seqic_9c_all        0.714
#> 10 population/sample numerator_9d_all    2    
#> 11 population/sample denominator_9d_all  7    
#> 12 population/sample seqic_9d_all        0.286
#> 13 population/sample numerator_9e_all    7    
#> 14 population/sample denominator_9e_all  7    
#> 15 population/sample seqic_9e_all        1    
#> 16 population/sample numerator_9f_all    4    
#> 17 population/sample denominator_9f_all  7    
#> 18 population/sample seqic_9f_all        0.571
```
