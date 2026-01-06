# Calculate Trauma Hospital Performance Based on Robust and Validated Measures

This function calculates trauma hospital performance based on the M, W,
and Z scores, which are derived from survival probability and mortality
data, using established methods. It computes the W-score, M-score, and
Z-score based on the provided dataset and calculates performance metrics
for trauma programs. For more information on the methods used in this
function, please see Champion et al. (1990) on the W score, and Flora
(1978) and Boyd et al. (1987) on the M and Z scores.

## Usage

``` r
trauma_performance(
  df,
  Ps_col,
  outcome_col,
  z_method = c("survival", "mortality")
)
```

## Arguments

- df:

  A data frame containing patient data.

- Ps_col:

  The name of the column containing the probability of survival (Ps).
  The values should be numeric and between 0 and 1. Values greater than
  1 will be automatically converted to decimal format by dividing by
  100.

- outcome_col:

  The name of the column containing the binary outcome data.

  The column should contain binary values indicating the patient
  outcome. Valid values include 1 (dead) and 0 (alive), or TRUE (dead)
  and FALSE (alive). The function will check values in this column and
  expects them to represent the outcome in a binary form.

- z_method:

  A character vector indicating which method to use for calculating the
  Z-score. Must be one of "survival" or "mortality". The default is
  "survival".

## Value

A tibble containing the following calculations:

- `N_Patients`: The total number of patients included in the analysis.

- `N_Survivors`: The total number of patients who survived, based on the
  provided outcome data.

- `N_Deaths`: The total number of patients who died, based on the
  provided outcome data.

- `Predicted_Survivors`: The total predicted number of survivors based
  on the survival probability (`Ps`) for all patients.

- `Predicted_Deaths`: The total predicted number of deaths, calculated
  as `1 - Ps` for all patients.

- `Patient_Estimate`: The estimated number of patients who survived that
  were predicted to die, calculated based on the W-score. This value
  reflects the difference between the actual and predicted number of
  deceased patients.

- `W_Score`: The W-score, representing the difference between the
  observed and expected number of survivors per 100 patients. A positive
  W-score indicates that more patients survived than expected, while a
  negative score indicates that fewer patients survived than expected.

- `M_Score`: The M-score, which compares the observed patient case mix
  to the Major Trauma Outcomes Study (MTOS) case mix. A higher score
  indicates that the patient mix is more similar to MTOS, while a lower
  score indicates a dissimilar mix. Based on the MTOS literature, an
  M_Score \>= 0.88 indicates that the Z_Score comes from distribution
  similar enough to the MTOS Ps distribution.

- `Z_Score`: The Z-score, which quantifies the difference between the
  actual and predicted mortality (if `z_method = "mortality"`) or
  survival (if `z_method = "survival"`). A Z-score \> 1.96 is considered
  to point to the statistical significance of the W-Score at alpha =
  0.05 level for survival. The positive Z_Score indicates that more
  patients survived than predicted, while a negative Z-score indicates
  fewer survivors than predicted.

## Details

The function checks whether the `outcome_col` contains values
representing a binary outcome. It also ensures that `Ps_col` contains
numeric values within the range 0 to 1. If any values exceed 1, a
warning is issued. The patients are then grouped into predefined Ps
ranges, and the function compares the fraction of patients in each range
with the MTOS case mix distribution.

Like other statistical computing functions, `trauma_performance()` is
happiest without missing data. It is best to pass complete probability
of survival and outcome data to the function for optimal performance.
With smaller datasets, this is especially helpful. However,
`trauma_performance()` will throw a warning about missing values, if any
exist in `Ps_col` and/or `outcome_col`.

## Note

This function will produce the most reliable and interpretable results
when using a dataset that has one row per patient, with each column
being a feature.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Generate example data
set.seed(123)

# Parameters
# Total number of patients
n_patients <- 5000

# Arbitrary group labels
groups <- sample(x = LETTERS[1:2], size = n_patients, replace = TRUE)

# Trauma types
trauma_type_values <- sample(
  x = c("Blunt", "Penetrating"),
  size = n_patients,
  replace = TRUE
)

# RTS values
rts_values <- sample(
  x = seq(from = 0, to = 7.8408, by = 0.005),
  size = n_patients,
  replace = TRUE
)

# patient ages
ages <- sample(
  x = seq(from = 0, to = 100, by = 1),
  size = n_patients,
  replace = TRUE
)

# ISS scores
iss_scores <- sample(
  x = seq(from = 0, to = 75, by = 1),
  size = n_patients,
  replace = TRUE
)

# Generate survival probabilities (Ps)
Ps <- traumar::probability_of_survival(
  trauma_type = trauma_type_values,
  age = ages,
  rts = rts_values,
  iss = iss_scores
)

# Simulate survival outcomes based on Ps
survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)

# Create data frame
data <- data.frame(Ps = Ps, survival = survival_outcomes, groups = groups) |>
  dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))

# Calculate trauma performance (W, M, Z scores)
trauma_performance(data, Ps_col = Ps, outcome_col = death)
#> # A tibble: 1 × 9
#>   N_Patients N_Survivors N_Deaths Predicted_Survivors Predicted_Deaths
#>        <int>       <int>    <int>               <dbl>            <dbl>
#> 1       5000        1701     3299               1711.            3289.
#> # ℹ 4 more variables: Patient_Estimate <dbl>, W_Score <dbl>, M_Score <dbl>,
#> #   Z_Score <dbl>
```
