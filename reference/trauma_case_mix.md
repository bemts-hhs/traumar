# View the Current Patient Population Case Mix Compared to the Major Trauma Study Case Mix

This function compares the current patient population's case mix (based
on probability of survival, Ps) to the MTOS case mix by binning patients
into specific Ps ranges. It returns the fraction of patients in each
range and compares it to the MTOS distribution. For more information on
the methods used in these calculations, please see Flora (1978) and Boyd
et al. (1987).

## Usage

``` r
trauma_case_mix(df, Ps_col, outcome_col)
```

## Arguments

- df:

  A data frame containing patient data.

- Ps_col:

  The name of the column containing the probability of survival (Ps)
  values.

- outcome_col:

  The name of the column containing the binary outcome data (valid
  values are 1 or TRUE for alive, 0 or FALSE for dead).

## Value

A data frame containing:

- `Ps_range`: The probability of survival range category.

- `current_fraction`: The fraction of patients in the current dataset
  within each Ps range.

- `MTOS_distribution`: The reference distribution of patients in each Ps
  range based on the MTOS study.

- `survivals`: The number of observed survivors (outcome = 1) in each Ps
  range.

- `predicted_survivals`: The sum of predicted survivals (sum of Ps
  values) in each Ps range.

- `deaths`: The number of observed deaths (outcome = 0) in each Ps
  range.

- `predicted_deaths`: The sum of predicted deaths (sum of 1 - Ps values)
  in each Ps range.

- `count`: The total number of patients in each Ps range.

## Details

The function checks whether the `outcome_col` contains values
representing a binary outcome. It also ensures that `Ps_col` contains
numeric values within the range 0 to 1. If any values exceed 1, a
warning is issued. The patients are then grouped into predefined Ps
ranges, and the function compares the fraction of patients in each range
with the MTOS case mix distribution.

Like other statistical computing functions, `trauma_case_mix()` is
happiest without missing data. It is best to pass complete probability
of survival and outcome data to the function for optimal performance.
With smaller datasets, this is especially helpful. However,
`trauma_case_mix()` will throw a warning about missing values, if any
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

# Compare the current case mix with the MTOS case mix
trauma_case_mix(data, Ps_col = Ps, outcome_col = death)
#>      Ps_range current_fraction MTOS_distribution survivals predicted_survivals
#> 1 0.00 - 0.25           0.5414             0.010      2534            187.9191
#> 2 0.26 - 0.50           0.1432             0.043       468            269.5871
#> 3 0.51 - 0.75           0.1278             0.000       217            405.8877
#> 4 0.76 - 0.90           0.0870             0.052        58            366.1312
#> 5 0.91 - 0.95           0.0508             0.053        18            237.6390
#> 6 0.96 - 1.00           0.0498             0.842         4            243.4833
#>   deaths predicted_deaths count
#> 1    173      2519.080869  2707
#> 2    248       446.412896   716
#> 3    422       233.112251   639
#> 4    377        68.868790   435
#> 5    236        16.361033   254
#> 6    245         5.516716   249
```
