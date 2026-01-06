# Relative Mortality Metric (RMM) Calculation

Calculates the Relative Mortality Metric (RMM) from Napoli et al. (2017)
based on patient survival probabilities (Ps) and actual outcomes. The
function groups patients into bins based on their survival probability
scores (Ps) and computes a weighted mortality metric along with
confidence intervals. For more information on the methods used in this
function, see as well Schroeder et al. (2019), and Kassar et al. (2016).

The Relative Mortality Metric (RMM) quantifies the performance of a
center in comparison to the anticipated mortality based on the TRISS
national benchmark. The RMM measures the difference between observed and
expected mortality, with a range from -1 to 1.

- An RMM of 0 indicates that the observed mortality aligns with the
  expected national benchmark across all acuity levels.

- An RMM greater than 0 indicates better-than-expected performance,
  where the center is outperforming the national benchmark.

- An RMM less than 0 indicates under-performance, where the center’s
  observed mortality is higher than the expected benchmark.

This metric helps assess how a center's mortality compares to the
national standards, guiding quality improvement efforts. `rmm()`
utilizes bootstrap sampling to calculate the confidence intervals via
the standard error method.

## Usage

``` r
rmm(
  data,
  Ps_col,
  outcome_col,
  group_vars = NULL,
  n_samples = 100,
  Divisor1 = 5,
  Divisor2 = 5,
  Threshold_1 = 0.9,
  Threshold_2 = 0.99,
  bootstrap_ci = TRUE,
  pivot = FALSE,
  seed = NULL
)
```

## Arguments

- data:

  A data frame or tibble containing the data.

- Ps_col:

  The name of the column containing the survival probabilities (Ps).
  Should be numeric on a scale from 0 to 1.

- outcome_col:

  The name of the column containing the outcome data. It should be
  binary, with values indicating patient survival. A value of `1` should
  represent "alive" (survived), while `0` should represent "dead" (did
  not survive). `TRUE/FALSE` are accepted as well. Ensure the column
  contains only these possible values.

- group_vars:

  Optional character vector specifying grouping variables for stratified
  analysis. If `NULL`, the calculation is performed on the entire
  dataset.

- n_samples:

  A numeric value indicating the number of bootstrap samples to take
  from the data source.

- Divisor1:

  A positive numeric value controlling the coarseness of bins for Ps
  values below `Threshold_1`. It scales the number of steps from the
  start of the dataset up to the `Threshold_1` cut point. Larger values
  produce fewer, broader bins; smaller values produce more, narrower
  bins. Defaults to `5`.

- Divisor2:

  A positive numeric value controlling the coarseness of bins for Ps
  values between `Threshold_1` and `Threshold_2`. Larger values yield
  wider bins, and smaller values yield narrower bins in this range.
  Defaults to `5`.

- Threshold_1:

  A numeric value that defines the lower bound of the high-survival
  probability range in `Ps_col`. The function identifies the first index
  where `Ps_col` exceeds this value and begins applying smaller bin
  widths from that point onward. Defaults to `0.9`, meaning binning
  changes once Ps \> 0.90.

- Threshold_2:

  A numeric value that defines the upper bound of the high-survival
  probability range in `Ps_col`. The function identifies the first index
  where `Ps_col` exceeds this value. Between `Threshold_1` and
  `Threshold_2`, finer binning is applied; above `Threshold_2`, binning
  may again change. Defaults to `0.99`, meaning the special binning
  range is between Ps values of 0.90 and 0.99.

- bootstrap_ci:

  A logical indicating whether to return the relative mortality metric
  estimate and 95% confidence intervals using bootstrap sampling.
  Default is `TRUE`.

- pivot:

  A logical indicating whether to return the results in a long format
  (`pivot = TRUE`) or wide format (`pivot = FALSE`, default). Use with
  caution in tandem with `group_vars` if the grouping variable is of a
  different class than `rmm()`'s outputs, such as `factor` or
  `character` grouping variables.

- seed:

  Optional numeric value to set a random seed for reproducibility. If
  `NULL` (default), no seed is set.

## Value

A tibble containing the Relative Mortality Metric (RMM) and related
statistics:

- `population_RMM_LL`: The lower bound of the 95% confidence interval
  for the population RMM.

- `population_RMM`: The final calculated Relative Mortality Metric for
  the population existing in `data`.

- `population_RMM_UL`: The upper bound of the 95% confidence interval
  for the population RMM.

- `population_CI`: The confidence interval width for the population RMM.

- `bootstrap_RMM_LL`: The lower bound of the 95% confidence interval for
  the bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)

- `bootstrap_RMM`: The average RMM value calculated for the bootstrap
  sample. (optional, if `bootstrap_ci = TRUE`)

- `bootstrap_RMM_UL`: The upper bound of the 95% confidence interval for
  the bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)

- `bootstrap_CI`: The width of the 95% confidence interval for the
  bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)

- If `pivot = TRUE`, the results will be in long format with two
  columns: `stat` and `value`, where each row corresponds to one of the
  calculated statistics.

- If `pivot = FALSE` (default), the results will be returned in wide
  format, with each statistic as a separate column.

## Details

Like other statistical computing functions, `rmm()` is happiest without
missing data. It is best to pass complete probability of survival and
mortality outcome data to the function for optimal performance. With
smaller datasets, this is especially helpful. However, `rmm()` will
throw a warning about missing values, if any exist in `Ps_col` and/or
`outcome_col`.

`rmm()` assumes `Ps_col` contains probabilities derived from real-world
inputs for the Trauma Injury Severity Score (TRISS) model. Synthetic or
low-variability data (especially with small sample sizes) may not
reflect the distribution of TRISS-derived survival probabilities. This
can result in unstable estimates or function failure due to insufficient
dispersion. With small sample sizes, it may be important to use smaller
values with the divisor arguments and adjust the thresholds (based on
the distribution of the `Ps_col` values) to create bins that better
accommodate the data.

Due to the use of bootstrap sampling within the function, users should
consider setting the random number `seed` within `rmm()` for
reproducibility.

## Note

This function will produce the most reliable and interpretable results
when using a dataset that has one row per patient, with each column
being a feature.

By default, `rmm()` derives bin cut points from the full dataset’s
distribution. This ensures comparability across groups when `group_vars`
is used. To tailor results to a specific group (e.g., a single
hospital), filter the dataset to that subgroup before calling `rmm()`.
The function will then compute bins and related statistics using only
that subset’s `Ps_col` distribution. When `group_vars` is used, and ff a
group lacks observations within one or more bins,
[`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)
will compute statistics only for the bins that contain data. Bins with
no observations are excluded from the summary for that group.

## References

Kassar, O.M., Eklund, E.A., Barnhardt, W.F., Napoli, N.J., Barnes, L.E.,
Young, J.S. (2016). Trauma survival margin analysis: A dissection of
trauma center performance through initial lactate. The American Surgeon,
82(7), 649-653. <doi:10.1177/000313481608200733>

Napoli, N. J., Barnhardt, W., Kotoriy, M. E., Young, J. S., & Barnes, L.
E. (2017). Relative mortality analysis: A new tool to evaluate clinical
performance in trauma centers. IISE Transactions on Healthcare Systems
Engineering, 7(3), 181–191. <doi:10.1080/24725579.2017.1325948>

Schroeder, P. H., Napoli, N. J., Barnhardt, W. F., Barnes, L. E., &
Young, J. S. (2018). Relative mortality analysis of the “golden hour”: A
comprehensive acuity stratification approach to address disagreement in
current literature. Prehospital Emergency Care, 23(2), 254–262.
<doi:10.1080/10903127.2018.1489021>

## See also

[`probability_of_survival()`](https://bemts-hhs.github.io/traumar/reference/probability_of_survival.md),
[`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md),
and
[`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md)

## Author

Nicolas Foss, Ed.D, MS, original implementation in MATLAB by Nicholas J.
Napoli, Ph.D., MS

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

# Example usage of the `rmm` function
rmm(data = data, Ps_col = Ps,
    outcome_col = survival,
    Divisor1 = 4,
    Divisor2 = 4,
    n_samples = 10
    )
#> # A tibble: 1 × 8
#>   population_RMM_LL population_RMM population_RMM_UL population_CI
#>               <dbl>          <dbl>             <dbl>         <dbl>
#> 1            -0.106        0.00247             0.111         0.108
#> # ℹ 4 more variables: bootstrap_RMM_LL <dbl>, bootstrap_RMM <dbl>,
#> #   bootstrap_RMM_UL <dbl>, bootstrap_CI <dbl>

# pivot!
rmm(data = data, Ps_col = Ps,
    outcome_col = survival,
    Divisor1 = 4,
    Divisor2 = 4,
    n_samples = 10,
    pivot = TRUE
    )
#> # A tibble: 8 × 2
#>   stat                 value
#>   <chr>                <dbl>
#> 1 population_RMM_LL -0.106  
#> 2 population_RMM     0.00247
#> 3 population_RMM_UL  0.111  
#> 4 population_CI      0.108  
#> 5 bootstrap_RMM_LL  -0.0145 
#> 6 bootstrap_RMM     -0.00544
#> 7 bootstrap_RMM_UL   0.00366
#> 8 bootstrap_CI       0.00910

# Create example grouping variable (e.g., hospital)
hospital <- sample(c("Hospital A", "Hospital B"), n_patients, replace = TRUE)

# Create data frame
data <- data.frame(
  Ps = Ps,
  survival = survival_outcomes,
  hospital = hospital
) |>
  dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))

# Example usage of the `rmm` function with grouping by hospital
rmm(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  group_vars = "hospital",
  Divisor1 = 4,
  Divisor2 = 4,
  n_samples = 10
)
#> # A tibble: 2 × 9
#>   hospital   population_RMM_LL population_RMM population_RMM_UL population_CI
#>   <chr>                  <dbl>          <dbl>             <dbl>         <dbl>
#> 1 Hospital A            -0.131         0.0211             0.173         0.152
#> 2 Hospital B            -0.169        -0.0149             0.139         0.154
#> # ℹ 4 more variables: bootstrap_RMM_LL <dbl>, bootstrap_RMM <dbl>,
#> #   bootstrap_RMM_UL <dbl>, bootstrap_CI <dbl>

# Pivoted output for easier visualization
rmm(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  group_vars = "hospital",
  Divisor1 = 4,
  Divisor2 = 4,
  n_samples = 10,
  pivot = TRUE
)
#> # A tibble: 16 × 3
#>    hospital   stat                   value
#>    <chr>      <chr>                  <dbl>
#>  1 Hospital A population_RMM_LL -0.131    
#>  2 Hospital A population_RMM     0.0211   
#>  3 Hospital A population_RMM_UL  0.173    
#>  4 Hospital A population_CI      0.152    
#>  5 Hospital A bootstrap_RMM_LL   0.00784  
#>  6 Hospital A bootstrap_RMM      0.0202   
#>  7 Hospital A bootstrap_RMM_UL   0.0326   
#>  8 Hospital A bootstrap_CI       0.0124   
#>  9 Hospital B population_RMM_LL -0.169    
#> 10 Hospital B population_RMM    -0.0149   
#> 11 Hospital B population_RMM_UL  0.139    
#> 12 Hospital B population_CI      0.154    
#> 13 Hospital B bootstrap_RMM_LL  -0.0231   
#> 14 Hospital B bootstrap_RMM     -0.0115   
#> 15 Hospital B bootstrap_RMM_UL   0.0000725
#> 16 Hospital B bootstrap_CI       0.0116   
```
