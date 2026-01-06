# Bin-Level Summary for Relative Mortality Metric (RMM)

Calculates a bin-level summary for the Relative Mortality Metric (RMM)
from Napoli et al. (2017) by grouping data into bins based on survival
probabilities (Ps) and summarizing outcomes within each bin. This
function returns statistics such as total alive, total dead, estimated
mortality, anticipated mortality, and confidence intervals for each bin.
For more information on the methods used in this function, see as well
Schroeder et al. (2019), and Kassar et al. (2016).

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
national standards, guiding quality improvement
efforts.`rm_bin_summary()` utilizes bootstrap sampling to calculate the
confidence intervals via the standard error method.

## Usage

``` r
rm_bin_summary(
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

- seed:

  Optional numeric value to set a random seed for reproducibility. If
  `NULL` (default), no seed is set.

## Value

A tibble containing bin-level statistics including:

- `bin_number`: The bin to which each record was assigned.

- `TA_b`: Total alive in each bin (number of patients who survived).

- `TD_b`: Total dead in each bin (number of patients who did not
  survive).

- `N_b`: Total number of patients in each bin.

- `EM_b`: Estimated mortality rate for each bin (TD_b / (TA_b + TD_b)).

- `AntiS_b`: The anticipated survival rate for each bin.

- `AntiM_b`: The anticipated mortality rate for each bin.

- `bin_start`: The lower bound of the survival probability range for
  each bin.

- `bin_end`: The upper bound of the survival probability range for each
  bin.

- `midpoint`: The midpoint of the bin range (calculated as (bin_start +
  bin_end) / 2).

- `R_b`: The width of each bin (bin_end - bin_start).

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

## Details

Like other statistical computing functions, `rm_bin_summary()` is
happiest without missing data. It is best to pass complete probability
of survival and mortality outcome data to the function for optimal
performance. With smaller datasets, this is especially helpful. However,
`rm_bin_summary()` will throw a warning about missing values, if any
exist in `Ps_col` and/or `outcome_col`.

`rm_bin_summary()` assumes `Ps_col` contains probabilities derived from
real-world inputs for the Trauma Injury Severity Score (TRISS) model.
Synthetic or low-variability data (especially with small sample sizes)
may not reflect the distribution of TRISS-derived survival
probabilities. This can result in unstable estimates or function failure
due to insufficient dispersion. With small sample sizes, it may be
important to use smaller values with the divisor arguments and adjust
the thresholds (based on the distribution of the `Ps_col` values) to
create bins that better accommodate the data.

Due to the use of bootstrap sampling within the function, users should
consider setting the random number seed within `rm_bin_summary()` using
the `seed` argument for reproducibility.

## Note

This function will produce the most reliable and interpretable results
when using a dataset that has one row per patient, with each column
being a feature.

By default, `rm_bin_summary()` derives bin cut points from the full
dataset’s distribution. This ensures comparability across groups when
`group_vars` is used. To tailor results to a specific group (e.g., a
single hospital), filter the dataset to that subgroup before calling
`rm_bin_summary()`. The function will then compute bins and related
statistics using only that subset’s `Ps_col` distribution. When
`group_vars` is used, and ff a group lacks observations within one or
more bins, `rm_bin_summary()` will compute statistics only for the bins
that contain data. Bins with no observations are excluded from the
summary for that group.

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
[`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md), and
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

# Example usage of the `rm_bin_summary()` function
rm_bin_summary(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  n_samples = 10,
  Divisor1 = 4,
  Divisor2 = 4
)
#> # A tibble: 8 × 19
#>   bin_number  TA_b  TD_b   N_b  EM_b AntiS_b AntiM_b bin_start bin_end midpoint
#>        <int> <int> <int> <int> <dbl>   <dbl>   <dbl>     <dbl>   <dbl>    <dbl>
#> 1          1     9  1107  1116 0.992 0.00935  0.991   0.000202  0.0256   0.0129
#> 2          2    72  1043  1115 0.935 0.0732   0.927   0.0256    0.146    0.0856
#> 3          3   300   815  1115 0.731 0.293    0.707   0.146     0.484    0.315 
#> 4          4   805   309  1114 0.277 0.697    0.303   0.484     0.900    0.692 
#> 5          5   115    10   125 0.08  0.916    0.0844  0.900     0.929    0.914 
#> 6          6   116     9   125 0.072 0.940    0.0600  0.929     0.952    0.940 
#> 7          7   119     6   125 0.048 0.963    0.0372  0.952     0.972    0.962 
#> 8          8   165     0   165 0     0.984    0.0162  0.972     0.997    0.985 
#> # ℹ 9 more variables: R_b <dbl>, population_RMM_LL <dbl>, population_RMM <dbl>,
#> #   population_RMM_UL <dbl>, population_CI <dbl>, bootstrap_RMM_LL <dbl>,
#> #   bootstrap_RMM <dbl>, bootstrap_RMM_UL <dbl>, bootstrap_CI <dbl>

# Create example grouping variable (e.g., hospital)
hospital <- sample(c("Hospital A", "Hospital B"), n_patients, replace = TRUE)

# Create data frame
data <- data.frame(
  Ps = Ps,
  survival = survival_outcomes,
  hospital = hospital
) |>
  dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))

# Example usage of the `rm_bin_summary()` function with grouping
rm_bin_summary(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  group_vars = "hospital", # Stratifies by hospital
  n_samples = 10,
  Divisor1 = 4,
  Divisor2 = 4
)
#> # A tibble: 16 × 20
#>    hospital   bin_number  TA_b  TD_b   N_b   EM_b AntiS_b AntiM_b bin_start
#>    <chr>           <int> <int> <int> <int>  <dbl>   <dbl>   <dbl>     <dbl>
#>  1 Hospital A          1     6   544   550 0.989  0.00976  0.990   0.000202
#>  2 Hospital A          2    44   518   562 0.922  0.0729   0.927   0.0256  
#>  3 Hospital A          3   151   392   543 0.722  0.294    0.706   0.146   
#>  4 Hospital A          4   391   157   548 0.286  0.701    0.299   0.484   
#>  5 Hospital A          5    60     5    65 0.0769 0.915    0.0848  0.900   
#>  6 Hospital A          6    65     5    70 0.0714 0.941    0.0590  0.929   
#>  7 Hospital A          7    51     2    53 0.0377 0.963    0.0371  0.952   
#>  8 Hospital A          8    91     0    91 0      0.983    0.0170  0.972   
#>  9 Hospital B          1     3   563   566 0.995  0.00895  0.991   0.000202
#> 10 Hospital B          2    28   525   553 0.949  0.0735   0.926   0.0256  
#> 11 Hospital B          3   149   423   572 0.740  0.293    0.707   0.146   
#> 12 Hospital B          4   414   152   566 0.269  0.694    0.306   0.484   
#> 13 Hospital B          5    55     5    60 0.0833 0.916    0.0839  0.900   
#> 14 Hospital B          6    51     4    55 0.0727 0.939    0.0613  0.929   
#> 15 Hospital B          7    68     4    72 0.0556 0.963    0.0372  0.952   
#> 16 Hospital B          8    74     0    74 0      0.985    0.0151  0.972   
#> # ℹ 11 more variables: bin_end <dbl>, midpoint <dbl>, R_b <dbl>,
#> #   population_RMM_LL <dbl>, population_RMM <dbl>, population_RMM_UL <dbl>,
#> #   population_CI <dbl>, bootstrap_RMM_LL <dbl>, bootstrap_RMM <dbl>,
#> #   bootstrap_RMM_UL <dbl>, bootstrap_CI <dbl>
```
