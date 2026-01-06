# Create Nonlinear Probability of Survival Bins

This function generates nonlinear bins for probability of survival data
based on specified thresholds and divisors as specified in Napoli et al.
(2017), Schroeder et al. (2019), and Kassar et al. (2016). This function
calculates bin statistics, including mean, standard deviation, total
alive, total dead, count, and percentage for each bin.

## Usage

``` r
nonlinear_bins(
  data,
  Ps_col,
  outcome_col,
  group_vars = NULL,
  divisor1 = 5,
  divisor2 = 5,
  threshold_1 = 0.9,
  threshold_2 = 0.99
)
```

## Arguments

- data:

  A `data.frame` or `tibble` containing the probability of survival data
  for a set of patients.

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

  Optional grouping variables for bin statistics calculations. These
  should be specified as quoted column names.

- divisor1:

  A positive numeric value controlling the coarseness of bins for Ps
  values below `threshold_1`. It scales the number of steps from the
  start of the dataset up to the `threshold_1` cut point. Larger values
  produce fewer, broader bins; smaller values produce more, narrower
  bins. Defaults to `5`.

- divisor2:

  A positive numeric value controlling the coarseness of bins for Ps
  values between `threshold_1` and `threshold_2`. Larger values yield
  wider bins, and smaller values yield narrower bins in this range.
  Defaults to `5`.

- threshold_1:

  A numeric value that defines the lower bound of the high-survival
  probability range in `Ps_col`. The function identifies the first index
  where `Ps_col` exceeds this value and begins applying smaller bin
  widths from that point onward. Defaults to `0.9`, meaning binning
  changes once Ps \> 0.90.

- threshold_2:

  A numeric value that defines the upper bound of the high-survival
  probability range in `Ps_col`. The function identifies the first index
  where `Ps_col` exceeds this value. Between `threshold_1` and
  `threshold_2`, finer binning is applied; above `threshold_2`, binning
  may again change. Defaults to `0.99`, meaning the special binning
  range is between Ps values of 0.90 and 0.99.

## Value

A list with two elements:

- `intervals`: A vector defining bin boundaries for probability of
  survival.

- `bin_stats`: A `tibble` containing:

  - `bin_number`: Bin index.

  - `bin_start`, `bin_end`: Bin range.

  - `mean`, `sd`: Mean and standard deviation of `Ps_col` within the
    bin.

  - `Pred_Survivors_b`, `Pred_Deaths_b`: Predicted counts of survivors
    and decedents, respectively.

  - `AntiS_b`, `AntiM_b`: Anticipated proportion survived, and deceased,
    respectively.

  - `alive`, `dead`: Count of observed survivors and non-survivors.

  - `count`: Total records in the bin.

  - `percent`: Percentage of total records within each bin.

## Details

Like other statistical computing functions, `nonlinear_bins()` is
happiest without missing data. It is best to pass complete probability
of survival and outcome data to the function for optimal performance.
With smaller datasets, this is especially helpful. However,
`nonlinear_bins()` will throw a warning about missing values, if any
exist in `Ps_col` and/or `outcome_col`.

`nonlinear_bins()` assumes `Ps_col` contains probabilities derived from
real-world inputs for the Trauma Injury Severity Score (TRISS) model.
Synthetic or low-variability data (especially with small sample sizes)
may not reflect the distribution of TRISS-derived survival
probabilities. This can result in unstable estimates or function failure
due to insufficient dispersion. With small sample sizes, it may be
important to use smaller values with the divisor arguments and adjust
the thresholds (based on the distribution of the `Ps_col` values) to
create bins that better accommodate the data.

By default, `nonlinear_bins()` derives bin cut points from the full
dataset’s distribution. This ensures comparability across groups when
`group_vars` is used. To tailor binning to a specific group (e.g., a
single hospital), filter the dataset to that subgroup before calling
`nonlinear_bins()`. The function will then compute bins and related
statistics using only that subset’s `Ps_col` distribution. When
`group_vars` is used, and ff a group lacks observations within one or
more bins,
[`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)
will compute statistics only for the bins that contain data. Bins with
no observations are excluded from the summary for that group.

The `threshold_1` and `threshold_2` arguments set probability cut points
that define the start and end of a high-survival range where bin widths
are adjusted for finer resolution. The `divisor1` and `divisor2`
arguments are scaling factors that determine how many bins are created
before and within this high-survival range, respectively. Lower divisors
yield narrower bins, capturing more detail, while higher divisors yield
broader bins, smoothing the distribution.

## Note

This function will produce the most reliable and interpretable results
when using a dataset that has one row per patient, with each column
being a feature.

The `mean` and `AntiS_b` are approximately equivalent in this context.
They are kept in the output for clarity.

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
[`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)

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

# Apply the nonlinear_bins function
results <- nonlinear_bins(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  divisor1 = 4,
  divisor2 = 4,
  threshold_1 = 0.9,
  threshold_2 = 0.99
)

# View results
results$intervals
#> [1] 0.0002015449 0.0256191282 0.1455317587 0.4842820556 0.9003870455
#> [6] 0.9285354475 0.9518925450 0.9722272703 0.9968989233
results$bin_stats
#> # A tibble: 8 × 13
#>   bin_number bin_start bin_end    mean      sd Pred_Survivors_b Pred_Deaths_b
#>        <int>     <dbl>   <dbl>   <dbl>   <dbl>            <dbl>         <dbl>
#> 1          1  0.000202  0.0256 0.00935 0.00722             10.4       1106.  
#> 2          2  0.0256    0.146  0.0732  0.0345              81.7       1033.  
#> 3          3  0.146     0.484  0.293   0.0959             327.         788.  
#> 4          4  0.484     0.900  0.697   0.124              777.         337.  
#> 5          5  0.900     0.929  0.916   0.00790            114.          10.5 
#> 6          6  0.929     0.952  0.940   0.00680            117.           7.50
#> 7          7  0.952     0.972  0.963   0.00564            120.           4.65
#> 8          8  0.972     0.997  0.984   0.00686            162.           2.67
#> # ℹ 6 more variables: AntiS_b <dbl>, AntiM_b <dbl>, alive <int>, dead <int>,
#> #   count <int>, percent <dbl>

# Example with grouping by a categorical variable

# Run the function using a single grouping variable
results_grouped <- nonlinear_bins(
  data,
  Ps_col = Ps,
  outcome_col = survival,
  group_vars = "groups"
)

# View grouped results
results_grouped$bin_stats
#> # A tibble: 20 × 14
#>    groups bin_number bin_start bin_end    mean      sd Pred_Survivors_b
#>    <chr>       <int>     <dbl>   <dbl>   <dbl>   <dbl>            <dbl>
#>  1 A               1  0.000202  0.0165 0.00636 0.00465             2.94
#>  2 A               2  0.0165    0.0794 0.0418  0.0183             19.1 
#>  3 A               3  0.0794    0.252  0.155   0.0516             68.5 
#>  4 A               4  0.252     0.570  0.393   0.0953            172.  
#>  5 A               5  0.570     0.900  0.734   0.0976            320.  
#>  6 A               6  0.900     0.923  0.912   0.00660            39.2 
#>  7 A               7  0.923     0.941  0.933   0.00541            47.6 
#>  8 A               8  0.941     0.962  0.952   0.00587            44.7 
#>  9 A               9  0.962     0.976  0.968   0.00426            54.2 
#> 10 A              10  0.976     0.997  0.986   0.00610            64.1 
#> 11 B               1  0.000202  0.0165 0.00657 0.00466             2.83
#> 12 B               2  0.0165    0.0794 0.0417  0.0180             18.1 
#> 13 B               3  0.0794    0.252  0.153   0.0496             68.7 
#> 14 B               4  0.252     0.570  0.405   0.0920            184.  
#> 15 B               5  0.570     0.900  0.746   0.100             340.  
#> 16 B               6  0.900     0.923  0.914   0.00659            52.1 
#> 17 B               7  0.923     0.941  0.933   0.00510            45.7 
#> 18 B               8  0.941     0.962  0.952   0.00553            50.5 
#> 19 B               9  0.962     0.976  0.969   0.00371            42.6 
#> 20 B              10  0.976     0.997  0.985   0.00550            73.9 
#> # ℹ 7 more variables: Pred_Deaths_b <dbl>, AntiS_b <dbl>, AntiM_b <dbl>,
#> #   alive <int>, dead <int>, count <int>, percent <dbl>
```
