
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/bemts-hhs/traumar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bemts-hhs/traumar/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/traumar)](https://CRAN.R-project.org/package=traumar)
[![Codecov test
coverage](https://codecov.io/gh/bemts-hhs/traumar/graph/badge.svg)](https://app.codecov.io/gh/bemts-hhs/traumar)
<!-- badges: end -->

# traumar <img src="man/figures/try_object.png" align="right" width="200" style="display: block; margin: auto;" />

Continuous Quality Improvement (CQI) and Process Improvement (PI) are
essential pillars of healthcare, particularly in the care of injured
patients. However, hospitals, trauma systems, and their trauma program
managers (TPMs) often lack access to standardized quality measures
derived from academic literature. The {traumar} package addresses this
gap by providing tools to calculate quality measures related to relative
mortality efficiently and accurately. By automating these calculations,
{traumar} empowers hospital systems, trauma networks, and TPMs to focus
their efforts on analyzing outcomes and driving meaningful improvements
in patient care. Whether you’re seeking to enhance PI initiatives or
streamline CQI processes, {traumar} serves as a valuable resource for
advancing trauma care quality.

## Installation

You can install the development version of traumar from
[GitHub](https://github.com/bemts-hhs/traumar) with:

``` r
# install.packages("pak")
pak::pak("bemts-hhs/traumar")
```

## There is More!

{traumar} has many functions to help you in your data analysis journey!
Check out the additional package documentation at
[bemts-hhs/github.io/trauma](https://bemts-hhs/github.io/trauma) where
you can find examples of each function the package has to offer.

## Calculating the W-Score

The W-Score tells us how many survivals (or deaths) on average out of
every 100 cases seen in a trauma center. Using R, we can do this with
the {traumar} package.

### First, we will create the data for these examples

``` r

# Generate example data with high negative skewness
set.seed(123)

# Parameters
n_patients <- 10000  # Total number of patients

# Generate survival probabilities (Ps) using a logistic distribution
Ps <- plogis(rnorm(n_patients, mean = 2, sd = 1.5))  # Skewed towards higher values

# Simulate survival outcomes based on Ps
survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)

# Create data frame
data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
```

### The W-Score!

``` r

# Calculate trauma performance (W, M, Z scores)
trauma_performance(data, Ps_col = Ps, outcome_col = death)
#> # A tibble: 10 × 2
#>    Calculation_Name        Value
#>    <chr>                   <dbl>
#>  1 N_Patients          10000    
#>  2 N_Survivors          8137    
#>  3 N_Deaths             1863    
#>  4 Predicted_Survivors  8097.   
#>  5 Predicted_Deaths     1903.   
#>  6 SE_Predictions         34.2  
#>  7 Patient_Estimate       40.3  
#>  8 W_Score                 0.403
#>  9 M_Score                 0.374
#> 10 Z_Score                 1.18
```

## Comparing the Probability of Survival Distribution of your Patient Mix to the [Major Trauma Outcomes Study](https://journals.lww.com/jtrauma/Abstract/1990/11000/The_Major_Trauma_Outcome_Study__Establishing.8.aspx)

The M and Z scores are calculated using methods defined in the
[literature](https://journals.lww.com/jtrauma/abstract/1978/10000/a_method_for_comparing_survival_of_burn_patients.3.aspx)
may not be meaningful if your the distribution of the probability of
survival measure is not similar enough to the Major Trauma Outcomes
Study distribution. {traumar} provides a way to check this in your data
analysis script, or even from the console. The `trauma_performance()`
function does this under the hood for you, so you can get a read out of
how much confidence you can put into the Z score.

``` r

# Compare the current case mix with the MTOS case mix
trauma_case_mix(data, Ps_col = Ps, outcome_col = death)
#>      Ps_range current_fraction MTOS_distribution
#> 1 0.00 - 0.25           0.0209             0.010
#> 2 0.26 - 0.50           0.0742             0.043
#> 3 0.51 - 0.75           0.1907             0.000
#> 4 0.76 - 0.90           0.3000             0.052
#> 5 0.91 - 0.95           0.1979             0.053
#> 6 0.96 - 1.00           0.2163             0.842
```

## The Relative Mortality Metric

[Napoli et
al. (2017)](https://www.tandfonline.com/doi/abs/10.1080/24725579.2017.1325948)
published methods for calculating a measure of trauma center (or system)
performance while overcoming a problem with the W-Score and the TRISS
methodology. Given that the majority of patients seen at trauma centers
will have a probability of survival over 90%, estimating performance
based on the W-Score may only indicate how well a center performed with
lower acuity patients. Using Napoli et al. (2017), it is possible to
calculate a score that is similar to the W-Score in its
interpretability, but deals with the negatively skewed probability of
survival problem by creating non-linear bins of score ranges, and then
weighting a score based on the nature of those bins. The Relative
Mortality Metric (RMM) has a scale from -1 to 1, where

- An RMM of 0 indicates that the observed mortality aligns with the
  expected national benchmark across all acuity levels.
- An RMM greater than 0 indicates better-than-expected performance,
  where the center is outperforming the national benchmark.
- An RMM less than 0 indicates under-performance, where the center’s
  observed mortality is higher than the expected benchmark.

## Non-Linear Binning Algorithm

An important part of the approach Napoli et al. (2017) took was to
modify the M-Score approach of looking at linear bins of the probability
of survival distribution, and make it non-linear. The {traumar} package
does this for you using Dr. Napoli’s method:

``` r

# Apply the nonlinear_bins function
results <- nonlinear_bins(data = data,
                         Ps_col = Ps,
                         outcome_col = survival,
                         divisor1 = 4,
                         divisor2 = 4,
                         threshold_1 = 0.9,
                         threshold_2 = 0.99)

# View intervals created by the algorithm
results$intervals
#>  [1] 0.02257717 0.59018811 0.75332640 0.84397730 0.90005763 0.93040607
#>  [7] 0.95446838 0.97345230 0.99000626 0.99957866

# View the bin statistics
results$bin_stats
#> # A tibble: 9 × 9
#>   bin_number bin_start bin_end  mean      sd alive  dead count percent
#>        <int>     <dbl>   <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>
#> 1          1    0.0226   0.590 0.416 0.133     614   775  1389   0.139
#> 2          2    0.590    0.753 0.681 0.0480    953   436  1389   0.139
#> 3          3    0.753    0.844 0.803 0.0261   1108   281  1389   0.139
#> 4          4    0.844    0.900 0.873 0.0162   1208   179  1387   0.139
#> 5          5    0.900    0.930 0.916 0.00879   911    95  1006   0.101
#> 6          6    0.930    0.954 0.943 0.00699   954    52  1006   0.101
#> 7          7    0.954    0.973 0.964 0.00545   979    27  1006   0.101
#> 8          8    0.973    0.990 0.981 0.00485   989    17  1006   0.101
#> 9          9    0.990    1.00  0.994 0.00253   421     1   422   0.042
```

## The RMM function

The RMM is helpful in that it will be sensitive to higher acuity
patients as well, so if a trauma center is struggling to effectively
intervene with higher acuity patients, this will be reflected in the
RMM. Conversely, with the W-Score, trauma center performance may not
appear to decline due to the influence of the lower acuity patients
using the MTOS Distribution. {traumar} automates the calculation of the
RMM as a single score, and by the non-linear binning process recommended
by Napoli et al. (2017). The `rmm()` and `rm_bin_summary()` functions
call `nonlinear_bins()` under the hood to produce the statistics you see
below in the example. Notice that a bootstrap sample is taken to
estimate a simulated distribution of RMM scores to calculate 95%
confidence intervals, while also calculating the population RMM
estimate:

``` r

# Example usage of the `rmm()` function
rmm(data = data,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 250,
    Divisor1 = 4,
    Divisor2 = 4
    )
#> # A tibble: 1 × 6
#>   population_RMM lower_ci bootstrap_RMM upper_ci sd_RMM  se_RMM
#>            <dbl>    <dbl>         <dbl>    <dbl>  <dbl>   <dbl>
#> 1          0.166    0.159         0.161    0.163 0.0181 0.00114

# Pivoting can be helpful at times
rmm(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  n_samples = 250,
  Divisor1 = 4,
  Divisor2 = 4,
  pivot = TRUE
)
#> # A tibble: 6 × 2
#>   stat             value
#>   <chr>            <dbl>
#> 1 population_RMM 0.166  
#> 2 lower_ci       0.159  
#> 3 bootstrap_RMM  0.161  
#> 4 upper_ci       0.163  
#> 5 sd_RMM         0.0181 
#> 6 se_RMM         0.00114

# RMM calculated by non-linear bin range
# `rm_bin_summary()` function
rm_bin_summary(data = data,
               Ps_col = Ps,
               outcome_col = survival,
               Divisor1 = 4,
               Divisor2 = 4,
               n_samples = 250
               )
```
