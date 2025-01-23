
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/bemts-hhs/traumar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bemts-hhs/traumar/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/traumar)](https://CRAN.R-project.org/package=traumar)
[![Codecov test
coverage](https://codecov.io/gh/bemts-hhs/traumar/graph/badge.svg)](https://app.codecov.io/gh/bemts-hhs/traumar)
<!-- badges: end -->

# traumar <img src="man/figures/traumar_hex.png" align="right" width="200" style="background-color: transparent !important; border: none;" />

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
set.seed(123)  # For reproducibility
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

The RMM is helpful in that it will be sensitive to higher acuity
patients as well, so if a trauma center is struggling to effectively
intervene with higher acuity patients, this will be reflected in the
RMM. Conversely, with the W-Score, trauma center performance may not
appear to decline due to the influence of the lower acuity patients
using the MTOS Distribution. {traumar} automates the calculation of the
RMM as a single score, and by the non-linear binning process recommended
by Napoli et al. (2017).

``` r

# Example usage of the `rmm()` function
rmm(data = data,
    Ps_col = Ps,
    outcome_col = survival)
#> ✔ Data validation passed.
#>    numerator denominator        E_b     RMM_LL       RMM    RMM_UL
#> 1 0.06649169   0.4776776 0.07660905 0.06258879 0.1391978 0.2158069

# Pivoting can be helpful at times
rmm(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  pivot = TRUE
)
#> ✔ Data validation passed.
#> # A tibble: 6 × 2
#>   coefficient  value
#>   <chr>        <dbl>
#> 1 numerator   0.0665
#> 2 denominator 0.478 
#> 3 E_b         0.0766
#> 4 RMM_LL      0.0626
#> 5 RMM         0.139 
#> 6 RMM_UL      0.216

# RMM calculated by non-linear bin range
# `rm_bin_summary()` function
rm_bin_summary(data = data,
               Ps_col = Ps,
               outcome_col = survival)
#> ✔ Data validation passed.
#>    bin_number TA_b TD_b  N_b       EM_b  bin_start   bin_end  midpoint
#> 1           1  454  657 1111 0.59135914 0.02257717 0.5423470 0.2824621
#> 2           2  686  425 1111 0.38253825 0.54234698 0.7015426 0.6219448
#> 3           3  850  261 1111 0.23492349 0.70154257 0.7958116 0.7486771
#> 4           4  922  189 1111 0.17011701 0.79581165 0.8571453 0.8264785
#> 5           5  971  139 1110 0.12522523 0.85714527 0.9000576 0.8786014
#> 6           6  727   78  805 0.09689441 0.90005763 0.9251891 0.9126234
#> 7           7  752   53  805 0.06583851 0.92518915 0.9460383 0.9356137
#> 8           8  779   26  805 0.03229814 0.94603830 0.9626674 0.9543529
#> 9           9  783   22  805 0.02732919 0.96266743 0.9762396 0.9694535
#> 10         10 1213   13 1226 0.01060359 0.97623957 0.9995787 0.9879091
#>           R_b    AntiM_b         E_b     numerator  denominator       RMM_LL
#> 1  0.51976981 0.71753793 0.026472901  6.558393e-02 0.3729545526  0.149376746
#> 2  0.15919559 0.37805523 0.028513634 -7.136780e-04 0.0601847250 -0.040371760
#> 3  0.09426908 0.25132289 0.025507174  1.545956e-03 0.0236919780  0.039745137
#> 4  0.06133362 0.17352154 0.022268528  2.088121e-04 0.0106427043 -0.002648313
#> 5  0.04291236 0.12139855 0.019213093 -1.642116e-04 0.0052094984 -0.050734668
#> 6  0.02513152 0.08737661 0.019507496 -2.391967e-04 0.0021959070 -0.128435935
#> 7  0.02084916 0.06438627 0.016955206 -3.027788e-05 0.0013423995 -0.039510248
#> 8  0.01662913 0.04564713 0.014418476  2.219822e-04 0.0007590721  0.278020373
#> 9  0.01357214 0.03054650 0.011887823  4.366569e-05 0.0004145813  0.093436964
#> 10 0.02333909 0.01209088 0.006117843  3.471206e-05 0.0002821901  0.116891645
#>            RMM       RMM_UL
#> 1   0.17584965  0.202322549
#> 2  -0.01185813  0.016655509
#> 3   0.06525231  0.090759485
#> 4   0.01962021  0.041888743
#> 5  -0.03152157 -0.012308481
#> 6  -0.10892844 -0.089420943
#> 7  -0.02255504 -0.005599836
#> 8   0.29243885  0.306857325
#> 9   0.10532479  0.117212609
#> 10  0.12300949  0.129127331
```

## There is More!

{traumar} has other functions to help you in your data analysis journey!
Check out the additional package documentation at
[bemts-hhs/github.io/trauma](https://bemts-hhs/github.io/trauma) where
you can find examples of each function the package has to offer.
