# Changelog

## traumar (development version)

## traumar 1.2.3

CRAN release: 2026-01-08

- [`probability_of_survival()`](https://bemts-hhs.github.io/traumar/reference/probability_of_survival.md)
  was updated to enhance code readability and leverage mathematical
  notation in the calculation of predicted survival probabilities. The
  function now aligns with the coefficients published in Norouzi et
  al. (2013) and Merchant et al. (2023). Consistent with Boyd et al.
  (1987), the function does not treat patients under 15 years of age
  differently and accounts for penetrating injuries similarly to other
  age groups. This update ensures a standardized approach to calculating
  survival probabilities for both blunt and penetrating traumas.

- From this release forward, all development of `traumar` will be done
  in the IDE Positron!

## traumar 1.2.2

CRAN release: 2025-08-26

- [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md),
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md),
  and
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md):
  Updated the documentation for the threshold arguments along with the
  divisor arguments. These were updated along with the Details section
  of the
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md)
  function documentation to provide a better explanation of the binning
  algorithm under the hood.

- [`probability_of_survival()`](https://bemts-hhs.github.io/traumar/reference/probability_of_survival.md):
  Updated the Return section documentation to be more accurate that the
  output is not a percentage, it is the predicted probability of
  survival on a scale from 0 to 1. A previous version of this function
  was multiplied by 100 to seem more like a percentage.

## traumar 1.2.1

CRAN release: 2025-06-24

- Within the
  [`trauma_performance()`](https://bemts-hhs.github.io/traumar/reference/trauma_performance.md)
  function, renamed the variable `predicted_prob_death` to
  `scale_factor` which is commensurate with the source literature.

- updated comments in
  [`trauma_performance()`](https://bemts-hhs.github.io/traumar/reference/trauma_performance.md)
  for `z_method` method of the `Z_score` to reflect the right text.

- In
  [`trauma_performance()`](https://bemts-hhs.github.io/traumar/reference/trauma_performance.md),
  completed the comment where the `scale_factor` is created so that it
  is complete and clear.

- Corrected a test error at CRAN from using bootstrap CI process in
  testing with 100,000 observations and 100 bootstrap samples to make
  sure [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md)
  and
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)
  ran in under 60 sec. That test now does not use the bootstrap process
  so the core function can be tested and will always run in under a
  minute with 100,000 observations.

- Cleaned up other tests within for relative_mortality.R that were
  checking for correct error / warning handling where multiple lines of
  output were sent to the console. Built a custom function to deal with
  those scenarios and correctly perform those unit tests.

## traumar 1.2.0

CRAN release: 2025-05-22

- This minor release introduces functionality to demonstrate how the
  Iowa trauma system currently engages in the quality improvement
  process using the System Evaluation and Quality Improvement Committee
  (SEQIC) Indicators.

- Added a [Contributor Code of
  Conduct](https://bemts-hhs.github.io/traumar/CODE_OF_CONDUCT.html) and
  support information.

- Additionally, a convenience function
  [`is_it_normal()`](https://bemts-hhs.github.io/traumar/reference/is_it_normal.md)
  provides the ability for users of `traumar` to get descriptive
  statistics on one or more numeric variables, with optional normality
  tests, and diagnostic plots (for one variable only). Grouping
  functionality is also supported in
  [`is_it_normal()`](https://bemts-hhs.github.io/traumar/reference/is_it_normal.md)
  to conduct exploratory data analysis of one or more variables within
  zero or more groups.

- Added the following functions:

  - [`seqic_indicator_1()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_1.md)
  - [`seqic_indicator_2()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_2.md)
  - [`seqic_indicator_3()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_3.md)
  - [`seqic_indicator_4()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_4.md)
  - [`seqic_indicator_5()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_5.md)
  - [`seqic_indicator_6()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_6.md)
  - [`seqic_indicator_7()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_7.md)
  - [`seqic_indicator_8()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_8.md)
  - [`seqic_indicator_9()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_9.md)
  - [`seqic_indicator_10()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_10.md)
  - [`seqic_indicator_11()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_11.md)
  - [`seqic_indicator_12()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_12.md)
  - [`seqic_indicator_13()`](https://bemts-hhs.github.io/traumar/reference/seqic_indicator_13.md)
  - [`is_it_normal()`](https://bemts-hhs.github.io/traumar/reference/is_it_normal.md)

- A fix was applied to
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md)
  to make the `percent` column calculate correctly when groups were not
  introduced.

- Removed hard-coded rounding from most calculations within the package
  where possible.

- Improved examples for the the package’s README,
  [`probability_of_survival()`](https://bemts-hhs.github.io/traumar/reference/probability_of_survival.md),
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md),
  [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md), and
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)
  using more helpful data.

- Improved error messages coming from
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md)
  when the argument `Ps_col` does not follow the expected distribution
  of the calculated probability of survival, and/or a sample size too
  small to calculate bins is passed to the function, including when
  passed to
  [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md) and
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md).

- Code formatting changed to the `air` package through the RStudio IDE.

- Updated data validation for
  [`trauma_case_mix()`](https://bemts-hhs.github.io/traumar/reference/trauma_case_mix.md),
  [`trauma_performance()`](https://bemts-hhs.github.io/traumar/reference/trauma_performance.md),
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md),
  [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md), and
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)
  to provide improved messaging related to missings in `Ps_col` and
  `outcome_col` .

- Across functions using the probability of survival calculation, it is
  expected that Ps values have a range of \[0, 1\]. Functions will no
  longer handle values in percentage format (e.g. 10, 50, 98).

- The `outcome` argument was removed from
  [`trauma_performance()`](https://bemts-hhs.github.io/traumar/reference/trauma_performance.md)
  to remove ambiguity in the nature of the `outcome_col` values. Only
  values of `TRUE/FALSE` and `1/0` are accepted.

- The `diagnostics` argument was removed from
  [`trauma_performance()`](https://bemts-hhs.github.io/traumar/reference/trauma_performance.md)
  to make the user interface smoother. Instead of providing guidance via
  outputs to the console, users are encouraged to seek assistance with
  interpreting results via the source academic literature and the
  package documentation.

- [`trauma_performance()`](https://bemts-hhs.github.io/traumar/reference/trauma_performance.md)
  will no longer provide a pivoted output as a default. Users can elect
  to pivot the outputs as needed in their workflows.

- [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md) and
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)
  now have a new argument `bootstrap_ci` that allows a user to elect to
  use the bootstrap CIs, or not. `bootstrap_ci` defaults to `TRUE` in
  order to better support backward compatibility.

## traumar 1.1.0

CRAN release: 2025-03-25

### New Features

- Added optional grouping functionality to
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md),
  [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md), and
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md).

  - Setting `group_vars = NULL` applies the functions to the entire
    dataset without subgrouping.

  - For pivoting the
    [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md)
    outputs longer, setting `pivot = TRUE` will work when `group_vars`
    is invoked by pivoting longer with the grouping context.

### Enhancements

- Improved `NA` handling in
  [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md) and
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md).

- Ensured RMM calculations remain within the expected range of \[-1 to
  1\], including their 95% confidence intervals.

- Optimized
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md)
  by replacing its internal `for` loop with `dplyr` functions, enhancing
  accuracy and efficiency without introducing breaking changes.

- Improved command line messaging and documentation within
  [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md) and
  [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)
  regarding probability of survival values `Ps_col < 0` and
  `Ps_col > 1`. Now, these functions will throw an error if probability
  of survival values are `Ps_col < 0` or `Ps_col > 1`.

- The
  [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md)
  function has improved data validation for the `Ps_col` variable.

------------------------------------------------------------------------

## traumar 1.0.0

CRAN release: 2025-02-21

- Initial release to CRAN.  
- Achieved comprehensive test coverage (\>90%).

------------------------------------------------------------------------

## traumar 0.0.1.9000

- Introduced
  [`probability_of_survival()`](https://bemts-hhs.github.io/traumar/reference/probability_of_survival.md)
  function.  
- Expanded outputs for:
  - [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md)  
  - [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)  
  - [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md)  
- Updated existing tests and added new test cases.  
- Began test coverage improvements.

------------------------------------------------------------------------

## traumar 0.0.1

- Introduced core package functions:
  - [`trauma_case_mix()`](https://bemts-hhs.github.io/traumar/reference/trauma_case_mix.md)  
  - [`trauma_performance()`](https://bemts-hhs.github.io/traumar/reference/trauma_performance.md)  
  - [`rmm()`](https://bemts-hhs.github.io/traumar/reference/rmm.md)  
  - [`rm_bin_summary()`](https://bemts-hhs.github.io/traumar/reference/rm_bin_summary.md)  
  - [`nonlinear_bins()`](https://bemts-hhs.github.io/traumar/reference/nonlinear_bins.md)  
  - [`impute()`](https://bemts-hhs.github.io/traumar/reference/impute.md)  
  - [`normalize()`](https://bemts-hhs.github.io/traumar/reference/normalize.md)  
  - [`season()`](https://bemts-hhs.github.io/traumar/reference/season.md)  
  - [`weekend()`](https://bemts-hhs.github.io/traumar/reference/weekend.md)  
  - [`pretty_number()`](https://bemts-hhs.github.io/traumar/reference/pretty_number.md)  
  - [`pretty_percent()`](https://bemts-hhs.github.io/traumar/reference/pretty_percent.md)  
  - [`small_count_label()`](https://bemts-hhs.github.io/traumar/reference/small_count_label.md)  
  - [`stat_sig()`](https://bemts-hhs.github.io/traumar/reference/stat_sig.md)  
  - [`theme_cleaner()`](https://bemts-hhs.github.io/traumar/reference/theme_cleaner.md)  
  - `%not_in%`  
- Established package framework and initialization.
