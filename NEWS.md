# traumar 1.2.0

- This minor release introduces functionality to demonstrate how the Iowa trauma system currently engages in the quality improvement process using the System Evaluation and Quality Improvement Committee (SEQIC) Indicators.
_ Additionally, a convenience function `is_it_normal()` provides the ability for users of `traumar` to get descriptive statistics on one or more numeric variables, with optional normality tests, and diagnostic plots (for one variable only). Grouping functionality is also supported in `is_it_normal()` to conduct exploratory data analysis of one or more variables within zero or more groups.
- Added the following functions:
  - `seqic_indicator_1()`
  - `seqic_indicator_2()`
  - `seqic_indicator_3()`
  - `seqic_indicator_4()`
  - `seqic_indicator_5()`
  - `seqic_indicator_6()`
  - `seqic_indicator_7()`
  - `seqic_indicator_8()`
  - `seqic_indicator_9()`
  - `seqic_indicator_10()`
  - `seqic_indicator_11()`
  - `seqic_indicator_12()`
  - `seqic_indicator_13()`
  - `is_it_normal()`
- A fix was applied to `nonlinear_bins()` to make the `percent` column calculate correctly when groups were not introduced.
- Removed hard-coded rounding from most calculations within the package where possible.
- Improved examples for the the package's README, `probability_of_survival()`, `nonlinear_bins()`, `rmm()`, and `rm_bin_summary()` using more helpful data.
- Improved error messages coming from `nonlinear_bins()` when sample sizes too small to calculate bins are passed to the function, including when passed to `rmm()` and `rm_bin_summary()`.
- Code formatting changed to the `air` package through the RStudio IDE.
- Updated data validation for `trauma_case_mix()`, `trauma_performance()`, `nonlinear_bins()`, `rmm()`, and `rm_bin_summary()` to provide improved messaging related to missings in `Ps_col` and `outcome_col` .
- Across functions using the probability of survival calculation, it is expected that Ps values have a range of [0, 1].  Functions will no longer handle values in percentage format (e.g. 10, 50, 98).
- `trauma_performance()` will no longer print out a summary using the `diagnostics` argument, which is no longer used in this function.
- The `outcome` argument was removed from `trauma_performance()` to remove ambiguity in the nature of the `outcome_col` values. Only values of `TRUE/FALSE` and `1/0` are accepted.
- The `diagnostics` argument was removed from `trauma_performance()` to make the user interface smoother.  Instead of providing guidance via outputs the console, users are encouraged to seek assistance with interpreting results via the source academic literature and the package documentation.
- `trauma_performance()` will no longer provide a pivoted output as a default.  Users can elect to pivot the outputs as needed.
- `rmm()` and `rm_bin_summary()` now have a new argument `bootstrap_ci` that allows a user to elect to use the additional functionality, or not.  `bootstrap_ci` defaults to `TRUE`.

# traumar 1.1.0

## New Features

- Added optional grouping functionality to `nonlinear_bins()`, `rmm()`, and `rm_bin_summary()`.  
  - Setting `group_vars = NULL` applies the functions to the entire dataset without subgrouping.  
  - For pivoting the `rmm()` outputs longer, setting `pivot = TRUE` will work when `group_vars`
    is invoked by pivoting longer with the grouping context.

## Enhancements

- Improved `NA` handling in `rmm()` and `rm_bin_summary()`.  
- Ensured RMM calculations remain within the expected range of [-1 to 1], including their 95% confidence intervals.  
- Optimized `nonlinear_bins()` by replacing its internal `for` loop with `dplyr` functions, enhancing accuracy and efficiency without introducing breaking changes.
- Improved command line messaging and documentation within `rmm()` and `rm_bin_summary()` regarding probability of survival values `Ps_col < 0` and `Ps_col > 1`.  Now, these functions
  will throw an error if probability of survival values are `Ps_col < 0` or `Ps_col > 1`.
- The `nonlinear_bins()` function has improved data validation for the `Ps_col` variable.

---

# traumar 1.0.0

- Initial release to CRAN.  
- Achieved comprehensive test coverage (>90%).  

---

# traumar 0.0.1.9000

- Introduced `probability_of_survival()` function.  
- Expanded outputs for:  
  - `rmm()`  
  - `rm_bin_summary()`  
  - `nonlinear_bins()`  
- Updated existing tests and added new test cases.  
- Began test coverage improvements.  

---

# traumar 0.0.1

- Introduced core package functions:  
  - `trauma_case_mix()`  
  - `trauma_performance()`  
  - `rmm()`  
  - `rm_bin_summary()`  
  - `nonlinear_bins()`  
  - `impute()`  
  - `normalize()`  
  - `season()`  
  - `weekend()`  
  - `pretty_number()`  
  - `pretty_percent()`  
  - `small_count_label()`  
  - `stat_sig()`  
  - `theme_cleaner()`  
  - `%not_in%`  
- Established package framework and initialization.  
