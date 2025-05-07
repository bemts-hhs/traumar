# traumar 1.2.0

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
