# Exploratory Data Analysis, Normality Testing, and Visualization

**\[experimental\]**

`is_it_normal()` calculates descriptive statistics and conducts
univariate normality testing on one or more numeric variables in a
dataset using a selected statistical test. Optional plots are included
for one variable at a time, only. Results are returned as a named list
containing summaries and, optionally, normality tests and/or diagnostic
plots.

## Usage

``` r
is_it_normal(
  df,
  ...,
  group_vars = NULL,
  seed = 10232015,
  normality_test = NULL,
  include_plots = FALSE,
  plot_theme = traumar::theme_cleaner
)
```

## Arguments

- df:

  A `data.frame` or `tibble` containing the variables to assess.

- ...:

  One or more unquoted column names from `df` to be analyzed.

- group_vars:

  Optional. A character vector of column names in `df` to group results
  by (e.g., `c("year", "hospital_level")`). If `NULL`, no grouping is
  applied. Grouped summaries and normality tests are computed within
  each unique combination of values across these variables.

- seed:

  A numeric value passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) to ensure
  reproducibility. Default is `10232015`.

- normality_test:

  A character string specifying the statistical test to use. Must be one
  of: `"shapiro-wilk" or "shapiro" or "sw"`,
  `"kolmogorov-smirnov" or "ks"`, `"anderson-darling" or "ad"`,
  `"lilliefors" or "lilli"`, `"cramer-von-mises" or "cvm"`,
  `"pearson" or "p"`, or `"shapiro-francia" or "sf"`. If `NULL`, no
  normality test is performed, which is the default.

- include_plots:

  Logical. If `TRUE`, plots are generated for a single variable.
  Plotting is disabled if multiple variables are passed.

- plot_theme:

  A
  [`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html)
  function to apply to all plots. Default is
  [`traumar::theme_cleaner`](https://bemts-hhs.github.io/traumar/reference/theme_cleaner.md).

## Value

A named list with the following elements:

- descriptive_statistics:

  A `tibble` of summary statistics for each variable.

- normality_test:

  A `tibble` of test statistics and p-values (if
  `normality_test == TRUE`).

- plots:

  A patchwork object containing four plots (if `include_plots = TRUE`
  and one variable supplied).

## Details

- If the data do not meet the test requirements for a chosen test of
  normality, `is_it_normal()` will not run the tests.

- Normality tests may yield differing results. Each test has distinct
  assumptions and sensitivity. Users should verify assumptions and
  consult test-specific guidance to ensure appropriate use.

- The function will abort with helpful CLI messages if input types or
  structures are incorrect.

- If plotting is enabled, and `nrow(df) > 10000`, a warning is issued as
  plotting may become computationally expensive.

## Note

Supported normality tests are below. Please check the specifications of
these tests in the corresponding documentation.

- Shapiro-Wilk
  ([`stats::shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html))

- Kolmogorov-Smirnov
  ([`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html))

- Anderson-Darling
  ([`nortest::ad.test()`](https://rdrr.io/pkg/nortest/man/ad.test.html))

- Lilliefors
  ([`nortest::lillie.test()`](https://rdrr.io/pkg/nortest/man/lillie.test.html))

- Cramer-von Mises
  ([`nortest::cvm.test()`](https://rdrr.io/pkg/nortest/man/cvm.test.html))

- Pearson (`norest::pearson.test()`)

- Shapiro-Francia
  ([`nortest::sf.test()`](https://rdrr.io/pkg/nortest/man/sf.test.html))

Please note that if grouped plotting is enabled, each group will
generate its own set of plots. This may flood your IDE or console. Plan
your use of this functionality with care to avoid lags or unwanted
outputs.

## Author

Nicolas Foss, Ed.D., MS
