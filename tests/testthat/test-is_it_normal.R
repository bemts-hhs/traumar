###_____________________________________________________________________________
### Data Validation Testing
###_____________________________________________________________________________

testthat::test_that("is_it_normal() input validation works correctly", {
  # Create minimal test data
  test_data <- data.frame(
    var1 = rnorm(100),
    group1 = rep(c("A", "B"), each = 50),
    group2 = rep(c("X", "Y"), 50)
  )

  # df must be data.frame or tibble
  testthat::expect_error(
    traumar::is_it_normal(df = "not_a_df", var1),
    class = "rlang_error"
  )

  # group_vars must be character vector or NULL
  testthat::expect_error(
    traumar::is_it_normal(df = test_data, var1, group_vars = 123),
    class = "rlang_error"
  )

  # group_vars must exist in df
  testthat::expect_error(
    traumar::is_it_normal(df = test_data, var1, group_vars = "nonexistent_col"),
    class = "rlang_error"
  )

  # Invalid normality_test should error
  testthat::expect_error(
    traumar::is_it_normal(
      df = test_data,
      var1,
      normality_test = "invalid_test"
    ),
    class = "rlang_error"
  )

  # plot_theme must be a function if include_plots is TRUE
  testthat::expect_error(
    traumar::is_it_normal(
      df = test_data,
      var1,
      include_plots = TRUE,
      plot_theme = "not_a_function"
    ),
    class = "rlang_error"
  )

  # seed must be numeric
  testthat::expect_error(
    traumar::is_it_normal(df = test_data, var1, seed = "not_numeric"),
    class = "rlang_error"
  )

  # Should succeed with valid input
  testthat::expect_no_error(
    traumar::is_it_normal(
      df = test_data,
      var1,
      group_vars = c("group1", "group2"),
      normality_test = "sw"
    )
  )
})

###_____________________________________________________________________________
### Calculation testing
###_____________________________________________________________________________

# Test 1: Ensure an error is raised if plot_theme is not a function when include_plots = TRUE
testthat::test_that("plot_theme must be a function if include_plots is TRUE", {
  # Create minimal test data
  test_data <- data.frame(
    var1 = rnorm(100),
    group1 = rep(c("A", "B"), each = 50),
    group2 = rep(c("X", "Y"), 50)
  )

  testthat::expect_error(
    traumar::is_it_normal(
      df = test_data,
      var1,
      include_plots = TRUE,
      plot_theme = "not_a_function",
      normality_test = NULL
    ),
    "not of the correct class",
  )
})

# Test 2: Ensure no error if plot_theme is a valid function (e.g., theme from ggplot2)
testthat::test_that("plot_theme can be a valid function if include_plots is TRUE", {
  # Create minimal test data
  test_data <- data.frame(
    var1 = rnorm(100),
    group1 = rep(c("A", "B"), each = 50),
    group2 = rep(c("X", "Y"), 50)
  )

  testthat::expect_no_error(
    traumar::is_it_normal(
      df = test_data,
      var1,
      include_plots = TRUE,
      plot_theme = ggplot2::theme_bw,
      normality_test = NULL
    )
  )
})

# Test 3: Ensure seed is numeric
testthat::test_that("seed must be numeric", {
  # Create minimal test data
  test_data <- data.frame(
    var1 = rnorm(100),
    group1 = rep(c("A", "B"), each = 50),
    group2 = rep(c("X", "Y"), 50)
  )

  testthat::expect_error(
    traumar::is_it_normal(
      df = test_data,
      var1,
      seed = "not_a_number",
      normality_test = NULL
    ),
    regexp = "seed.*must be.*numeric"
  )

  testthat::expect_no_error(
    traumar::is_it_normal(
      df = test_data,
      var1,
      seed = 123,
      normality_test = NULL
    )
  )
})

# Test 4: Ensure correct handling of grouping behavior
testthat::test_that("grouping behavior is handled correctly", {
  # Create minimal test data
  test_data <- data.frame(
    var1 = rnorm(100),
    group1 = rep(c("A", "B"), each = 50),
    group2 = rep(c("X", "Y"), 50)
  )

  # Test with no grouping
  testthat::expect_message(
    traumar::is_it_normal(
      df = test_data,
      var1,
      normality_test = NULL
    ),
    "No grouping applied"
  )

  # Test with grouping
  testthat::expect_message(
    traumar::is_it_normal(
      df = test_data,
      var1,
      group_vars = "group1",
      normality_test = NULL
    ),
    "Grouping by.*group1"
  )
})

# Test 5: Ensure that proper warning is shown for normality test size violations
testthat::test_that("warning is shown for normality test size violations", {
  # Create minimal test data
  test_data <- data.frame(
    var1 = rnorm(5100),
    group1 = rep(c("A", "B"), each = 50),
    group2 = rep(c("X", "Y"), 50)
  )

  testthat::expect_message(
    traumar::is_it_normal(
      df = test_data,
      var1,
      include_plots = TRUE,
      plot_theme = ggplot2::theme_bw,
      normality_test = "shapiro-wilk"
    ),
    "One or more variables do not meet the sample size requirements"
  )
})

# Test 6: Ensure that info is displayed if no normality test is requested
testthat::test_that("info is displayed if no normality test is requested", {
  # Create minimal test data
  test_data <- data.frame(
    var1 = rnorm(100),
    group1 = rep(c("A", "B"), each = 50),
    group2 = rep(c("X", "Y"), 50)
  )

  testthat::expect_message(
    traumar::is_it_normal(
      df = test_data,
      var1,
      normality_test = NULL,
      include_plots = FALSE
    ),
    "No normality test requested"
  )
})

testthat::test_that("is_it_normal() calculates descriptive stats correctly", {
  df <- tibble::tibble(
    group = rep(c("A", "B"), each = 5),
    x = c(1, 2, 3, 4, 5, 6, NA, 8, 9, 10)
  )

  result <- traumar::is_it_normal(
    df,
    x,
    group_vars = "group"
  )

  stats <- result$descriptive_statistics

  testthat::expect_s3_class(stats, "tbl_df")
  testthat::expect_true(all(c("mean", "std_dev", "n_obs") %in% colnames(stats)))
  testthat::expect_equal(unique(stats$variable), "x")
  testthat::expect_equal(nrow(stats), 2) # 2 groups
})

testthat::test_that("is_it_normal() runs all supported normality tests without error", {
  df <- tibble::tibble(
    g = rep(letters[1:2], each = 50),
    y = c(rnorm(50), rnorm(50))
  )

  test_options <- c(
    "shapiro-wilk",
    "shapiro",
    "sw",
    "kolmogorov-smirnov",
    "ks",
    "anderson-darling",
    "ad",
    "lilliefors",
    "lilli",
    "cramer-von mises",
    "cvm",
    "pearson",
    "p",
    "shapiro-francia",
    "sf"
  )

  for (test_name in test_options) {
    result <- traumar::is_it_normal(
      df,
      y,
      group_vars = "g",
      normality_test = test_name
    )

    nt <- result$normality_test

    testthat::expect_s3_class(nt, "tbl_df")
    testthat::expect_true(
      all(
        c("variable", "test", "statistic", "p_value", "result") %in%
          colnames(nt)
      )
    )
    testthat::expect_equal(
      unique(nt$variable),
      "y"
    )
    testthat::expect_equal(nrow(nt), 2)
  }
})


testthat::test_that("is_it_normal() gracefully skips test on NA-only data", {
  df <- tibble::tibble(
    z = rep(NA_real_, 10)
  )

  result <- suppressWarnings(
    traumar::is_it_normal(
      df,
      z,
      normality_test = "shapiro-wilk"
    )
  )

  nt <- result$normality_test

  testthat::expect_s3_class(nt, "tbl_df")
  testthat::expect_true("Test Skipped" %in% nt$result)
  testthat::expect_true(is.na(nt$statistic))
})

testthat::test_that("is_it_normal() returns NULL for normality_test when not specified", {
  df <- tibble::tibble(a = rnorm(10))

  result <- traumar::is_it_normal(df, a)

  testthat::expect_null(result$normality_test)
})

testthat::test_that("Grouped plotting works with user confirmation", {
  df <- dplyr::tibble(
    group = rep(c("A", "B"), each = 100),
    value = c(
      stats::rnorm(100, mean = 5),
      stats::rnorm(100, mean = 10)
    )
  )

  result <- traumar::is_it_normal(
    df = df,
    value,
    group_vars = "group",
    include_plots = TRUE
  )

  testthat::expect_true("plots" %in% names(result))
  testthat::expect_s3_class(result$plots[[1]], "patchwork")
  testthat::expect_length(result$plots, 2)

  testthat::expect_message(
    traumar::is_it_normal(
      df = df,
      value,
      group_vars = "group",
      include_plots = TRUE
    ),
    "Grouped plotting is enabled"
  )
})

testthat::test_that("Grouped plotting is skipped when user passes more than one variable", {
  df <- dplyr::tibble(
    group = rep(c("A", "B"), each = 100),
    value1 = stats::rnorm(200),
    value2 = stats::rnorm(200, mean = 10, sd = 5)
  )

  result <- traumar::is_it_normal(
    df = df,
    value1,
    value2,
    group_vars = "group",
    include_plots = TRUE
  )

  testthat::expect_null(result$plots)
})

testthat::test_that("Large data warning is issued for plotting", {
  df <- dplyr::tibble(
    group = rep("A", 10001),
    value = stats::rnorm(10001)
  )

  testthat::expect_message(
    traumar::is_it_normal(
      df = df,
      value,
      group_vars = "group",
      include_plots = TRUE
    ),
    "Large Data Warning"
  )

  testthat::expect_message(
    traumar::is_it_normal(
      df = df,
      value,
      group_vars = "group",
      include_plots = TRUE
    ),
    "Consider downsampling"
  )
})

testthat::test_that("Grouped plotting: plots generated, warnings issued", {
  # Synthetic grouped dataset
  set.seed(123)
  df <- dplyr::tibble(
    group = rep(c("A", "B"), each = 60),
    x = c(rnorm(60, mean = 10), rnorm(60, mean = 20))
  )

  # Capture CLI output
  result <- traumar::is_it_normal(
    df = df,
    x,
    group_vars = "group",
    include_plots = TRUE
  )

  # Basic structure checks
  testthat::expect_true("plots" %in% names(result))
  testthat::expect_identical(class(result$plots), "list")
  testthat::expect_length(result$plots, 2) # Two groups: A and B

  # Check that each plot object is a patchwork
  purrr::walk(result$plots, ~ testthat::expect_s3_class(.x, "patchwork"))

  # CLI warnings/messages for grouped plotting
  testthat::expect_message(
    traumar::is_it_normal(
      df = df,
      x,
      group_vars = "group",
      include_plots = TRUE
    ),
    "Grouped plotting is enabled"
  )

  testthat::expect_message(
    traumar::is_it_normal(
      df = df,
      x,
      group_vars = "group",
      include_plots = TRUE
    ),
    "assign the function result to an object"
  )
})

testthat::test_that("Large data warning issued for nrow > 10000", {
  df_large <- dplyr::tibble(
    group = rep(c("A", "B"), each = 6000),
    x = rnorm(12000)
  )

  testthat::expect_message(
    traumar::is_it_normal(
      df = df_large,
      x,
      group_vars = "group",
      include_plots = TRUE
    ),
    "Large Data Warning"
  )

  testthat::expect_message(
    traumar::is_it_normal(
      df = df_large,
      x,
      group_vars = "group",
      include_plots = TRUE
    ),
    "Consider downsampling your data"
  )
})
