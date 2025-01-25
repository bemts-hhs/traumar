# Tests for nonlinear_bins function

test_that("nonlinear_bins handles basic functionality correctly", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Run the function
  result <- nonlinear_bins(data, Ps_col = Ps, outcome_col = survival, divisor1 = 5, divisor2 = 5, threshold_1 = 0.9, threshold_2 = 0.99)

  # Check intervals
  expect_type(result$intervals, "double")
  expect_true(length(result$intervals) > 1)

  # Check bin_stats structure
  expect_s3_class(result$bin_stats, "tbl_df")
  expect_named(result$bin_stats, c("bin_number", "bin_start", "bin_end", "mean", "sd", "alive", "dead", "count", "percent"))
})

test_that("nonlinear_bins produces reasonable bins for uniform data", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Run the function
  result <- nonlinear_bins(data, Ps_col = Ps, outcome_col = survival)

  # Check intervals cover correct range
  expect_true(min(result$intervals) > 0)
  expect_true(max(result$intervals) < 1)

  # Check bin stats are populated
  expect_true(all(result$bin_stats$count > 0))
})

test_that("nonlinear_bins respects divisor and threshold parameters", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Run with different divisors and thresholds
  result1 <- nonlinear_bins(data, Ps_col = Ps, outcome_col = survival, divisor1 = 10, divisor2 = 10)
  result2 <- nonlinear_bins(data, Ps_col = Ps, outcome_col = survival, divisor1 = 2, divisor2 = 2)

  # Check intervals differ based on divisors
  expect_true(length(result1$intervals) > length(result2$intervals))

  # Check thresholds affect intervals
  result3 <- nonlinear_bins(data, Ps_col = Ps, outcome_col = survival, threshold_1 = 0.8, threshold_2 = 0.95)
  expect_true(length(result3$intervals) < length(result1$intervals))

})

test_that("nonlinear_bins handles invalid inputs gracefully", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Missing column
  expect_error(nonlinear_bins(data, Ps_col = NotAColumn, outcome_col = survival))

  # Non-numeric column, warning will happen
  data <- data.frame(Ps = c("d", "e", "f"), survival = c("a", "b", "c"))
  expect_error(nonlinear_bins(data, Ps_col = Ps, outcome_col = survival))

})

test_that("nonlinear_bins produces accurate bin statistics", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Run the function
  result <- nonlinear_bins(data, Ps_col = Ps, outcome_col = survival, divisor1 = 5, divisor2 = 5)

  # Validate statistics
  for (i in seq_len(nrow(result$bin_stats))) {
    bin_data <- Ps[Ps >= result$bin_stats$bin_start[i] & (Ps < result$bin_stats$bin_end[i] | i == nrow(result$bin_stats))]
    if (length(bin_data) > 0) {
      expect_equal(result$bin_stats$mean[i], mean(bin_data))
      expect_equal(result$bin_stats$sd[i], sd(bin_data))
      expect_equal(result$bin_stats$count[i], length(bin_data))
    }
  }
})
