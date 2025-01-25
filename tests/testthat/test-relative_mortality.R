# test the rmm function
testthat::test_that("rmm function validates inputs correctly", {
  # Test if data is a data frame
  testthat::expect_error(rmm(data = NULL, Ps_col = Ps, outcome_col = survival))

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))
  testthat::expect_error(rmm(data = df, outcome_col = survival, n_samples = 100))

  # Test missing outcome_col
  testthat::expect_error(rmm(data = df, Ps_col = Ps))

  # Test non-binary outcome_col
  df_non_binary <- tibble::tibble(Ps = plogis(rnorm(5, mean = 2, sd = 1.5)), survival = c(1, 2, 3, 4, 5))
  testthat::expect_error(rmm(data = df_non_binary, Ps_col = Ps, outcome_col = survival, n_samples = 100))

  # Test non-numeric Ps_col
  df_non_numeric <- tibble::tibble(Ps = c("a", "b", "c"), survival = c(1, 0, 1))
  testthat::expect_error(rmm(data = df_non_numeric, Ps_col = Ps, outcome_col = survival, n_samples = 100))

  # Test Ps values > 1
  df_ps_above_1 <- tibble::tibble(Ps = c(150, 80, 30), survival = c(1, 0, 1))
  testthat::expect_error(rmm(data = df_ps_above_1, Ps_col = Ps, outcome_col = survival, n_samples = 100))

  #Test incorrect input to n_samples
  testthat::expect_error(rmm(data = data.frame(Ps = c(0.1, 0.5, 0.9, .005), survival = c(1, 0, 1, 0)),
                   Ps_col = Ps, outcome_col = survival,
                   n_samples = "1000")
               )
})

testthat::test_that("rmm function computes binning correctly", {

  set.seed(01232025)

  # Test the binning process with some mock data
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  # Check if binning works as expected
  result <- rmm(data = df, Ps_col = Ps, outcome_col = survival, Divisor1 = 5, Divisor2 = 5, n_samples = 100)
  testthat::expect_true(all(c("population_RMM", "lower_ci", "bootstrap_RMM", "upper_ci", "sd_RMM", "se_RMM") %in% colnames(result)))

  # Test that RMM is calculated
  testthat::expect_true(all(!is.na(result$population_RMM)))
  testthat::expect_true(all(!is.na(result$bootstrap_RMM)))
})

testthat::test_that("rmm function calculates RMM and its confidence intervals", {

  set.seed(01232025)

  # Test with mock data for RMM calculation
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  result <- rmm(data = df, Ps_col = Ps, outcome_col = survival, Divisor1 = 5, Divisor2 = 5, n_samples = 100)

  # Test for upper and lower bounds of RMM
  testthat::expect_true(all(result$lower_ci <= result$bootstrap_RMM))
  testthat::expect_true(all(result$upper_ci >= result$bootstrap_RMM))
})

testthat::test_that("rmm function handles the pivot argument correctly", {
  # Test with pivot = TRUE
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  result_pivot <- rmm(data = df, Ps_col = Ps, outcome_col = survival, pivot = TRUE, n_samples = 100)
  testthat::expect_true("stat" %in% colnames(result_pivot))
  testthat::expect_true("value" %in% colnames(result_pivot))

  # Test without pivot (default is FALSE)
  result_no_pivot <- rmm(data = df, Ps_col = Ps, outcome_col = survival, pivot = FALSE, n_samples = 100)
  testthat::expect_false("stat" %in% colnames(result_no_pivot))
  testthat::expect_false("value" %in% colnames(result_no_pivot))
})

testthat::test_that("rmm function handles edge cases correctly", {

  # Test with edge case: only one row
  df_one_row <- tibble::tibble(Ps = 0.50, survival = 1)
  testthat::expect_error(rmm(data = df_one_row, Ps_col = Ps, outcome_col = survival))

  # Test with all NA values in Ps
  df_na_ps <- tibble::tibble(Ps = c(NA, NA, NA), survival = c(1, 0, 1))
  testthat::expect_error(rmm(data = df_na_ps, Ps_col = Ps, outcome_col = survival))

  # Test with all missing outcome values
  df_na_outcome <- tibble::tibble(Ps = c(.20, .50, .80), survival = c(NA, NA, NA))
  testthat::expect_error(rmm(data = df_na_outcome, Ps_col = Ps, outcome_col = survival))

})

# test the rm_bin_summary function

testthat::test_that("rm_bin_summary validates inputs correctly", {
  # Test if data is a data frame
  testthat::expect_error(rm_bin_summary(data = NULL, Ps_col = Ps, outcome_col = survival))

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))
  testthat::expect_error(rm_bin_summary(data = df, outcome_col = survival))

  # Test missing outcome_col
  testthat::expect_error(rm_bin_summary(data = df, Ps_col = Ps))

  # Test non-binary outcome_col
  df_non_binary <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = sample(1:3, 1000, replace = TRUE))
  testthat::expect_error(rm_bin_summary(data = df_non_binary, Ps_col = Ps, outcome_col = survival))

  # Test non-numeric Ps_col
  df_non_numeric <- tibble::tibble(Ps = c("a", "b", "c"), survival = c(1, 0, 1))
  testthat::expect_error(rm_bin_summary(data = df_non_numeric, Ps_col = Ps, outcome_col = survival))

  # Test Ps values > 1
  df_ps_above_1 <- tibble::tibble(Ps = c(150, 80, 30), survival = c(1, 0, 1))
  testthat::expect_error(rm_bin_summary(data = df_ps_above_1, Ps_col = Ps, outcome_col = survival))
})
