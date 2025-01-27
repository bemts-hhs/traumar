# test the rmm function
testthat::test_that("rmm function validates inputs correctly", {
  # Test if data is a data frame
  testthat::expect_error(rmm(data = NULL, Ps_col = Ps, outcome_col = survival))

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))
  testthat::expect_error(rmm(data = df, outcome_col = survival, n_samples = 5))

  # Test missing outcome_col
  testthat::expect_error(rmm(data = df, Ps_col = Ps))

  # Test non-binary outcome_col
  df_non_binary <- tibble::tibble(Ps = plogis(rnorm(5, mean = 2, sd = 1.5)), survival = c(1, 2, 3, 4, 5))
  testthat::expect_error(rmm(data = df_non_binary, Ps_col = Ps, outcome_col = survival, n_samples = 5))

  # Test non-numeric Ps_col
  df_non_numeric <- tibble::tibble(Ps = c("a", "b", "c"), survival = c(1, 0, 1))
  testthat::expect_error(rmm(data = df_non_numeric, Ps_col = Ps, outcome_col = survival, n_samples = 5))

  # Test Ps values > 1
  df_ps_above_1 <- tibble::tibble(Ps = c(150, 80, 30), survival = c(1, 0, 1))
  testthat::expect_error(rmm(data = df_ps_above_1, Ps_col = Ps, outcome_col = survival, n_samples = 5))

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
  result <- rmm(data = df, Ps_col = Ps, outcome_col = survival, Divisor1 = 5, Divisor2 = 5, n_samples = 5)
  testthat::expect_true(all(c("population_RMM_LL", "population_RMM", "population_RMM_UL", "population_CI", "bootstrap_RMM_LL", "bootstrap_RMM", "bootstrap_RMM_UL", "bootstrap_CI") %in% colnames(result)))

  # Test that RMM is calculated
  testthat::expect_true(all(!is.na(result$population_RMM)))
  testthat::expect_true(all(!is.na(result$bootstrap_RMM)))
})

testthat::test_that("rmm function calculates RMM and its confidence intervals", {

  set.seed(01232025)

  # Test with mock data for RMM calculation
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  result <- rmm(data = df, Ps_col = Ps, outcome_col = survival, Divisor1 = 5, Divisor2 = 5, n_samples = 5)

  # Test for upper and lower bounds of RMM
  testthat::expect_true(all(result$population_RMM_LL <= result$population_RMM))
  testthat::expect_true(all(result$population_RMM_UL >= result$population_RMM))
  testthat::expect_true(all(result$bootstrap_RMM_LL <= result$bootstrap_RMM))
  testthat::expect_true(all(result$bootstrap_RMM_UL >= result$bootstrap_RMM))
})

testthat::test_that("rmm function handles the pivot argument correctly", {
  # Test with pivot = TRUE
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  result_pivot <- rmm(data = df, Ps_col = Ps, outcome_col = survival, pivot = TRUE, n_samples = 5)
  testthat::expect_true("stat" %in% colnames(result_pivot))
  testthat::expect_true("value" %in% colnames(result_pivot))

  # Test without pivot (default is FALSE)
  result_no_pivot <- rmm(data = df, Ps_col = Ps, outcome_col = survival, pivot = FALSE, n_samples = 5)
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

testthat::test_that("nonlinear_bins produces correct bin data", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  bin_data <- nonlinear_bins(
    df,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 5,
    divisor2 = 5,
    threshold_1 = 0.9,
    threshold_2 = 0.99
  )

  testthat::expect_true("bin_stats" %in% names(bin_data))
  testthat::expect_true("intervals" %in% names(bin_data))
  testthat::expect_equal(nrow(bin_data$bin_stats), 10)  # assuming 5 bins
})

testthat::test_that("bootstrap data has the correct number of samples", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  bootstrap_data <- df |>
    infer::generate(reps = 100, type = "bootstrap")

  testthat::expect_equal(nrow(bootstrap_data), 100 * nrow(df))
})

testthat::test_that("bin statistics are calculated correctly", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  try_function <- rm_bin_summary(df, Ps, survival, n_samples = 5)

  testthat::expect_true("TA_b" %in% names(try_function))
  testthat::expect_true("TD_b" %in% names(try_function))
  testthat::expect_true("EM_b" %in% names(try_function))
})

testthat::test_that("RMM is calculated correctly", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  rmm_result <- df |>
    rm_bin_summary(Ps, survival, n_samples = 5)

  testthat::expect_true("population_RMM" %in% names(rmm_result))
  testthat::expect_true(all(rmm_result$population_RMM >= -1) || all(rmm_result$population_RMM <= 1))

})

testthat::test_that("confidence intervals are correctly computed in final RMM", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  rmm_result <- rm_bin_summary(df, Ps, survival, n_samples = 5)

  testthat::expect_true("bootstrap_RMM_LL" %in% names(rmm_result))
  testthat::expect_true("bootstrap_RMM_UL" %in% names(rmm_result))
  testthat::expect_true("bootstrap_CI" %in% names(rmm_result))
  testthat::expect_true(all(rmm_result$bootstrap_RMM_UL > rmm_result$bootstrap_RMM_LL))  # CI upper should be greater than lower
})

testthat::test_that("RMM final data is correctly sorted by bin_number", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  rmm_result_final <- rm_bin_summary(df, Ps, survival, n_samples = 5)

  testthat::expect_equal(min(rmm_result_final$bin_number), 1)
  testthat::expect_equal(max(rmm_result_final$bin_number), 10)  # Assuming 10 bins

})
