testthat::test_that("trauma_performance handles invalid dataframe input", {
  testthat::expect_error(
    trauma_performance(NULL, Ps_col = Ps, outcome_col = death),
    regexp = "The first argument must be a dataframe."
  )
})

testthat::test_that("trauma_performance checks for binary outcome column", {
  df <- tibble::tibble(Ps = c(0.8, 0.9), outcome_col = c(100, 1))
  testthat::expect_error(
    trauma_performance(df, Ps_col = Ps, outcome_col = outcome_col),
    regexp = "contains numeric values other than 0 and 1"
  )
})

testthat::test_that("trauma_performance correctly processes probabilities above 1", {
  df <- tibble::tibble(Ps = c(12, 20), death = c(1, 0))
  expect_error(
    trauma_performance(df, Ps_col = Ps, outcome_col = death),
    regexp = "probability of survival.*values must be between 0 and 1"
  )
})

testthat::test_that("trauma_performance validates Ps column is numeric", {
  df <- tibble::tibble(Ps = c("high", "medium"), death = c(1, 0))
  testthat::expect_error(
    trauma_performance(df, Ps_col = Ps, outcome_col = death),
    "column must be numeric."
  )
})

testthat::test_that("trauma_performance checks if Ps values are between 0 and 100", {
  df <- tibble::tibble(Ps = c(101, -1), death = c(1, 0))
  testthat::expect_error(
    trauma_performance(df, Ps_col = Ps, outcome_col = death),
    regexp = "values must be between 0 and 1"
  )
})

testthat::test_that("trauma_performance calculates W-score", {
  df <- tibble::tibble(Ps = c(0.8, 0.9), death = c(1, 0))
  result <- trauma_performance(df, Ps_col = Ps, outcome_col = death)
  testthat::expect_true("W_Score" %in% names(result))
})

testthat::test_that("trauma_performance calculates M-score correctly", {
  df <- tibble::tibble(Ps = c(0.8, 0.9), death = c(1, 0))
  result <- trauma_performance(df, Ps_col = Ps, outcome_col = death)
  testthat::expect_true("M_Score" %in% names(result))
})

testthat::test_that("trauma_performance calculates Z-score with survival method", {
  df <- tibble::tibble(Ps = c(0.8, 0.9), death = c(1, 0))
  result <- trauma_performance(
    df,
    Ps_col = Ps,
    outcome_col = death,
    z_method = "survival"
  )
  testthat::expect_true("Z_Score" %in% names(result))
})

testthat::test_that("trauma_performance calculates Z-score with mortality method", {
  df <- tibble::tibble(Ps = c(0.8, 0.9), death = c(1, 0))
  result <- trauma_performance(
    df,
    Ps_col = Ps,
    outcome_col = death,
    z_method = "mortality"
  )
  testthat::expect_true("Z_Score" %in% names(result))
})

testthat::test_that("trauma_performance works when outcome_col is TRUE/FALSE", {
  df <- tibble::tibble(Ps = c(0.8, 0.9), death = c(TRUE, FALSE))
  result <- trauma_performance(df, Ps_col = Ps, outcome_col = death)
  testthat::expect_true("W_Score" %in% names(result))
})

testthat::test_that("trauma_performance handles edge case of empty dataframe", {
  df <- tibble::tibble(Ps = numeric(0), death = logical(0))

  result <- trauma_performance(df, Ps_col = Ps, outcome_col = death)

  # Additionally, check if the result is a tibble with the correct structure (optional)
  testthat::expect_s3_class(result, "tbl_df") # Check that the result is a tibble
  testthat::expect_equal(nrow(result), 1) # Verify that 9 rows are in the result
  testthat::expect_equal(ncol(result), 9) # Verify that there are 2 columns
})
