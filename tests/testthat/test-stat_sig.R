test_that("stat_sig works within a mutate call", {
  # Create a data frame with a column of p-values
  data <- data.frame(p_value = c(0.0005, 0.02, 0.08, 0.15, 0.009))

  # Expected result based on the p-value thresholds
  expected_significance <- c("***", "*", ".", "<>", "**")

  # Apply stat_sig within mutate and check the result
  result <- data |>
    dplyr::mutate(significance = stat_sig(p_val_data = p_value))

  # Compare the 'significance' column with the expected result
  expect_equal(result$significance, expected_significance)
})

test_that("stat_sig works with p-values in the middle of thresholds in a mutate", {
  # Create a data frame with a column of p-values
  data <- data.frame(p_value = c(0.001, 0.01, 0.05, 0.1, 0.15))

  # Expected result for the given p-values
  expected_significance <- c("***", "**", "*", ".", "<>")

  # Apply stat_sig within mutate and check the result
  result <- data |>
    dplyr::mutate(significance = stat_sig(p_value))

  # Compare the 'significance' column with the expected result
  expect_equal(result$significance, expected_significance)
})

test_that("stat_sig works with an empty data frame in mutate", {
  # Create an empty data frame
  data <- data.frame(p_value = numeric(0))

  # Apply stat_sig within mutate (should return an empty 'significance' column)
  result <- data |>
    dplyr::mutate(significance = stat_sig(p_value))

  # Check that the resulting data frame has no rows and the correct column
  expect_equal(nrow(result), 0)
  expect_true("significance" %in% colnames(result))
})

test_that("stat_sig handles NA values in a mutate call", {
  # Create a data frame with NA values in p_value
  data <- data.frame(p_value = c(0.0005, NA, 0.05, 0.08, 0.15))

  # Expected result with significance codes and NA preserved
  expected_significance <- c("***", NA, "*", ".", "<>")

  # Apply stat_sig within mutate and check the result
  result <- data |>
    dplyr::mutate(significance = stat_sig(p_value))

  # Compare the 'significance' column with the expected result
  expect_equal(result$significance, expected_significance)
})

test_that("stat_sig correctly assigns significance codes", {
  # Test case with various p-values
  p_vals <- c(0.0005, 0.005, 0.02, 0.08, 0.15)
  expected <- c("***", "**", "*", ".", "<>")

  result <- stat_sig(p_vals)
  expect_equal(result, expected)
})

test_that("stat_sig handles p-values at the boundary correctly", {
  # Test case with boundary p-values
  p_vals <- c(0.001, 0.01, 0.05, 0.1, 0.15)
  expected <- c("***", "**", "*", ".", "<>")

  result <- stat_sig(p_vals)
  expect_equal(result, expected)
})

test_that("stat_sig throws an error for non-numeric input", {
  # Test non-numeric input
  expect_error(stat_sig("a string"),
               "must be a numeric vector")
})

test_that("stat_sig throws an error for p-values outside the [0, 1] range", {
  # Test p-values out of range
  expect_error(stat_sig(c(-0.01, 0.5, 1.2)),
               "must be between 0 and 1.")
})

test_that("stat_sig handles edge case of empty vector", {
  # Test with an empty vector
  result <- stat_sig(numeric(0))
  expect_no_success(result)
})

