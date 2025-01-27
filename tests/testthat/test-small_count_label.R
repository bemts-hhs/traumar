test_that("small_count_label returns correct output for numeric replacement", {
  expect_equal(small_count_label(c(1, 5, 10), 5, 0), c(0, 5, 10))
  expect_equal(small_count_label(c(1, 4, 10), 5, 0), c(0, 0, 10))
  expect_equal(small_count_label(c(1, 2, 3), 3, 0), c(0, 0, 3))
  expect_equal(small_count_label(c(1, 5, 10), 10, 0), c(0, 0, 10))
})

test_that("small_count_label returns correct output for character replacement", {
  expect_equal(small_count_label(c(1, 5, 10), 5, "Below Cutoff"), c("Below Cutoff", "5", "10"))
  expect_equal(small_count_label(c(1, 4, 10), 5, "Below Cutoff"), c("Below Cutoff", "Below Cutoff", "10"))
  expect_equal(small_count_label(c(1, 2, 3), 3, "Below Cutoff"), c("Below Cutoff", "Below Cutoff", "3"))
  expect_equal(small_count_label(c(1, 5, 10), 10, "Below Cutoff"), c("Below Cutoff", "Below Cutoff", "10"))
})

test_that("small_count_label handles invalid var input", {
  expect_error(small_count_label("text", 5, "Below Cutoff"), regexp = "must be a numeric vector")
  expect_error(small_count_label(list(1, 2, 3), 5, "Below Cutoff"), regexp = "must be a numeric vector")
})

test_that("small_count_label handles invalid cutoff input", {
  expect_error(small_count_label(c(1, 2, 3), "five", "Below Cutoff"), regexp = "must be a single numeric value")
  expect_error(small_count_label(c(1, 2, 3), c(5, 6), "Below Cutoff"), regexp = "must be a single numeric value")
})

test_that("small_count_label handles invalid replacement input", {
  expect_error(small_count_label(c(1, 2, 3), 5, list("Below Cutoff")), regexp = "must be either a string or a numeric value")
  expect_error(small_count_label(c(1, 2, 3), 5, TRUE), regexp = "must be either a string or a numeric value")
})

test_that("small_count_label handles edge cases", {
  expect_equal(small_count_label(c(5, 5, 5), 5, "At Cutoff"), c(5, 5, 5))  # Values equal to cutoff remain unchanged
  expect_equal(small_count_label(c(0, -1, 5), 0, "Below Zero"), c("0", "Below Zero", "5"))  # Edge case for negative and zero values
})
