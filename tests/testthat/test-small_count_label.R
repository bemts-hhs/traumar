test_that("small_count_label returns correct output for numeric replacement", {
  expect_equal(small_count_label(c(1, 5, 10), 5, 0), c(0, 5, 10))
  expect_equal(small_count_label(c(1, 4, 10), 5, 0), c(0, 0, 10))
  expect_equal(small_count_label(c(1, 2, 3), 3, 0), c(0, 0, 3))
  expect_equal(small_count_label(c(1, 5, 10), 10, 0), c(0, 0, 10))
})

test_that("small_count_label returns correct output for character replacement", {
  expect_equal(
    small_count_label(c(1, 5, 10), 5, "Below Cutoff"),
    c("Below Cutoff", "5", "10")
  )
  expect_equal(
    small_count_label(c(1, 4, 10), 5, "Below Cutoff"),
    c("Below Cutoff", "Below Cutoff", "10")
  )
  expect_equal(
    small_count_label(c(1, 2, 3), 3, "Below Cutoff"),
    c("Below Cutoff", "Below Cutoff", "3")
  )
  expect_equal(
    small_count_label(c(1, 5, 10), 10, "Below Cutoff"),
    c("Below Cutoff", "Below Cutoff", "10")
  )
})

test_that("small_count_label handles invalid var input", {
  expect_error(
    small_count_label("text", 5, "Below Cutoff"),
    regexp = "var.*must be.*numeric"
  )
  expect_error(
    small_count_label(list(1, 2, 3), 5, "Below Cutoff"),
    regexp = "var.*must be.*numeric"
  )
})

test_that("small_count_label handles invalid cutoff input", {
  expect_error(
    small_count_label(c(1, 2, 3), "five", "Below Cutoff"),
    regexp = "cutoff.*must be.*numeric"
  )
  expect_error(
    small_count_label(c(1, 2, 3), c(5, 6), "Below Cutoff"),
    regexp = "cutoff.*must have an exact length of 1"
  )
})

test_that("small_count_label handles invalid replacement input", {
  expect_error(
    small_count_label(c(1, 2, 3), 5, list("Below Cutoff")),
    regexp = "replacement.*must be of class.*numeric, character, logical"
  )
  expect_error(
    small_count_label(c(1, 2, 3), 5, data.frame(x = "Below Cutoff")),
    regexp = "replacement.*must be of class.*numeric, character, logical"
  )
})

test_that("small_count_label handles edge cases", {
  expect_equal(small_count_label(c(5, 5, 5), 5, "At Cutoff"), c(5, 5, 5)) # Values equal to cutoff remain unchanged
  expect_equal(
    small_count_label(c(0, -1, 5), 0, "Below Zero"),
    c("0", "Below Zero", "5")
  ) # Edge case for negative and zero values
})
