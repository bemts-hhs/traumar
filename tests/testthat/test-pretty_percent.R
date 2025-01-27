test_that("pretty_percent handles numeric input", {
  expect_equal(pretty_percent(0.12345), "12.3%")
  expect_equal(pretty_percent(0.12345, n_decimal = 2), "12.35%")
  expect_equal(pretty_percent(0.12345, n_decimal = 0), "12%")
  expect_equal(pretty_percent(c(0.1, 0.25, 0.3333), n_decimal = 1), c("10%", "25%", "33.3%"))
})

test_that("pretty_percent handles non-numeric input", {
  expect_error(pretty_percent("text"), "The `variable` argument must be numeric.")
  expect_error(pretty_percent(list(1, 2, 3)), "The `variable` argument must be numeric.")
})

test_that("pretty_percent handles n_decimal input correctly", {
  expect_error(pretty_percent(0.5, n_decimal = -1), "positive numeric value.")
  expect_equal(pretty_percent(0.5, n_decimal = 0), "50%")
})

test_that("pretty_percent removes unnecessary trailing zeros and periods", {
  expect_equal(pretty_percent(0.12345, n_decimal = 3), "12.345%")
  expect_equal(pretty_percent(0.5, n_decimal = 3), "50%")
  expect_equal(pretty_percent(0.33333, n_decimal = 3), "33.333%")
  expect_equal(pretty_percent(0.123, n_decimal = 2), "12.3%")
  expect_equal(pretty_percent(0.01, n_decimal = 1), "1%")
  expect_equal(pretty_percent(1, n_decimal = 0), "100%")
})

test_that("pretty_percent handles edge cases correctly", {
  expect_equal(pretty_percent(0), "0%")
  expect_equal(pretty_percent(1), "100%")
  expect_equal(pretty_percent(0.9999, n_decimal = 2), "99.99%")
  expect_equal(pretty_percent(-0.1), "-10%")
  expect_equal(pretty_percent(-0.12345, n_decimal = 3), "-12.345%")
})

test_that("pretty_percent handles vector input correctly", {
  input <- c(0.1, 0.25, 0.3333, 0.5)
  expected <- c("10%", "25%", "33.3%", "50%")
  expect_equal(pretty_percent(input, n_decimal = 1), expected)
})
