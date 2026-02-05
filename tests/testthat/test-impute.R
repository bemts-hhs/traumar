test_that("impute() handles non-numeric input", {
  expect_error(impute(c("a", "b", "c")), "x: must be.*numeric")
})

test_that("impute() defaults focus and method correctly", {
  x <- c(10, 20, 30, 10000, 2000, NA)
  expect_error(
    impute(x, focus = c("skew", "missing")),
    "must have an exact length of 1"
  )
  expect_error(
    impute(x, focus = "skew", method = c("winsorize", "iqr")),
    "must have an exact length of 1"
  )
  expect_error(
    impute(x, focus = "missing", method = c("mean", "median")),
    "must have an exact length of 1"
  )
})

test_that("impute() validates method for skew focus", {
  x <- c(10, 20, 30, 10000, 2000, NA)
  expect_error(
    impute(x, focus = "skew", method = "mean")
  )
})


test_that("impute() validates percentile for winsorization", {
  x <- c(10, 20, 30, 10000, 2000, NA)
  expect_error(impute(
    x,
    focus = "skew",
    method = "winsorize",
    percentile = -0.1
  ))
  expect_no_error(impute(
    x,
    focus = "skew",
    method = "winsorize",
    percentile = 0.1
  ))
})

test_that("impute() correctly winsorizes with default percentiles", {
  x <- c(10, 20, 30, 10000, 2000, NA)
  result <- impute(x, focus = "skew", method = "winsorize")
  expect_equal(result, c(10.4, 20.0, 30.0, 9680.0, 2000.0, NA)) # Default 1% and 99%
})

test_that("impute() correctly winsorizes with custom percentiles", {
  x <- c(10, 20, 30, 10000, 2000, NA)
  result <- impute(x, focus = "skew", method = "winsorize", percentile = 0.05)
  expect_equal(result, c(8400.0, 12.0, 12.0, 12.0, 12.0, NA)) # Custom 5% and 95%
})

test_that("impute() handles IQR for extreme values", {
  x <- c(10, 20, 30, 10000, 2000, NA)
  result <- impute(x, focus = "skew", method = "iqr")
  expect_equal(result, c(10, 20, 30, 4970, 2000, NA)) # IQR-based limits
})

test_that("impute() imputes missing values with mean", {
  x <- c(10, 20, 30, 10000, 2000, NA)
  result <- impute(x, focus = "missing", method = "mean")
  expect_equal(result, c(10, 20, 30, 10000, 2000, 2412)) # Mean = 2412
})

test_that("impute() imputes missing values with median", {
  x <- c(10, 20, 30, 10000, 2000, NA)
  result <- impute(x, focus = "missing", method = "median")
  expect_equal(result, c(10, 20, 30, 10000, 2000, 30)) # Median = 30
})
