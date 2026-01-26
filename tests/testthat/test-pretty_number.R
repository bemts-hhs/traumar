test_that("pretty_number handles basic formatting", {
  expect_equal(pretty_number(1234), "1.23k")
  expect_equal(pretty_number(1234567), "1.23m")
  expect_equal(pretty_number(1234567890), "1.23b")
})

test_that("pretty_number respects the digits argument", {
  # set up options
  withr::local_options(lifecycle_verbosity = "warning")

  # tests
  expect_equal(pretty_number(1234, digits = 1), "1.2k")
  expect_equal(pretty_number(1234, digits = 0), "1k")
  expect_error(
    pretty_number(1234, digits = "two")
  )
})

test_that("pretty_number adds prefix correctly", {
  expect_equal(pretty_number(1234, prefix = "$"), "$1.23k")
  expect_equal(pretty_number(1234, prefix = "€"), "€1.23k")
})

test_that("pretty_number handles rounding correctly", {
  expect_equal(pretty_number(1256, truncate = TRUE), "1.3k")
  expect_equal(pretty_number(1256, truncate = FALSE), "1.26k")
})

test_that("pretty_number supports large numbers up to decillions", {
  expect_equal(pretty_number(1e33), "1dec")
  expect_equal(pretty_number(1e30), "1non")
  expect_equal(pretty_number(1e27), "1oct")
})

test_that("pretty_number handles small numbers without formatting", {
  expect_equal(pretty_number(1), "1")
  expect_equal(pretty_number(999), "999")
})

test_that("pretty_number handles invalid x input", {
  expect_error(pretty_number("abc"), "x.*must be of class.*numeric, integer.*")
  expect_error(pretty_number(TRUE), "x.*must be of class.*numeric, integer.*")
})

test_that("pretty_number validates digits argument and warns about the deprecated digits argument", {
  expect_error(
    pretty_number(1234, digits = "two"),
    "digits.*must be of class.*numeric, integer.*."
  )
  expect_no_error(
    pretty_number(1234, digits = 2.5)
  )
})

test_that("pretty_number validates prefix argument", {
  expect_error(
    pretty_number(1234, prefix = 123),
    "prefix.*must be of class.*character"
  )
})

test_that("pretty_number handles NA and NULL values gracefully", {
  expect_error(pretty_number(NA))
  expect_error(pretty_number(NULL))
})

test_that("pretty_number handles vectorized input", {
  input <- c(1234, 5678, 9101112)
  result <- pretty_number(input)
  expected <- c("1.23k", "5.68k", "9.1m")
  expect_equal(result, expected)
})

test_that("pretty_number handles edge cases for large and small numbers", {
  expect_equal(pretty_number(1e12), "1t")
  expect_equal(pretty_number(1e6), "1m")
  expect_equal(pretty_number(1e-3), "0")
  expect_equal(pretty_number(0), "0")
})

test_that("n_decimal and digits give identical results", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    pretty_number(1234567, n_decimal = 1),
    pretty_number(1234567, digits = 1)
  )
})

test_that("pretty_number() n_decimal is lifecycle-deprecated", {
  withr::local_options(lifecycle_verbosity = "error")

  expect_error(
    pretty_number(1234, n_decimal = 1),
    class = "lifecycle_error_deprecated"
  )
})
