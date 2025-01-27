testthat::test_that("weekend classifies correctly for valid dates", {

  # Test for a weekend date (Saturday)
  testthat::expect_equal(weekend(as.Date("2025-01-18")), "Weekend")

  # Test for a weekend date (Sunday)
  testthat::expect_equal(weekend(as.Date("2025-01-19")), "Weekend")

  # Test for a weekday date (Monday)
  testthat::expect_equal(weekend(as.Date("2025-01-15")), "Weekday")

  # Test for a weekday date (Friday)
  testthat::expect_equal(weekend(as.Date("2025-01-17")), "Weekday")
})

testthat::test_that("weekend handles POSIXct correctly", {

  # Test for a weekend date (Saturday) as POSIXct
  testthat::expect_equal(weekend(as.POSIXct("2025-01-18 00:00:00")), "Weekend")

  # Test for a weekday date (Monday) as POSIXct
  testthat::expect_equal(weekend(as.POSIXct("2025-01-15 00:00:00")), "Weekday")
})

testthat::test_that("weekend throws error for invalid input", {

  # Test for invalid input: character string
  testthat::expect_error(weekend("2025-01-18"), "must be an object of class")

  # Test for invalid input: numeric value
  testthat::expect_error(weekend(20250118),
               "must be an object of class")

  # Test for invalid input: NULL
  testthat::expect_error(weekend(NULL),
               "must be an object of class")
})

testthat::test_that("weekend works with vectorized input", {

  # Test for a mix of weekend and weekday dates
  input_dates <- as.Date(c("2025-01-18", "2025-01-15", "2025-01-19", "2025-01-17"))
  expected_output <- c("Weekend", "Weekday", "Weekend", "Weekday")

  testthat::expect_equal(weekend(input_dates), expected_output)
})

testthat::test_that("weekend returns correct classification for all weekdays", {

  weekdays_dates <- as.Date(c("2025-01-13", "2025-01-14", "2025-01-15", "2025-01-16", "2025-01-17"))
  weekdays_classification <- rep("Weekday", 5)

  testthat::expect_equal(weekend(weekdays_dates), weekdays_classification)
})

testthat::test_that("weekend returns correct classification for all weekend days", {

  weekend_dates <- as.Date(c("2025-01-18", "2025-01-19"))
  weekend_classification <- rep("Weekend", 2)

  testthat::expect_equal(weekend(weekend_dates), weekend_classification)
})
