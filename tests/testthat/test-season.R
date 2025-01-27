test_that("season returns correct season for valid Date input", {
  expect_equal(season(as.Date("2025-01-15")), factor("Winter"))
  expect_equal(season(as.Date("2025-05-10")), factor("Spring"))
  expect_equal(season(as.Date("2025-07-01")), factor("Summer"))
  expect_equal(season(as.Date("2025-10-15")), factor("Fall"))
})

test_that("season returns correct season for valid POSIXct input", {
  expect_equal(season(as.POSIXct("2025-01-15 12:00:00")), factor("Winter"))
  expect_equal(season(as.POSIXct("2025-04-25 08:00:00")), factor("Spring"))
  expect_equal(season(as.POSIXct("2025-08-01 18:00:00")), factor("Summer"))
  expect_equal(season(as.POSIXct("2025-09-30 00:00:00")), factor("Fall"))
})

test_that("season returns Undetermined for invalid input", {
  expect_error(season("2025-01-15"), regexp = "must be an object of class")
  expect_error(season(20250115), regexp = "must be an object of class")
  expect_error(season(NULL), regexp = "must be an object of class")
})

test_that("season handles NA input correctly", {
  expect_error(season(NA))
})

test_that("season handles edge cases for months", {
  expect_equal(season(as.Date("2025-12-01")), factor("Winter"))
  expect_equal(season(as.POSIXct("2025-03-01 00:00:00")), factor("Spring"))
})
