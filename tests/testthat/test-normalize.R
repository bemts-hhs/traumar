test_that("normalize handles min-max normalization correctly", {
  x <- c(10, 20, 30, 40, 50)
  expected <- (x - min(x)) / (max(x) - min(x))
  result <- normalize(x, method = "min_max")
  expect_equal(result, expected)
})

test_that("normalize handles z-score normalization correctly", {
  x <- c(10, 20, 30, 40, 50)
  expected <- (x - mean(x)) / sd(x)
  result <- normalize(x, method = "z_score")
  expect_equal(result, expected)
})

test_that("normalize handles NA values correctly", {
  x <- c(10, 20, NA, 40, 50)

  # Min-max normalization
  expected_min_max <- (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  result_min_max <- normalize(x, method = "min_max")
  expect_equal(result_min_max, expected_min_max)

  # Z-score normalization
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  expected_z_score <- (x - mean_x) / sd_x
  result_z_score <- normalize(x, method = "z_score")
  expect_equal(result_z_score, expected_z_score)
})

test_that("normalize defaults to min-max normalization when method is not supplied", {
  x <- c(10, 20, 30, 40, 50)
  expected <- (x - min(x)) / (max(x) - min(x))
  result <- normalize(x)
  expect_equal(result, expected)
})

test_that("normalize handles invalid method input by defaulting to min-max normalization", {
  x <- c(10, 20, 30, 40, 50)
  expected <- (x - min(x)) / (max(x) - min(x))
  expect_error(normalize(x, method = "invalid"))
})

test_that("normalize raises an error for non-numeric inputs", {
  expect_error(normalize(c("a", "b", "c")))
})

test_that("normalize handles integer inputs correctly", {
  x <- c(10L, 20L, 30L, 40L, 50L)
  expected_min_max <- (x - min(x)) / (max(x) - min(x))
  expected_z_score <- (x - mean(x)) / sd(x)

  result_min_max <- normalize(x, method = "min_max")
  result_z_score <- normalize(x, method = "z_score")

  expect_equal(result_min_max, expected_min_max)
  expect_equal(result_z_score, expected_z_score)
})

test_that("normalize handles empty input gracefully", {
  x <- numeric(0)

  expect_warning(
    result <- normalize(x, method = "min_max")
  )
  expect_equal(result, numeric(0))

  expect_warning(
    result <- normalize(x, method = "z_score")
  )
  expect_equal(result, numeric(0))
})
