test_that("probability_of_survival handles valid inputs correctly", {
  trauma_type <- c("Blunt", "Penetrating")
  age <- c(30, 60)
  rts <- c(7.84, 6.90)
  iss <- c(10, 25)

  expected <- c(
    1 / (1 + exp(-(-0.4499 + 0.8085 * 7.84 - 0.0835 * 10 - (1.7430 * 0)))),
    1 / (1 + exp(-(-2.5355 + 0.9934 * 6.90 - 0.0651 * 25 - (1.1360 * 1))))
  )

  result <- probability_of_survival(trauma_type, age, rts, iss)
  expect_equal(result, expected)
})

# Test invalid trauma_type
test_that("probability_of_survival handles invalid trauma_type", {
  trauma_type <- c("Blunt", "Unknown")
  age <- c(30, 60)
  rts <- c(7.84, 6.90)
  iss <- c(10, 25)

  expect_warning(probability_of_survival(trauma_type, age, rts, iss))
})

# Test error in trauma_type
test_that("probability_of_survival handles invalid trauma_type", {
  trauma_type <- c(2, 1)
  age <- c(30, 60)
  rts <- c(7.84, 6.90)
  iss <- c(10, 25)

  expect_error(probability_of_survival(trauma_type, age, rts, iss))
})

# Test invalid age
test_that("probability_of_survival handles invalid age", {
  trauma_type <- c("Blunt", "Penetrating")
  age <- c(-5, 60) # Negative age
  rts <- c(7.84, 6.90)
  iss <- c(10, 25)

  expect_warning(probability_of_survival(trauma_type, age, rts, iss))
})

# Test invalid rts
test_that("probability_of_survival handles invalid rts", {
  trauma_type <- c("Blunt", "Penetrating")
  age <- c(30, 60)
  rts <- c(8, 6.90) # Out of range RTS
  iss <- c(10, 25)

  expect_warning(probability_of_survival(trauma_type, age, rts, iss))
})

# Test invalid iss
test_that("probability_of_survival handles invalid iss", {
  trauma_type <- c("Blunt", "Penetrating")
  age <- c(30, 60)
  rts <- c(7.84, 6.90)
  iss <- c(10, 80) # Out of range ISS

  expect_warning(probability_of_survival(trauma_type, age, rts, iss))
})

# Test handling of 'Burn' trauma type
test_that("probability_of_survival warns about 'Burn' trauma type", {
  trauma_type <- c("Blunt", "Burn")
  age <- c(30, 60)
  rts <- c(7.84, 6.90)
  iss <- c(10, 25)

  expect_warning(probability_of_survival(trauma_type, age, rts, iss))
})

# Test NA handling
test_that("probability_of_survival handles invalid values correctly", {
  trauma_type <- c("Blunt", "stuff")
  age <- c(30, 60)
  rts <- c(7.84, 6)
  iss <- c(10, 25)

  testthat::expect_warning(probability_of_survival(trauma_type, age, rts, iss))
})

# Test NA handling
test_that("probability_of_survival handles NA values correctly", {
  trauma_type <- c("Blunt", NA)
  age <- c(30, 60)
  rts <- c(7.84, NA)
  iss <- c(10, NA)

  expect_error(probability_of_survival(trauma_type, age, rts, iss))
})
