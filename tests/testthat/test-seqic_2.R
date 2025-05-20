testthat::test_that("seqic_indicator_2() validates `data` input type", {
  testthat::expect_error(
    traumar::seqic_indicator_2(
      data = "not_a_data",
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time
    ),
    "data.*must be of class"
  )
})

testthat::test_that("seqic_indicator_2() validates `unique_incident_id` column type", {
  data <- tibble::tibble(
    incident_id = logical(length = 1),
    trauma_level = "I",
    incident_time = Sys.time() # invalid type
  )
  testthat::expect_error(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time
    ),
    "unique_incident_id.*must be of class"
  )
})

testthat::test_that("seqic_indicator_2() validates `incident_time` column type", {
  data <- tibble::tibble(
    incident_id = 1,
    trauma_level = "I",
    incident_time = 12345 # invalid type
  )
  testthat::expect_error(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time
    ),
    "incident_time.*must be of class"
  )
})

testthat::test_that("seqic_indicator_2() validates `level` column type", {
  data <- tibble::tibble(
    incident_id = 1,
    trauma_level = 3, # numeric, invalid
    incident_time = Sys.time()
  )
  testthat::expect_error(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time
    ),
    "level.*must be of class"
  )
})

testthat::test_that("seqic_indicator_2() validates `groups` is character vector", {
  data <- tibble::tibble(
    incident_id = 1,
    trauma_level = "I",
    incident_time = Sys.time()
  )
  testthat::expect_error(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time,
      groups = list(123)
    ),
    "All elements in.*groups.*must be strings"
  )
})

testthat::test_that("seqic_indicator_2() validates `groups` exist in data", {
  data <- tibble::tibble(
    incident_id = 1,
    trauma_level = "I",
    incident_time = Sys.time()
  )
  testthat::expect_error(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time,
      groups = c("nonexistent_column")
    ),
    "group variable.*are not valid columns"
  )
})

testthat::test_that("seqic_indicator_2() validates `calculate_ci` options", {
  data <- tibble::tibble(
    incident_id = 1,
    trauma_level = "I",
    incident_time = Sys.time()
  )
  testthat::expect_error(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time,
      calculate_ci = "invalid_method"
    ),
    "calculate_ci.*must be.*wilson.*clopper-pearson"
  )
})

testthat::test_that("seqic_indicator_2() validates `included_levels` class", {
  data <- tibble::tibble(
    incident_id = 1,
    trauma_level = "I",
    incident_time = Sys.time()
  )
  testthat::expect_error(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time,
      included_levels = list("I", "II")
    ),
    "included_levels.*must be of class"
  )
})

testthat::test_that("seqic_indicator_2 basic calculations work", {
  data <- tibble::tibble(
    incident_id = c("A", "B", "C", "D", "E"),
    trauma_level = c("I", "II", "III", "V", "II"),
    incident_time = as.POSIXct(c(
      NA,
      "2023-01-02 12:00",
      NA,
      "2023-01-03 14:00",
      "2023-01-04 15:00"
    ))
  )

  result <- traumar::seqic_indicator_2(
    data = data,
    unique_incident_id = incident_id,
    level = trauma_level,
    incident_time = incident_time
  )

  testthat::expect_true(tibble::is_tibble(result))
  testthat::expect_equal(result$numerator_2, 2)
  testthat::expect_equal(result$denominator_2, 4) # Level V excluded
  testthat::expect_equal(result$seqic_2, 0.5)
})

testthat::test_that("seqic_indicator_2 handles confidence intervals", {
  data <- tibble::tibble(
    incident_id = c("A", "B", "C", "D"),
    trauma_level = c("I", "II", "III", "IV"),
    incident_time = as.POSIXct(c(
      NA,
      "2023-01-01 12:00",
      "2023-01-01 13:00",
      NA
    ))
  )

  result <- traumar::seqic_indicator_2(
    data = data,
    unique_incident_id = incident_id,
    level = trauma_level,
    incident_time = incident_time,
    calculate_ci = "clopper-pearson"
  )

  testthat::expect_true("lower_ci_2" %in% names(result))
  testthat::expect_true("upper_ci_2" %in% names(result))
  testthat::expect_true(result$lower_ci_2 <= result$seqic_2)
  testthat::expect_true(result$upper_ci_2 >= result$seqic_2)
})

testthat::test_that("seqic_indicator_2 respects grouping", {
  data <- tibble::tibble(
    incident_id = c("A", "B", "C", "D", "E", "F"),
    trauma_level = c("I", "I", "II", "II", "III", "III"),
    region = c("East", "East", "West", "West", "East", "East"),
    incident_time = as.POSIXct(c(NA, NA, "2023-01-01", NA, NA, "2023-01-01"))
  )

  result <- traumar::seqic_indicator_2(
    data = data,
    unique_incident_id = incident_id,
    level = trauma_level,
    incident_time = incident_time,
    groups = "region"
  )

  testthat::expect_true("region" %in% names(result))
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true(all(result$seqic_2 >= 0 & result$seqic_2 <= 1))
})

testthat::test_that("seqic_indicator_2 handles character incident_time (invalid)", {
  data <- tibble::tibble(
    incident_id = c("A", "B"),
    trauma_level = c("I", "II"),
    incident_time = c(NA, "2023-01-01 12:00") # Not POSIXct
  )

  testthat::expect_warning(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time
    ),
    regexp = NA # no warning expected even though type is not POSIXct
  )

  result <- suppressWarnings(
    traumar::seqic_indicator_2(
      data = data,
      unique_incident_id = incident_id,
      level = trauma_level,
      incident_time = incident_time
    )
  )

  testthat::expect_equal(result$numerator_2, 1)
})

testthat::test_that("seqic_indicator_2 returns label if not grouped", {
  data <- tibble::tibble(
    incident_id = c("A", "B"),
    trauma_level = c("I", "II"),
    incident_time = as.POSIXct(c(NA, "2023-01-01 12:00"))
  )

  result <- traumar::seqic_indicator_2(
    data = data,
    unique_incident_id = incident_id,
    level = trauma_level,
    incident_time = incident_time
  )

  testthat::expect_true("data" %in% names(result))
  testthat::expect_equal(result$data, "population/sample")
})
