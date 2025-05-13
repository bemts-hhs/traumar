# tests/testthat/test-seqic_indicator_7.R

test_that("Valid input returns expected structure and values", {
  df <- tibble::tibble(
    id = as.character(1:5),
    trauma_level = c("I", "II", "III", "IV", "V"),
    time_to_arrival = c(200, 100, 220, 150, 400),
    transfer_out = c("No", "No", "No", "No", "Yes")
  )

  res <- traumar::seqic_indicator_7(
    df = df,
    level = trauma_level,
    unique_incident_id = id,
    time_from_injury_to_arrival = time_to_arrival,
    transfer_out_indicator = transfer_out
  )

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("data", "numerator_7", "denominator_7", "seqic_7"))
  expect_equal(res$numerator_7, 2)
  expect_equal(res$denominator_7, 4)
  expect_equal(res$seqic_7, 0.5)
})

test_that("Grouping works correctly", {
  df <- tibble::tibble(
    id = c(1, 2, 3, 4),
    trauma_level = c("I", "I", "II", "II"),
    time_to_arrival = c(200, 100, 300, 90),
    transfer_out = c("No", "No", "No", "No"),
    region = c("North", "North", "South", "South")
  )

  res <- traumar::seqic_indicator_7(
    df,
    level = trauma_level,
    unique_incident_id = id,
    time_from_injury_to_arrival = time_to_arrival,
    transfer_out_indicator = transfer_out,
    groups = "region"
  )

  expect_equal(nrow(res), 2)
  expect_true(all(
    c("region", "numerator_7", "denominator_7", "seqic_7") %in% names(res)
  ))
})

test_that("Confidence intervals are added correctly", {
  df <- tibble::tibble(
    id = 1:6,
    trauma_level = rep("I", 6),
    time_to_arrival = c(200, 210, 90, 80, 185, 195),
    transfer_out = rep("No", 6)
  )

  res <- traumar::seqic_indicator_7(
    df,
    level = trauma_level,
    unique_incident_id = id,
    time_from_injury_to_arrival = time_to_arrival,
    transfer_out_indicator = transfer_out,
    calculate_ci = "wilson"
  )

  expect_true(all(c("lower_ci_7", "upper_ci_7") %in% names(res)))
  expect_true(res$lower_ci_7 < res$seqic_7 && res$upper_ci_7 > res$seqic_7)
})

#-------------------------------
# Input Validation Tests
#-------------------------------

test_that("Errors are thrown for invalid `df` input", {
  expect_error(
    traumar::seqic_indicator_7(
      "not_a_df",
      trauma_level,
      id,
      time_to_arrival,
      transfer_out
    ),
    "df"
  )
})

test_that("Errors are thrown for invalid `level` column type", {
  df <- tibble::tibble(
    id = 1,
    trauma_level = 1L,
    time_to_arrival = 10,
    transfer_out = "No"
  )
  expect_error(
    traumar::seqic_indicator_7(
      df,
      trauma_level,
      id,
      time_to_arrival,
      transfer_out
    ),
    "level"
  )
})

test_that("Errors are thrown for invalid `unique_incident_id` type", {
  df <- tibble::tibble(
    id = list("a"),
    trauma_level = "I",
    time_to_arrival = 10,
    transfer_out = "No"
  )
  expect_error(
    traumar::seqic_indicator_7(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      time_from_injury_to_arrival = time_to_arrival,
      transfer_out_indicator = transfer_out
    ),
    "unique_incident_id"
  )
})

test_that("Errors are thrown for invalid `transfer_out_indicator` type", {
  df <- tibble::tibble(
    id = 1,
    trauma_level = "I",
    time_to_arrival = 10,
    transfer_out = list(TRUE)
  )
  expect_error(
    traumar::seqic_indicator_7(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      time_from_injury_to_arrival = time_to_arrival,
      transfer_out_indicator = transfer_out
    ),
    "transfer_out_indicator"
  )
})

test_that("Errors are thrown for non-numeric `time_from_injury_to_arrival`", {
  df <- tibble::tibble(
    id = 1,
    trauma_level = "I",
    time_to_arrival = "late",
    transfer_out = "No"
  )
  expect_error(
    traumar::seqic_indicator_7(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      time_from_injury_to_arrival = time_to_arrival,
      transfer_out_indicator = transfer_out
    ),
    "time_from_injury_to_arrival"
  )
})

test_that("Invalid group column throws error", {
  df <- tibble::tibble(
    id = 1,
    trauma_level = "I",
    time_to_arrival = 10,
    transfer_out = "No"
  )
  expect_error(
    traumar::seqic_indicator_7(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      time_from_injury_to_arrival = time_to_arrival,
      transfer_out_indicator = transfer_out,
      groups = "not_a_column"
    ),
    "not valid columns"
  )
})

test_that("Invalid calculate_ci argument throws error", {
  df <- tibble::tibble(
    id = 1,
    trauma_level = "I",
    time_to_arrival = 10,
    transfer_out = "No"
  )
  expect_error(
    traumar::seqic_indicator_7(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      time_from_injury_to_arrival = time_to_arrival,
      transfer_out_indicator = transfer_out,
      calculate_ci = "bad_method"
    ),
    "calculate_ci"
  )
})

test_that("Invalid included_levels throws error", {
  df <- tibble::tibble(
    id = 1,
    trauma_level = "I",
    time_to_arrival = 10,
    transfer_out = "No"
  )
  expect_error(
    traumar::seqic_indicator_7(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      time_from_injury_to_arrival = time_to_arrival,
      transfer_out_indicator = transfer_out,
      included_levels = list("I")
    ),
    "included_levels"
  )
})
