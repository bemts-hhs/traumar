# tests/testthat/test-seqic_indicator_11.R

testthat::test_that("seqic_indicator_11() correctly expects columns to be in the 'data'", {
  # Minimal valid data
  data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "II", "III", "IV", "II", "III"),
    transferred_out = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    received = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    iss = c(4, 8, 10, 6, 5, 3),
    ed_LOS = c(8, 22, 12, 5, 7, 3),
    region = c("East", "West", "East", "East", "West", "West")
  )

  testthat::expect_error(
    traumar::seqic_indicator_11(
      data = data,
      level = fake,
      included_levels = c("I", "II", "III", "IV"),
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      unique_incident_id = id,
      iss = iss,
      ed_LOS = ed_LOS,
      groups = "region"
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_11(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      transfer_out_indicator = blah,
      receiving_indicator = received,
      unique_incident_id = id,
      iss = iss,
      ed_LOS = ed_LOS,
      groups = "region"
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_11(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      transfer_out_indicator = transferred_out,
      receiving_indicator = error,
      unique_incident_id = id,
      iss = iss,
      ed_LOS = ed_LOS,
      groups = "region"
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_11(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      unique_incident_id = TRUE,
      iss = iss,
      ed_LOS = ed_LOS,
      groups = "region"
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_11(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      unique_incident_id = id,
      iss = FALSE,
      ed_LOS = ed_LOS,
      groups = "region"
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_11(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      unique_incident_id = id,
      iss = iss,
      ed_LOS = length_of_stay,
      groups = "region"
    ),
    regexp = "It was not possible to validate"
  )
})

testthat::test_that("seqic_indicator_11 works with minimal valid input", {
  data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "II", "III", "IV", "II", "III"),
    transferred_out = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    received = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    iss = c(4, 8, 10, 6, 5, 3),
    ed_LOS = c(8, 22, 12, 5, 7, 3),
    region = c("East", "West", "East", "East", "West", "West")
  )

  res <- traumar::seqic_indicator_11(
    data = data,
    level = trauma_level,
    included_levels = c("I", "II", "III", "IV"),
    transfer_out_indicator = transferred_out,
    receiving_indicator = received,
    unique_incident_id = id,
    iss = iss,
    ed_LOS = ed_LOS,
    groups = "region"
  )

  testthat::expect_s3_class(res, "tbl_df")

  expected_columns <- c("numerator_11", "denominator_11", "seqic_11")
  testthat::expect_true(all(expected_columns %in% names(res)))
})

testthat::test_that("seqic_indicator_11 correctly validates columns", {
  data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "II", "III", "IV", "II", "III"),
    transferred_out = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    received = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    iss = c(4, 8, 10, 6, 5, 3),
    ed_LOS = c(8, 22, 12, 5, 7, 3),
    region = c("East", "West", "East", "East", "West", "West")
  )

  testthat::expect_error(seqic_indicator_11(data = "not_a_data"))

  bad_level <- data |> dplyr::mutate(trauma_level = 1:6)
  testthat::expect_error(
    seqic_indicator_11(
      data = bad_level,
      unique_incident_id = id,
      level = trauma_level,
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS
    ),
    "level.*must be character or factor"
  )

  bad_id <- data |> dplyr::mutate(id = rep(TRUE, 6))
  testthat::expect_error(
    seqic_indicator_11(
      data = bad_id,
      unique_incident_id = id,
      level = trauma_level,
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS
    ),
    "unique_incident_id.*must be of class.*character.*numeric.*factor"
  )

  bad_transfer_indicator <- data |>
    dplyr::mutate(transferred_out = numeric(length = 6))
  testthat::expect_error(
    seqic_indicator_11(
      data = bad_transfer_indicator,
      unique_incident_id = id,
      level = trauma_level,
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS
    ),
    "transfer_out_indicator.*must be of class.*character.*factor.*logical"
  )

  bad_receiving_indicator <- data |>
    dplyr::mutate(received = numeric(length = 6))
  testthat::expect_error(
    seqic_indicator_11(
      data = bad_receiving_indicator,
      unique_incident_id = id,
      level = trauma_level,
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS
    ),
    "receiving_indicator.*must be of class.*character.*factor.*logical"
  )

  bad_los <- data |>
    dplyr::mutate(ed_LOS = character(length = 6))
  testthat::expect_error(
    seqic_indicator_11(
      data = bad_los,
      unique_incident_id = id,
      level = trauma_level,
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS
    ),
    "ed_LOS.*must be of class.*numeric"
  )

  testthat::expect_error(
    seqic_indicator_11(
      data = data,
      unique_incident_id = id,
      level = trauma_level,
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS,
      groups = 1
    ),
    "You passed an object of class.*numeric"
  )

  testthat::expect_error(
    seqic_indicator_11(
      data = data,
      unique_incident_id = id,
      level = trauma_level,
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS,
      groups = "group"
    ),
    "Invalid grouping variable\\(s\\)"
  )

  testthat::expect_error(
    seqic_indicator_11(
      data = data,
      unique_incident_id = id,
      level = trauma_level,
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS,
      calculate_ci = "z"
    ),
    "is not NULL, it must be"
  )

  testthat::expect_error(
    seqic_indicator_11(
      data = data,
      unique_incident_id = id,
      level = trauma_level,
      included_levels = c(T, F, NA),
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      iss = iss,
      ed_LOS = ed_LOS
    ),
    "must be of class.*character.*factor.*numeric"
  )
})

testthat::test_that("seqic_indicator_11 includes CI columns when calculate_ci is specified", {
  data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "II", "III", "IV", "II", "III"),
    transferred_out = rep(FALSE, 6),
    received = rep(TRUE, 6),
    iss = c(4, 5, 3, 8, 7, 6),
    ed_LOS = c(5, 6, 7, 20, 21, 8)
  )

  res <- traumar::seqic_indicator_11(
    data = data,
    level = trauma_level,
    included_levels = c("I", "II", "III", "IV"),
    transfer_out_indicator = transferred_out,
    receiving_indicator = received,
    unique_incident_id = id,
    iss = iss,
    ed_LOS = ed_LOS,
    calculate_ci = "clopper-pearson"
  )

  testthat::expect_true(all(c("lower_ci_11", "upper_ci_11") %in% names(res)))
})

testthat::test_that("seqic_indicator_11 fails with both invalid and missing arguments", {
  data <- tibble::tibble(
    id = 1:3,
    trauma_level = c("I", "II", "III"),
    transferred_out = c(FALSE, FALSE, FALSE),
    received = c(TRUE, TRUE, TRUE),
    iss = c("low", "medium", "high"), # invalid
    ed_LOS = c(5, 6, 7)
  )

  testthat::expect_error(
    traumar::seqic_indicator_11(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      transfer_out_indicator = transferred_out,
      receiving_indicator = received,
      unique_incident_id = id,
      iss = iss,
      ed_LOS = ed_LOS
    ),
    "must be numeric"
  )
})

testthat::test_that("seqic_indicator_11 correctly filters and deduplicates input", {
  data <- tibble::tibble(
    id = c(1, 1, 2, 3),
    trauma_level = c("I", "I", "II", "III"),
    transferred_out = c(FALSE, FALSE, FALSE, TRUE),
    received = c(TRUE, TRUE, TRUE, TRUE),
    iss = c(4, 4, 8, 6),
    ed_LOS = c(10, 10, 15, 20)
  )

  res <- traumar::seqic_indicator_11(
    data = data,
    level = trauma_level,
    included_levels = c("I", "II", "III"),
    transfer_out_indicator = transferred_out,
    receiving_indicator = received,
    unique_incident_id = id,
    iss = iss,
    ed_LOS = ed_LOS
  )

  testthat::expect_equal(res$denominator_11, 2) # One duplicated ID, one transfer out excluded
})

testthat::test_that("seqic_indicator_11 calculates correct numerator and denominator", {
  data <- tibble::tibble(
    id = 1:4,
    trauma_level = c("I", "II", "II", "III"),
    transferred_out = c(FALSE, FALSE, FALSE, FALSE),
    received = c(TRUE, TRUE, TRUE, TRUE),
    iss = c(5, 6, 10, 8),
    ed_LOS = c(10, 120, 30, 1441)
  )

  res <- traumar::seqic_indicator_11(
    data = data,
    level = trauma_level,
    included_levels = c("I", "II", "III"),
    transfer_out_indicator = transferred_out,
    receiving_indicator = received,
    unique_incident_id = id,
    iss = iss,
    ed_LOS = ed_LOS
  )

  testthat::expect_equal(res$numerator_11, 2) # Two patients with ISS<9 and LOS<1440
  testthat::expect_equal(res$denominator_11, 4)
  testthat::expect_equal(res$seqic_11, 0.5)
})

testthat::test_that("seqic_indicator_11 appends CI columns when requested", {
  data <- tibble::tibble(
    id = 1:10,
    trauma_level = rep("I", 10),
    transferred_out = rep(FALSE, 10),
    received = rep(TRUE, 10),
    iss = c(3, 4, 5, 6, 7, 10, 12, 4, 5, 8),
    ed_LOS = rep(120, 10)
  )

  res <- traumar::seqic_indicator_11(
    data = data,
    level = trauma_level,
    included_levels = "I",
    transfer_out_indicator = transferred_out,
    receiving_indicator = received,
    unique_incident_id = id,
    iss = iss,
    ed_LOS = ed_LOS,
    calculate_ci = "wilson"
  )

  testthat::expect_true(all(c("lower_ci_11", "upper_ci_11") %in% names(res)))
})

testthat::test_that("seqic_indicator_11 includes 'population/sample' label if no grouping", {
  data <- tibble::tibble(
    id = 1:3,
    trauma_level = c("I", "II", "III"),
    transferred_out = rep(FALSE, 3),
    received = rep(TRUE, 3),
    iss = c(4, 6, 5),
    ed_LOS = c(12, 14, 16)
  )

  res <- traumar::seqic_indicator_11(
    data = data,
    level = trauma_level,
    included_levels = c("I", "II", "III"),
    transfer_out_indicator = transferred_out,
    receiving_indicator = received,
    unique_incident_id = id,
    iss = iss,
    ed_LOS = ed_LOS
  )

  testthat::expect_true("data" %in% names(res))
  testthat::expect_equal(res$data, "population/sample")
})

testthat::test_that("seqic_indicator_11 orders by group variable if provided", {
  data <- tibble::tibble(
    id = 1:4,
    trauma_level = c("II", "II", "I", "III"),
    transferred_out = rep(FALSE, 4),
    received = rep(TRUE, 4),
    iss = c(4, 4, 4, 4),
    ed_LOS = c(10, 20, 15, 10),
    region = c("Z", "A", "M", "B")
  )

  res <- traumar::seqic_indicator_11(
    data = data,
    level = trauma_level,
    included_levels = c("I", "II", "III"),
    transfer_out_indicator = transferred_out,
    receiving_indicator = received,
    unique_incident_id = id,
    iss = iss,
    ed_LOS = ed_LOS,
    groups = "region"
  )

  testthat::expect_equal(res$region, sort(data$region))
})
