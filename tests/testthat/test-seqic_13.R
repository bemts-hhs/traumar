# tests/testthat/test-seqic_indicator_13.R

testthat::test_that("seqic_indicator_13() correctly expects columns to be in the 'data'", {
  # Minimal valid data
  data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "II", "III", "IV", "I", "II"),
    validity = c(90, 85, 75, 92, 86, 70)
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = TRUE,
      unique_incident_id = id,
      validity_score = validity,
      validity_threshold = 85
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = trauma_level,
      included_levels = logical(length = 6),
      unique_incident_id = id,
      validity_score = validity,
      validity_threshold = 85
    ),
    regexp = "included_levels.*must be of class.*character.*factor.*numeric"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = fake,
      unique_incident_id = id,
      validity_score = validity,
      validity_threshold = 85
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = trauma_level,
      unique_incident_id = "faked",
      validity_score = validity,
      validity_threshold = 85
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = trauma_level,
      unique_incident_id = id,
      validity_score = a_fake,
      validity_threshold = 85
    ),
    regexp = "It was not possible to validate"
  )
})

testthat::test_that("seqic_indicator_13: input validation", {
  data <- tibble::tibble(
    id = 1:4,
    trauma_level = c("I", "II", "III", "IV"),
    validity = c(90, 85, 75, 92)
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = "not a data",
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity
    ),
    "must be a data frame"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data |> dplyr::mutate(trauma_level = 1:4),
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity
    ),
    "must be character or factor"
  )

  bad_id <- data |> dplyr::mutate(id = c(T, T, F, F))
  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = bad_id,
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity
    ),
    "must be of class"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data |> dplyr::mutate(validity = as.character(validity)),
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity
    ),
    "must be of class"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data |> dplyr::mutate(validity = c(90, -1, 105, 92)),
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity
    ),
    "must have a range"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity,
      validity_threshold = "high"
    ),
    "must be of class"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity,
      groups = 123
    ),
    "must be strings"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity,
      groups = c("nope")
    ),
    "Invalid grouping variable"
  )

  testthat::expect_error(
    traumar::seqic_indicator_13(
      data = data,
      level = trauma_level,
      unique_incident_id = id,
      validity_score = validity,
      calculate_ci = "bogus"
    ),
    "must be"
  )
})

testthat::test_that("seqic_indicator_13: correct computation", {
  data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "II", "III", "IV", "I", "II"),
    validity = c(90, 85, 75, 92, 86, 70)
  )

  out <- traumar::seqic_indicator_13(
    data = data,
    level = trauma_level,
    unique_incident_id = id,
    validity_score = validity,
    validity_threshold = 85
  )

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$numerator_13, 4)
  testthat::expect_equal(out$denominator_13, 6)
  testthat::expect_equal(out$seqic_13, 4 / 6)
  testthat::expect_equal(out$data, "population/sample")
})

testthat::test_that("seqic_indicator_13: grouping works", {
  data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "I", "II", "II", "III", "III"),
    region = c("A", "A", "A", "B", "B", "B"),
    validity = c(90, 84, 88, 70, 95, 60)
  )

  out <- traumar::seqic_indicator_13(
    data = data,
    level = trauma_level,
    unique_incident_id = id,
    validity_score = validity,
    groups = "region"
  )

  testthat::expect_equal(nrow(out), 2)
  testthat::expect_equal(
    out |> dplyr::filter(region == "A") |> dplyr::pull(numerator_13),
    2
  )
  testthat::expect_equal(
    out |> dplyr::filter(region == "A") |> dplyr::pull(denominator_13),
    3
  )
})

testthat::test_that("seqic_indicator_13: CI calculation works", {
  data <- tibble::tibble(
    id = 1:10,
    trauma_level = rep("I", 10),
    validity = c(rep(90, 7), rep(70, 3))
  )

  out <- traumar::seqic_indicator_13(
    data = data,
    level = trauma_level,
    unique_incident_id = id,
    validity_score = validity,
    calculate_ci = "wilson"
  )

  testthat::expect_true("lower_ci_13" %in% names(out))
  testthat::expect_true("upper_ci_13" %in% names(out))
  testthat::expect_true(out$lower_ci_13 < out$seqic_13)
  testthat::expect_true(out$upper_ci_13 > out$seqic_13)
})

testthat::test_that("seqic_indicator_13: zero denominator handled", {
  data <- tibble::tibble(
    id = 1:3,
    trauma_level = c("V", "V", "V"),
    validity = c(90, 95, 88)
  )

  out <- traumar::seqic_indicator_13(
    data = data,
    level = trauma_level,
    unique_incident_id = id,
    validity_score = validity
  )

  testthat::expect_equal(out$denominator_13, 0)
  testthat::expect_true(is.na(out$seqic_13))
})

testthat::test_that("seqic_indicator_13: deduplication works", {
  data <- tibble::tibble(
    id = c(1, 1, 2, 3, 4),
    trauma_level = c("I", "I", "I", "I", "I"),
    validity = c(90, 90, 80, 85, 95)
  )

  out <- traumar::seqic_indicator_13(
    data = data,
    level = trauma_level,
    unique_incident_id = id,
    validity_score = validity
  )

  testthat::expect_equal(out$denominator_13, 4) # unique ids only
})
