# tests/testthat/test-seqic_indicator_12.R

testthat::test_that("seqic_indicator_12() correctly expects columns to be in the 'data'", {
  # Minimal valid data
  data <- tibble::tibble(
    id = 1:4,
    trauma_level = c("I", "II", "III", "IV"),
    facility = c("A", "B", "C", "D"),
    data_entry_delay = c(10, 20, 80, 70)
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = TRUE,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay,
      data_entry_standard = 60
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = fake,
      unique_incident_id = id,
      data_entry_time = data_entry_delay,
      data_entry_standard = 60
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = not_a_column,
      data_entry_time = data_entry_delay,
      data_entry_standard = 60
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = something,
      data_entry_standard = 60
    ),
    regexp = "It was not possible to validate"
  )
})

testthat::test_that("seqic_indicator_12: input validation", {
  data <- tibble::tibble(
    id = 1:3,
    trauma_level = c("I", "II", "III"),
    facility = c("A", "B", "C"),
    data_entry_delay = c(10, 20, 30)
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = "not a data",
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay
    ),
    "data.*must be of class.*data\\.frame, tbl, tbl_df"
  )

  bad_id <- data |> dplyr::mutate(id = logical(3))
  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = bad_id,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay
    ),
    "unique_incident_id.*must be of class.*numeric.*integer.*character.*factor"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay,
      data_entry_standard = "60"
    ),
    "data_entry_standard.*must be.*numeric"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay,
      groups = 123
    ),
    "groups.*must be of class.*character.*factor"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay,
      groups = "group"
    ),
    "groups.*contains invalid column names.*group.*Valid column names are.*id, trauma_level, facility, data_entry_delay"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      included_levels = c(T, F, NA),
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay
    ),
    "included_levels.*must be of class.*numeric, character, factor, integer"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay,
      exclude_facility_list = c(T, F, NA)
    ),
    "exclude_facility_list.*must be of class.*numeric, character, factor"
  )

  bad_data <- data |> dplyr::mutate(trauma_level = as.numeric(1:3))
  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = bad_data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay
    ),
    "level.*must be of class.*character.*factor"
  )

  bad_data2 <- data |> dplyr::mutate(facility = list(1, 2, 3))
  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = bad_data2,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay
    ),
    "must be of class"
  )

  bad_delay <- data |> dplyr::mutate(data_entry_delay = character(3))
  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = bad_delay,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay
    ),
    "data_entry_time.*must be.*numeric"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay,
      groups = c("nonexistent")
    ),
    "groups.*contains invalid column names.*nonexistent.*Valid column names are.*id, trauma_level, facility, data_entry_delay"
  )

  testthat::expect_error(
    traumar::seqic_indicator_12(
      data = data,
      level = trauma_level,
      facility_id = facility,
      unique_incident_id = id,
      data_entry_time = data_entry_delay,
      calculate_ci = "fake"
    ),
    "calculate_ci.*contains invalid values.*fake.*Valid values are.*wilson, clopper-pearson"
  )
})

testthat::test_that("seqic_indicator_12: basic calculation", {
  data <- tibble::tibble(
    id = 1:4,
    trauma_level = c("I", "II", "III", "IV"),
    facility = c("A", "B", "C", "D"),
    data_entry_delay = c(10, 20, 80, 70)
  )

  res <- traumar::seqic_indicator_12(
    data = data,
    level = trauma_level,
    facility_id = facility,
    unique_incident_id = id,
    data_entry_time = data_entry_delay,
    data_entry_standard = 60
  )

  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$numerator_12, 2)
  testthat::expect_equal(res$denominator_12, 4)
  testthat::expect_equal(res$seqic_12, 0.5)
})

testthat::test_that("seqic_indicator_12: excluded facilities are removed", {
  data <- tibble::tibble(
    id = 1:4,
    trauma_level = c("I", "II", "III", "IV"),
    facility = c("A", "B", "C", "D"),
    data_entry_delay = c(10, 20, 80, 70)
  )

  res <- traumar::seqic_indicator_12(
    data = data,
    level = trauma_level,
    facility_id = facility,
    unique_incident_id = id,
    exclude_facility_list = c("D"),
    data_entry_time = data_entry_delay,
    data_entry_standard = 60
  )

  testthat::expect_equal(res$numerator_12, 2)
  testthat::expect_equal(res$denominator_12, 3)
})

testthat::test_that("seqic_indicator_12: works with grouping", {
  data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "I", "II", "II", "III", "III"),
    facility = c("X", "X", "Y", "Y", "Z", "Z"),
    data_entry_delay = c(10, 90, 30, 70, 40, 65),
    region = c("A", "A", "A", "A", "B", "B")
  )

  res <- traumar::seqic_indicator_12(
    data = data,
    level = trauma_level,
    facility_id = facility,
    unique_incident_id = id,
    data_entry_time = data_entry_delay,
    groups = "region",
    data_entry_standard = 60
  )

  testthat::expect_equal(nrow(res), 2)
  testthat::expect_equal(
    res |> dplyr::filter(region == "A") |> dplyr::pull(seqic_12),
    0.5
  )
  testthat::expect_equal(
    res |> dplyr::filter(region == "B") |> dplyr::pull(seqic_12),
    0.5
  )
})

testthat::test_that("seqic_indicator_12: confidence intervals are added", {
  data <- tibble::tibble(
    id = 1:10,
    trauma_level = rep("I", 10),
    facility = rep("A", 10),
    data_entry_delay = c(rep(20, 6), rep(70, 4))
  )

  res <- traumar::seqic_indicator_12(
    data = data,
    level = trauma_level,
    facility_id = facility,
    unique_incident_id = id,
    data_entry_time = data_entry_delay,
    calculate_ci = "wilson"
  )

  testthat::expect_true("lower_ci_12" %in% names(res))
  testthat::expect_true("upper_ci_12" %in% names(res))
  testthat::expect_true(res$lower_ci_12 < res$seqic_12)
  testthat::expect_true(res$upper_ci_12 > res$seqic_12)
})
