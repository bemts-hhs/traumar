test_that("Data validation for seqic_indicator_6 works correctly", {
  # Create a simple test data frame for validation
  test_data <- tibble::tibble(
    id = as.character(1:10),
    trauma_level = rep(c("I", "II", "III", "IV", "V"), times = 2),
    transfer_out = c(
      "No",
      "No",
      "Yes",
      "No",
      "No",
      "No",
      "No",
      "No",
      "No",
      "No"
    ),
    transfer_in = c(
      "Yes",
      "Yes",
      "No",
      "Yes",
      "No",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes"
    ),
    gcs_low = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    time_to_arrival = c(200, 100, 300, 190, 400, 181, 100, 179, 240, 178)
  )

  # Test: Valid inputs should pass without errors
  testthat::expect_silent(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival
    )
  )

  # Test: Invalid `data` (not a data frame or tibble)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = list(),
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival
    ),
    regexp = "must be of class.*data.frame.*tibble"
  )

  # Test: Invalid `level` (not character or factor)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = time_to_arrival, # Using `id` which is character, should be `trauma_level`
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival
    ),
    regexp = "must be of class.*character.*factor"
  )

  # Test: Invalid `unique_incident_id` (not character, factor, or numeric)
  bad_data <- test_data |> dplyr::mutate(id = logical(10))
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = bad_data,
      level = trauma_level,
      unique_incident_id = id, # Using `trauma_level`, which is factor, should be `id`
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival
    ),
    regexp = "must be of class.*character.*numeric.*factor"
  )

  # Test: Invalid `transfer_out_indicator` (not character, factor, or logical)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = time_to_arrival, # Using `gcs_low`, which is logical, should be `transfer_out`
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival
    ),
    regexp = "must be of class.*character.*factor.*logical"
  )

  # Test: Invalid `receiving_indicator` (not character, factor, or logical)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = time_to_arrival, # Using `gcs_low`, which is logical, should be `transfer_in`
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival
    ),
    regexp = "must be of class.*character.*factor.*logical"
  )

  # Test: Invalid `low_GCS_indicator` (not character, factor, or logical)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = time_to_arrival, # Using `time_to_arrival`, which is numeric, should be `gcs_low`
      time_from_injury_to_arrival = time_to_arrival
    ),
    regexp = "must be of class.*character.*factor.*logical"
  )

  # Test: Invalid `time_from_injury_to_arrival` (not numeric)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = gcs_low # Using `gcs_low`, which is logical, should be numeric
    ),
    regexp = "must be of class.*numeric"
  )

  # Test: Invalid `groups` (not a character vector)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival,
      groups = 123 # Invalid: groups must be a character vector
    ),
    regexp = "All elements in.*groups.*must be strings."
  )

  # Test: Invalid `groups` (columns not present in `data`)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival,
      groups = c("nonexistent_column") # Invalid: `nonexistent_column` does not exist in the data
    ),
    regexp = "are not valid columns in"
  )

  # Test: Invalid `calculate_ci` (not NULL, "wilson", or "clopper-pearson")
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival,
      calculate_ci = "invalid_ci" # Invalid: must be NULL, "wilson", or "clopper-pearson"
    ),
    regexp = "If.*calculate_ci.*is not.*NULL.*it must be.*wilson.*clopper-pearson"
  )

  # Test: Valid `included_levels` (should pass)
  testthat::expect_silent(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival,
      included_levels = c("I", "II")
    )
  )

  # Test: Invalid `included_levels` (not character, factor, or numeric)
  testthat::expect_error(
    traumar::seqic_indicator_6(
      data = test_data,
      level = trauma_level,
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      receiving_indicator = transfer_in,
      low_GCS_indicator = gcs_low,
      time_from_injury_to_arrival = time_to_arrival,
      included_levels = TRUE # Invalid: should be character, factor, or numeric
    ),
    regexp = ".*included_levels.*must be of class.*character.*factor.*numeric"
  )
})

testthat::test_that("seqic_indicator_6 calculates correct values without CI", {
  test_data <- tibble::tibble(
    id = as.character(1:10),
    trauma_level = rep(c("I", "II", "III", "IV", "V"), times = 2),
    transfer_out = c(
      "No",
      "No",
      "Yes",
      "No",
      "No",
      "No",
      "No",
      "No",
      "No",
      "No"
    ),
    transfer_in = c(
      "Yes",
      "Yes",
      "No",
      "Yes",
      "No",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes"
    ),
    gcs_low = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    time_to_arrival = c(200, 100, 300, 190, 400, 181, 100, 179, 240, 178)
  )

  result <- traumar::seqic_indicator_6(
    data = test_data,
    level = trauma_level,
    unique_incident_id = id,
    transfer_out_indicator = transfer_out,
    receiving_indicator = transfer_in,
    low_GCS_indicator = gcs_low,
    time_from_injury_to_arrival = time_to_arrival
  )

  testthat::expect_equal(result$numerator_6, 3)
  testthat::expect_equal(result$denominator_6, 6)
  testthat::expect_equal(result$seqic_6, 3 / 6)
})

testthat::test_that("seqic_indicator_6 computes CI and supports grouping", {
  test_data <- tibble::tibble(
    id = as.character(1:10),
    region = rep(c("East", "West"), each = 5),
    trauma_level = rep(c("I", "II", "III", "IV", "V"), times = 2),
    transfer_out = c(
      "No",
      "No",
      "Yes",
      "No",
      "No",
      "No",
      "No",
      "No",
      "No",
      "No"
    ),
    transfer_in = c(
      "Yes",
      "Yes",
      "No",
      "Yes",
      "No",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes"
    ),
    gcs_low = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    time_to_arrival = c(200, 100, 300, 190, 400, 181, 100, 179, 240, 178)
  )

  result_grouped <- traumar::seqic_indicator_6(
    data = test_data,
    level = trauma_level,
    unique_incident_id = id,
    transfer_out_indicator = transfer_out,
    receiving_indicator = transfer_in,
    low_GCS_indicator = gcs_low,
    time_from_injury_to_arrival = time_to_arrival,
    groups = "region",
    calculate_ci = "wilson"
  )

  # Check presence of CI columns
  testthat::expect_true(all(
    c("lower_ci_6", "upper_ci_6") %in% names(result_grouped)
  ))

  # Check output structure
  testthat::expect_true("region" %in% names(result_grouped))
  testthat::expect_equal(nrow(result_grouped), 2)

  # Validate CI bounds are within [0,1] and lower ≤ upper
  testthat::expect_true(all(dplyr::between(result_grouped$lower_ci_6, 0, 1)))
  testthat::expect_true(all(dplyr::between(result_grouped$upper_ci_6, 0, 1)))
  testthat::expect_true(all(
    result_grouped$lower_ci_6 <= result_grouped$upper_ci_6
  ))
})
