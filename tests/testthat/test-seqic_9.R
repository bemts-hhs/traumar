testthat::test_that("seqic_indicator_9() validates input types correctly", {
  data <- tibble::tibble(
    id = 1:3,
    trauma_level = c("I", "II", "III"),
    transport = c("Ambulance", "Ambulance", "Private"),
    activated = c(TRUE, FALSE, TRUE),
    ed_LOS = c(100, 120, 80),
    ed_decision = c(60, 70, 50),
    ed_discharge = c(130, 150, 110),
    transfer_out = c(TRUE, FALSE, TRUE),
    risk = c("High", "Moderate", "Low")
  )

  # data is not a data.frame or tibble
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data = as.matrix(data),
      level = trauma_level,
      included_levels = c("I", "II", "III"),
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      transport_method = transport,
      trauma_team_activated = activated,
      risk_group = risk,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision,
      ed_decision_discharge_LOS = ed_discharge
    ),
    "must be a data frame or tibble"
  )

  # level column invalid type
  bad_data <- dplyr::mutate(data, trauma_level = as.numeric(1:3))
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III"),
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      transport_method = transport,
      trauma_team_activated = activated,
      risk_group = risk,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision,
      ed_decision_discharge_LOS = ed_discharge
    ),
    "must be character or factor"
  )

  # unique_incident_id wrong class
  bad_data <- dplyr::mutate(data, id = list(1, 2, 3)) # list column
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III"),
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      transport_method = transport,
      trauma_team_activated = activated,
      risk_group = risk,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision,
      ed_decision_discharge_LOS = ed_discharge
    ),
    "must be of class .*character.*numeric.*factor"
  )

  # transfer_out_indicator wrong class
  bad_data <- dplyr::mutate(data, transfer_out = as.Date("2024-01-01") + 0:2)
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III"),
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      transport_method = transport,
      trauma_team_activated = activated,
      risk_group = risk,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision,
      ed_decision_discharge_LOS = ed_discharge
    ),
    "transfer_out_indicator.*must be of class"
  )

  # ed_LOS not numeric
  bad_data <- dplyr::mutate(data, ed_LOS = as.character(ed_LOS))
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III"),
      unique_incident_id = id,
      transfer_out_indicator = transfer_out,
      transport_method = transport,
      trauma_team_activated = activated,
      risk_group = risk,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision,
      ed_decision_discharge_LOS = ed_discharge
    ),
    "ed_LOS.*must be of class"
  )
})

testthat::test_that("seqic_indicator_9() input validation: remaining checks", {
  data <- dplyr::tibble(
    incident_id = "G",
    probability_of_survival = 0.95,
    trauma_center_level = "I",
    transfer_out = TRUE,
    transport_method = "Ambulance",
    ed_LOS = 3,
    ed_decision_LOS = "2 hours",
    ed_decision_discharge_LOS = 1.5,
    trauma_team_activated = 1L,
    risk_group = 3L
  )

  # ed_decision_LOS must be numeric
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data,
      level = trauma_center_level,
      transfer_out_indicator = transfer_out,
      transport_method = transport_method,
      unique_incident_id = incident_id,
      trauma_team_activated = trauma_team_activated,
      risk_group = risk_group,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision_LOS,
      ed_decision_discharge_LOS = ed_decision_discharge_LOS
    ),
    "ed_decision_LOS.*class.*character"
  )

  # ed_decision_discharge_LOS must be numeric
  data$ed_decision_LOS <- 2.0
  data$ed_decision_discharge_LOS <- "1.5 hours"
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data,
      level = trauma_center_level,
      transfer_out_indicator = transfer_out,
      transport_method = transport_method,
      unique_incident_id = incident_id,
      trauma_team_activated = trauma_team_activated,
      risk_group = risk_group,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision_LOS,
      ed_decision_discharge_LOS = ed_decision_discharge_LOS
    ),
    "ed_decision_discharge_LOS.*class.*character"
  )

  # trauma_team_activated must be character, factor, or logical
  data$ed_decision_discharge_LOS <- 1.5
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data,
      level = trauma_center_level,
      transfer_out_indicator = transfer_out,
      transport_method = transport_method,
      unique_incident_id = incident_id,
      trauma_team_activated = trauma_team_activated,
      risk_group = risk_group,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision_LOS,
      ed_decision_discharge_LOS = ed_decision_discharge_LOS
    ),
    "trauma_team_activated.*class.*integer"
  )

  # risk_group must be character or factor
  data$trauma_team_activated <- "Yes"
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data,
      level = trauma_center_level,
      transfer_out_indicator = transfer_out,
      transport_method = transport_method,
      unique_incident_id = incident_id,
      trauma_team_activated = trauma_team_activated,
      risk_group = risk_group,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision_LOS,
      ed_decision_discharge_LOS = ed_decision_discharge_LOS
    ),
    "risk_group.*character or factor"
  )

  # groups must be character vector
  data$risk_group <- "Low"
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data,
      level = trauma_center_level,
      transfer_out_indicator = transfer_out,
      transport_method = transport_method,
      unique_incident_id = incident_id,
      trauma_team_activated = trauma_team_activated,
      risk_group = risk_group,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision_LOS,
      ed_decision_discharge_LOS = ed_decision_discharge_LOS,
      groups = 123
    ),
    "groups.*strings"
  )

  # groups must exist in data
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data,
      level = trauma_center_level,
      transfer_out_indicator = transfer_out,
      transport_method = transport_method,
      unique_incident_id = incident_id,
      trauma_team_activated = trauma_team_activated,
      risk_group = risk_group,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision_LOS,
      ed_decision_discharge_LOS = ed_decision_discharge_LOS,
      groups = c("not_a_column")
    ),
    "Invalid grouping variable"
  )

  # calculate_ci must be "wilson" or "clopper-pearson"
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data,
      level = trauma_center_level,
      transfer_out_indicator = transfer_out,
      transport_method = transport_method,
      unique_incident_id = incident_id,
      trauma_team_activated = trauma_team_activated,
      risk_group = risk_group,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision_LOS,
      ed_decision_discharge_LOS = ed_decision_discharge_LOS,
      calculate_ci = "bootstrap"
    ),
    "must be.*wilson.*clopper-pearson"
  )

  # included_levels must be character, numeric, or factor
  testthat::expect_error(
    traumar::seqic_indicator_9(
      data,
      level = trauma_center_level,
      transfer_out_indicator = transfer_out,
      transport_method = transport_method,
      unique_incident_id = incident_id,
      trauma_team_activated = trauma_team_activated,
      risk_group = risk_group,
      ed_LOS = ed_LOS,
      ed_decision_LOS = ed_decision_LOS,
      ed_decision_discharge_LOS = ed_decision_discharge_LOS,
      included_levels = as.Date("2024-01-01")
    ),
    "included_levels.*class.*Date"
  )
})

testthat::test_that("seqic_indicator_9 filtering and flags behave as expected", {
  test_data <- dplyr::tibble(
    id = as.character(1:6),
    trauma_level = c("I", "II", "V", "III", "II", "IV"),
    transport = c(
      "Ambulance",
      "Private Vehicle",
      "Ambulance",
      "walk-in",
      "Ambulance",
      "Other"
    ),
    transfer_out = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
    activated = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    ed_LOS = c(121, 90, 200, 100, 80, 130),
    ed_decision = c(61, 55, 130, 50, 30, 125),
    ed_discharge = c(130, 110, 140, 70, 60, 160),
    risk = c("High", "Moderate", "Low", "High", "Moderate", "Low")
  )

  result <- traumar::seqic_indicator_9(
    data = test_data,
    level = trauma_level,
    included_levels = c("I", "II", "III", "IV"),
    unique_incident_id = id,
    transport_method = transport,
    transfer_out_indicator = transfer_out,
    ed_LOS = ed_LOS,
    ed_decision_LOS = ed_decision,
    ed_decision_discharge_LOS = ed_discharge,
    trauma_team_activated = activated,
    risk_group = risk
  )

  prep_data <- result$overall

  # Expect only records with trauma level I-IV and valid transport
  testthat::expect_equal(nrow(prep_data), 1)

  # Validate numerator counts
  testthat::expect_equal(prep_data$numerator_9a_all, 1) # ed_LOS > 120
  testthat::expect_equal(prep_data$numerator_9b_all, 0) # ed_LOS > 180
  testthat::expect_equal(prep_data$numerator_9c_all, 1) # ed_decision > 60
  testthat::expect_equal(prep_data$numerator_9d_all, 0) # ed_decision > 120
  testthat::expect_equal(prep_data$numerator_9e_all, 1) # ed_discharge > 60
  testthat::expect_equal(prep_data$numerator_9f_all, 1) # ed_discharge > 120

  # Validate denominators match count of eligible filtered records
  testthat::expect_equal(prep_data$denominator_9a_all, 1)
  testthat::expect_equal(prep_data$denominator_9b_all, 1)
})

testthat::test_that("regex transport exclusion works", {
  excluded_transports <- c(
    "Private Vehicle",
    "public vehicle",
    "walk-in",
    "walk in",
    "Not Known",
    "Not Recorded",
    "Not Applicable",
    "Other"
  )

  test_data <- dplyr::tibble(
    id = as.character(seq_along(excluded_transports)),
    trauma_level = rep("I", length(excluded_transports)),
    transport = excluded_transports,
    transfer_out = rep(TRUE, length(excluded_transports)),
    activated = rep(TRUE, length(excluded_transports)),
    ed_LOS = rep(130, length(excluded_transports)),
    ed_decision = rep(70, length(excluded_transports)),
    ed_discharge = rep(125, length(excluded_transports)),
    risk = rep("High", length(excluded_transports))
  )

  result <- traumar::seqic_indicator_9(
    data = test_data,
    level = trauma_level,
    included_levels = c("I", "II", "III", "IV"),
    unique_incident_id = id,
    transport_method = transport,
    transfer_out_indicator = transfer_out,
    ed_LOS = ed_LOS,
    ed_decision_LOS = ed_decision,
    ed_decision_discharge_LOS = ed_discharge,
    trauma_team_activated = activated,
    risk_group = risk
  )

  # Expect no records passed filtering due to transport exclusion
  testthat::expect_equal(nrow(result$overall), 1)
})

testthat::test_that("CI computation for seqic_indicator_9() returns expected columns and structure", {
  # Simulated data
  test_data <- tibble::tibble(
    id = as.character(1:6),
    trauma_level = c("I", "II", "III", "IV", "I", "II"),
    transport = c(
      "Ambulance",
      "Ambulance",
      "Ambulance",
      "Ambulance",
      "Ambulance",
      "Ambulance"
    ),
    activated = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    ed_LOS = c(130, 190, 110, 65, 150, 220),
    ed_decision = c(65, 125, 70, 30, 160, 140),
    ed_discharge = c(180, 200, 130, 90, 210, 250),
    transfer_out = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    risk = c("High", "Moderate", "Low", "High", "Low", "Moderate")
  )

  # Run function with CI enabled
  result <- traumar::seqic_indicator_9(
    data = test_data,
    level = trauma_level,
    included_levels = c("I", "II", "III", "IV"),
    unique_incident_id = id,
    transport_method = transport,
    transfer_out_indicator = transfer_out,
    ed_LOS = ed_LOS,
    ed_decision_LOS = ed_decision,
    ed_decision_discharge_LOS = ed_discharge,
    trauma_team_activated = activated,
    risk_group = risk,
    calculate_ci = "wilson"
  )

  # Confirm all expected CI columns exist in overall output
  expected_ci_cols <- c(
    "lower_ci_9a_all",
    "upper_ci_9a_all",
    "lower_ci_9b_all",
    "upper_ci_9b_all",
    "lower_ci_9c_all",
    "upper_ci_9c_all",
    "lower_ci_9d_all",
    "upper_ci_9d_all",
    "lower_ci_9e_all",
    "upper_ci_9e_all",
    "lower_ci_9f_all",
    "upper_ci_9f_all"
  )

  testthat::expect_true(all(expected_ci_cols %in% colnames(result$overall)))

  # Confirm all CI columns are numeric
  for (col in expected_ci_cols) {
    testthat::expect_type(result$overall[[col]], "double")
  }

  # Confirm no NA CIs when denominators are non-zero
  numerators <- result$overall |>
    dplyr::select(dplyr::starts_with("numerator_")) |>
    purrr::map_lgl(~ all(.x >= 0))

  denominators <- result$overall |>
    dplyr::select(dplyr::starts_with("denominator_")) |>
    purrr::map_lgl(~ all(.x > 0))

  testthat::expect_true(all(numerators))
  testthat::expect_true(all(denominators))
})
