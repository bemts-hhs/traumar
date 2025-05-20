# tests/testthat/test-seqic_indicator_10.R

testthat::test_that("seqic_indicator_10() correctly expects columns to be in the 'data'", {
  # Minimal valid data
  test_data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "II", "II", "III", "IV", "II"),
    acute_transfer = rep("No", 6),
    activation = c("Level 1", "None", "Level 2", "Level 1", NA, "Consultation"),
    iss = c(20, 10, 25, 12, 18, 9),
    nfti = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    region = c("East", "West", "East", "West", "East", "West")
  )

  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = test_data,
      level = false,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = NULL,
      groups = "region",
      calculate_ci = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = test_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = other,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = NULL,
      groups = "region",
      calculate_ci = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = test_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = minor_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = NULL,
      groups = "region",
      calculate_ci = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = test_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = TRUE,
      iss = iss,
      nfti = NULL,
      groups = "region",
      calculate_ci = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = test_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = "faked",
      nfti = NULL,
      groups = "region",
      calculate_ci = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = test_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = NULL,
      nfti = FALSE,
      groups = "region",
      calculate_ci = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = test_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti,
      groups = "region",
      calculate_ci = NULL
    ),
    regexp = "Please supply exactly one of"
  )
})

test_that("data validation fails appropriately", {
  data <- tibble::tibble(
    id = as.character(1:3),
    trauma_level = c("I", "II", "III"),
    acute_transfer = rep("No", 3),
    activation = c("Level 1", "Level 2", "None"),
    iss = c(15, 22, 10),
    nfti = c(TRUE, FALSE, TRUE),
    region = c("East", "West", "East")
  )

  # Not a data frame
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = list(),
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti
    ),
    "must be a data frame or tibble"
  )

  # Level not character or factor
  bad_data <- data |> dplyr::mutate(trauma_level = as.numeric(1:3))
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti
    ),
    "must be character or factor"
  )

  # unique_incident_id wrong class
  bad_data <- data |> dplyr::mutate(id = as.Date("2023-01-01") + 0:2)
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti
    ),
    "must be of class"
  )

  # trauma_team_activation_level not character/factor
  bad_data <- data |> dplyr::mutate(activation = as.Date("2023-01-01") + 0:2)
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti
    ),
    "must be character or factor"
  )

  # trauma_team_activation_level not character/factor
  bad_data <- data |> dplyr::mutate(acute_transfer = numeric(length = 3))
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti
    ),
    "must be of class.*character.*factor.*logical"
  )

  # iss is not numeric
  bad_data <- data |> dplyr::mutate(iss = as.character(iss))
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti
    ),
    "must be numeric"
  )

  # nfti wrong class
  bad_data <- data |> dplyr::mutate(nfti = as.Date("2023-01-01") + 0:2)
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = bad_data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti
    ),
    "must be character, factor, or logical"
  )

  # groups not character
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti,
      groups = 1:2
    ),
    "must be strings"
  )

  # groups has invalid column names
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti,
      groups = c("region", "nonexistent_col")
    ),
    "Invalid grouping variable"
  )

  # calculate_ci invalid
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = data,
      level = trauma_level,
      included_levels = c("I", "II", "III", "IV"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti,
      calculate_ci = "banana"
    ),
    "must be.*wilson.*clopper-pearson"
  )

  # included_levels wrong type
  testthat::expect_error(
    traumar::seqic_indicator_10(
      data = data,
      level = trauma_level,
      included_levels = list("I", "II"),
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      trauma_team_activation_level = activation,
      iss = iss,
      nfti = nfti
    ),
    "must be of class"
  )
})

test_that("SEQIC Indicator 10a, 10b, 10c calculate correctly", {
  test_data <- tibble::tibble(
    id = 1:6,
    trauma_level = c("I", "II", "II", "III", "IV", "II"),
    acute_transfer = rep("No", 6),
    activation = c("Level 1", "None", "Level 2", "Level 1", NA, "Consultation"),
    iss = c(20, 10, 25, 12, 18, 9),
    region = c("East", "West", "East", "West", "East", "West")
  )

  result <- traumar::seqic_indicator_10(
    data = test_data,
    level = trauma_level,
    included_levels = c("I", "II", "III", "IV"),
    unique_incident_id = id,
    transfer_out_indicator = acute_transfer,
    trauma_team_activation_level = activation,
    iss = iss,
    nfti = NULL,
    groups = "region",
    calculate_ci = NULL
  )

  # Check 10a: under-triage (major trauma but limited/no activation)
  testthat::expect_named(
    result,
    c("seqic_10", "diagnostics")
  )
  testthat::expect_true(all(
    c(
      "numerator_10a",
      "denominator_10a",
      "seqic_10a"
    ) %in%
      names(result$seqic_10)
  ))

  testthat::expect_true(all(
    result$seqic_10$seqic_10a >= 0 & result$seqic_10$seqic_10a <= 1,
    na.rm = TRUE
  ))

  # Check 10b: over-triage (minor trauma with full activation)
  testthat::expect_true(all(
    c("numerator_10b", "denominator_10b", "seqic_10b") %in%
      names(result$seqic_10)
  ))
  testthat::expect_true(all(
    result$seqic_10$seqic_10b >= 0 & result$seqic_10$seqic_10b <= 1,
    na.rm = TRUE
  ))

  # Check 10c: under-triage (major trauma cases missed, Peng & Xiang)
  testthat::expect_true(all(
    c("numerator_10c", "denominator_10c", "seqic_10c") %in%
      names(result$seqic_10)
  ))
  testthat::expect_true(all(
    result$seqic_10$seqic_10c >= 0 & result$seqic_10$seqic_10c <= 1,
    na.rm = TRUE
  ))
})

test_that("Model diagnostic statistics are calculated correctly", {
  test_data <- tibble::tibble(
    id = 1:4,
    trauma_level = c("I", "II", "II", "IV"),
    acute_transfer = rep("No", 4),
    activation = c("Level 1", "None", "Level 1", "Consultation"),
    iss = c(20, 10, 8, 18),
    region = c("East", "East", "West", "West")
  )

  result <- traumar::seqic_indicator_10(
    data = test_data,
    level = trauma_level,
    included_levels = c("I", "II", "III", "IV"),
    unique_incident_id = id,
    transfer_out_indicator = acute_transfer,
    trauma_team_activation_level = activation,
    iss = iss,
    nfti = NULL,
    groups = "region",
    calculate_ci = NULL
  )

  diag <- result$diagnostics
  testthat::expect_true(all(
    c(
      "full_minor",
      "full_major",
      "limited_minor",
      "limited_major",
      "sensitivity",
      "specificity",
      "positive_predictive_value",
      "negative_predictive_value",
      "false_negative_rate",
      "false_positive_rate",
      "false_discovery_rate",
      "false_omission_rate"
    ) %in%
      names(diag)
  ))

  testthat::expect_true(all(
    diag$sensitivity >= 0 & diag$sensitivity <= 1,
    na.rm = TRUE
  ))
})

test_that("Function includes triage logic indicator", {
  test_data <- tibble::tibble(
    id = 1:2,
    trauma_level = c("I", "II"),
    acute_transfer = rep("No", 2),
    activation = c("Level 1", "None"),
    iss = c(20, 5)
  )

  res <- traumar::seqic_indicator_10(
    data = test_data,
    level = trauma_level,
    included_levels = c("I", "II", "III", "IV"),
    unique_incident_id = id,
    transfer_out_indicator = acute_transfer,
    trauma_team_activation_level = activation,
    iss = iss,
    nfti = NULL
  )

  testthat::expect_equal(res$seqic_10$triage_logic, "cribari")
})

test_that("Confidence intervals are returned when requested", {
  test_data <- tibble::tibble(
    id = 1:12,
    trauma_level = rep("I", 12),
    acute_transfer = rep("No", 12),
    activation = rep(c("Level 1", "Level 2", "None"), each = 4),
    iss = rep(c(20, 10, 25, 15), 3)
  )

  res <- traumar::seqic_indicator_10(
    data = test_data,
    level = trauma_level,
    included_levels = c("I"),
    unique_incident_id = id,
    transfer_out_indicator = acute_transfer,
    trauma_team_activation_level = activation,
    iss = iss,
    nfti = NULL,
    calculate_ci = "wilson"
  )

  testthat::expect_true("lower_ci_10a" %in% names(res$seqic_10))
  testthat::expect_true("upper_ci_10a" %in% names(res$seqic_10))

  testthat::expect_true("lower_ci_10b" %in% names(res$seqic_10))
  testthat::expect_true("upper_ci_10b" %in% names(res$seqic_10))

  testthat::expect_true("lower_ci_10c" %in% names(res$seqic_10))
  testthat::expect_true("upper_ci_10c" %in% names(res$seqic_10))
})

testthat::test_that("classification works with ISS only", {
  test_data <- tibble::tibble(
    id = 1:12,
    trauma_level = rep("I", 12),
    acute_transfer = rep("No", 12),
    activation = rep(c("Level 1", "Level 2", "None"), each = 4),
    iss = rep(c(20, 10, 25, 15), 3)
  )

  res <- traumar::seqic_indicator_10(
    data = test_data,
    unique_incident_id = id,
    level = trauma_level,
    transfer_out_indicator = acute_transfer,
    iss = iss,
    trauma_team_activation_level = activation,
    nfti = NULL
  )

  expected_cols <- c(
    "data",
    "triage_logic",
    "full_minor",
    "full_major",
    "limited_minor",
    "limited_major",
    "N",
    "sensitivity",
    "specificity",
    "positive_predictive_value",
    "negative_predictive_value",
    "false_negative_rate",
    "false_positive_rate",
    "false_discovery_rate",
    "false_omission_rate"
  )

  testthat::expect_true(all(expected_cols %in% names(res$diagnostic)))

  expect_true(all(res$diagnostic$triage_logic == "cribari"))
})

testthat::test_that("classification works with NFTI only", {
  set.seed(123)
  test_data <- tibble::tibble(
    id = 1:12,
    trauma_level = rep("I", 12),
    acute_transfer = rep("No", 12),
    activation = rep(c("Level 1", "Level 2", "None"), each = 4),
    NFTI = sample(x = c(TRUE, FALSE), size = 12, replace = TRUE)
  )

  res <- traumar::seqic_indicator_10(
    data = test_data,
    unique_incident_id = id,
    level = trauma_level,
    transfer_out_indicator = acute_transfer,
    iss = NULL,
    trauma_team_activation_level = activation,
    nfti = NFTI
  )

  expected_cols <- c(
    "data",
    "triage_logic",
    "full_minor",
    "full_major",
    "limited_minor",
    "limited_major",
    "N",
    "sensitivity",
    "specificity",
    "positive_predictive_value",
    "negative_predictive_value",
    "false_negative_rate",
    "false_positive_rate",
    "false_discovery_rate",
    "false_omission_rate"
  )

  testthat::expect_true(all(expected_cols %in% names(res$diagnostic)))

  expect_true(all(res$diagnostic$triage_logic == "nfti"))
})

testthat::test_that("error is thrown when both ISS and NFTI are supplied", {
  set.seed(123)
  test_data <- tibble::tibble(
    id = 1:12,
    trauma_level = rep("I", 12),
    acute_transfer = rep("No", 12),
    activation = rep(c("Level 1", "Level 2", "None"), each = 4),
    NFTI = sample(x = c(TRUE, FALSE), size = 12, replace = TRUE),
    iss = rep(c(20, 10, 25, 15), 3)
  )

  expect_error(
    traumar::seqic_indicator_10(
      data = test_data,
      unique_incident_id = id,
      transfer_out_indicator = acute_transfer,
      level = trauma_level,
      iss = iss,
      trauma_team_activation_level = activation,
      nfti = NFTI
    ),
    "Please supply exactly one of"
  )
})
