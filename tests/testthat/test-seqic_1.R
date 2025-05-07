# tests/testthat/test-seqic_indicator_1.R

testthat::test_that("seqic_indicator_1() validates input types correctly", {
  # Create minimal valid input
  valid_df <- tibble::tibble(
    activation_level = factor("Level 1"),
    provider_type = factor("Surgery/Trauma"),
    trauma_level = factor("I"),
    response_minutes = 10,
    provider = "Dr. A",
    incident_id = 1
  )

  # Valid call should not error
  testthat::expect_silent(
    traumar::seqic_indicator_1(
      df = valid_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider
    )
  )

  # df is not a data.frame
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = as.matrix(valid_df),
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider
    ),
    "df.*must be of class"
  )

  # trauma_team_activation_level is not character or factor
  bad_df <- dplyr::mutate(valid_df, activation_level = 1)
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = bad_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider
    ),
    "trauma_team_activation_level.*must be of class"
  )

  # trauma_team_physician_service_type is invalid
  bad_df <- dplyr::mutate(valid_df, provider_type = 42)
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = bad_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider
    ),
    "trauma_team_physician_service_type.*must be of class"
  )

  # unique_incident_id invalid
  bad_df <- dplyr::mutate(valid_df, incident_id = list("A"))
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = bad_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider
    ),
    "unique_incident_id.*must be of class"
  )

  # level is not character or factor
  bad_df <- dplyr::mutate(valid_df, trauma_level = 99)
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = bad_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider
    ),
    "level.*must be of class"
  )

  # response_time is not numeric
  bad_df <- dplyr::mutate(valid_df, response_minutes = "soon")
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = bad_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider
    ),
    "response_time.*must be of class"
  )

  # trauma_team_activation_provider is not character or factor
  bad_df <- dplyr::mutate(valid_df, provider = 999)
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = bad_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider
    ),
    "trauma_team_activation_provider.*must be of class"
  )
})

testthat::test_that("seqic_indicator_1() validates groups and calculate_ci", {
  valid_df <- tibble::tibble(
    activation_level = "Level 1",
    provider_type = "Surgery/Trauma",
    trauma_level = "I",
    response_minutes = 10,
    provider = "Dr. A",
    incident_id = 1,
    group_var = "Test"
  )

  # groups contains a non-string
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = valid_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider,
      groups = list(123)
    ),
    "All elements in.*groups.*must be strings"
  )

  # groups contains a string not found in df
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = valid_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider,
      groups = c("not_a_col")
    ),
    "not valid columns in"
  )

  # calculate_ci is invalid
  testthat::expect_error(
    traumar::seqic_indicator_1(
      df = valid_df,
      trauma_team_activation_level = activation_level,
      trauma_team_physician_service_type = provider_type,
      level = trauma_level,
      unique_incident_id = incident_id,
      response_time = response_minutes,
      trauma_team_activation_provider = provider,
      calculate_ci = "wrong"
    ),
    "it must be"
  )
})

testthat::test_that("seqic_indicator_1 calculates indicators 1a–1c correctly", {
  # Sample test data
  test_data <- tibble::tibble(
    incident_id = c(1, 2, 3, 4, 5, 6),
    activation_level = c(
      "Level 1",
      "Level 1",
      "Level 1",
      "Level 1",
      "Level 1",
      "Level 1"
    ),
    provider_type = c(
      "Surgery/Trauma",
      "Surgery/Trauma",
      "Surgery/Trauma",
      "Surgery/Trauma",
      "Surgery/Trauma",
      "Surgery/Trauma"
    ),
    trauma_level = c("I", "II", "III", "III", "II", "I"),
    response_minutes = c(10, 20, 25, NA, 40, NA),
    provider = paste("Dr", LETTERS[1:6])
  )

  # Run without CI
  result <- traumar::seqic_indicator_1(
    df = test_data,
    trauma_team_activation_level = activation_level,
    trauma_team_physician_service_type = provider_type,
    level = trauma_level,
    unique_incident_id = incident_id,
    response_time = response_minutes,
    trauma_team_activation_provider = provider,
    calculate_ci = NULL
  )

  # Expectations for 1a (I and II only, within 15 minutes)
  testthat::expect_equal(result$numerator_1a, 1)
  testthat::expect_equal(result$denominator_1a, 3)
  testthat::expect_equal(result$seqic_1a, 1 / 3)

  # Expectations for 1b (I, II, III, within 30 minutes)
  testthat::expect_equal(result$numerator_1b, 3)
  testthat::expect_equal(result$denominator_1b, 4)
  testthat::expect_equal(result$seqic_1b, 0.75)

  # Expectations for 1c (missing response time among eligible)
  testthat::expect_equal(result$numerator_1c, 2)
  testthat::expect_equal(result$denominator_1c, 6)
  testthat::expect_equal(result$seqic_1c, 1 / 3)
})

testthat::test_that("seqic_indicator_1 returns confidence intervals when requested", {
  test_data <- tibble::tibble(
    incident_id = c(1, 2, 3),
    activation_level = rep("Level 1", 3),
    provider_type = rep("Surgery/Trauma", 3),
    trauma_level = c("I", "II", "III"),
    response_minutes = c(10, 30, NA),
    provider = c("Dr. X", "Dr. Y", "Dr. Z")
  )

  result <- traumar::seqic_indicator_1(
    df = test_data,
    trauma_team_activation_level = activation_level,
    trauma_team_physician_service_type = provider_type,
    level = trauma_level,
    unique_incident_id = incident_id,
    response_time = response_minutes,
    trauma_team_activation_provider = provider,
    calculate_ci = "wilson"
  )

  testthat::expect_true("lower_ci_1a" %in% names(result))
  testthat::expect_true("upper_ci_1a" %in% names(result))
  testthat::expect_true("lower_ci_1b" %in% names(result))
  testthat::expect_true("upper_ci_1b" %in% names(result))
  testthat::expect_true("lower_ci_1c" %in% names(result))
  testthat::expect_true("upper_ci_1c" %in% names(result))
})
