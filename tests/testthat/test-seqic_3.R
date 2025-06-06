# tests/testthat/test-seqic_indicator_3.R

testthat::test_that("seqic_indicator_3() correctly expects columns to be in the 'data'", {
  # Minimal valid data
  test_data <- tibble::tibble(
    unique_id = c("1", "2", "3", "4", "5", "6"),
    trauma_level = c("I", "II", "III", "IV", "I", "III"),
    trauma_category = c(
      "Blunt",
      "Penetrating",
      "Burn",
      "Blunt",
      "Burn",
      "Penetrating"
    ),
    survival_prob = c(0.9, 0.8, NA, 0.95, NA, 0.88)
  )

  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = test_data,
      level = TRUE,
      trauma_type = trauma_category,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob,
      groups = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = test_data,
      level = trauma_level,
      trauma_type = "faked",
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob,
      groups = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = test_data,
      level = trauma_level,
      trauma_type = trauma_category,
      unique_incident_id = FALSE,
      probability_of_survival = survival_prob,
      groups = NULL
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = test_data,
      level = trauma_level,
      trauma_type = trauma_category,
      unique_incident_id = unique_id,
      probability_of_survival = "some fake thing",
      groups = NULL
    ),
    regexp = "It was not possible to validate"
  )
})

testthat::test_that("seqic_indicator_3: input validation", {
  data <- tibble::tibble(
    trauma_level = c("I", "II"),
    trauma_category = c("Blunt", "Penetrating"),
    unique_id = 1:2,
    survival_prob = c(0.9, 0.8)
  )

  # Invalid data
  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = "not_a_data",
      level = trauma_level,
      trauma_type = trauma_category,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob
    ),
    "must be of class"
  )

  # Set up error prone logical unique id
  logical_data <- tibble::tibble(
    trauma_level = c("I", "II"),
    trauma_category = c("Blunt", "Penetrating"),
    unique_id = logical(length = 2),
    survival_prob = c(0.9, 0.8)
  )

  # Invalid unique_id
  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = logical_data,
      level = trauma_level,
      trauma_type = trauma_category,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob
    ),
    "unique_incident_id.*must be of class"
  )

  # Invalid level
  bad_data <- tibble::tibble(
    level = c(1L, 2L), # integer instead of character/factor
    trauma_type = c("Blunt", "Burn"),
    unique_id = c("1", "2"),
    survival_prob = c(0.95, 0.9)
  )

  expect_error(
    traumar::seqic_indicator_3(
      data = bad_data,
      level = level,
      trauma_type = trauma_type,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob
    ),
    regexp = "must be of class.*character.*or.*factor"
  )

  # Invalid groups
  valid_data <- tibble::tibble(
    level = factor(c("I", "II")),
    trauma_type = c("Blunt", "Penetrating"),
    unique_id = c("1", "2"),
    survival_prob = c(0.9, 0.85)
  )

  expect_error(
    traumar::seqic_indicator_3(
      data = valid_data,
      level = level,
      trauma_type = trauma_type,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob,
      groups = list(1, 2) # not character strings
    ),
    regexp = "All elements in .*groups.* must be strings"
  )

  # Invalid trauma_type
  bad_data <- dplyr::mutate(data, trauma_category = as.numeric(c(1, 2)))
  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = bad_data,
      level = trauma_level,
      trauma_type = trauma_category,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob
    ),
    "must be of class"
  )

  # Invalid probability_of_survival
  bad_data <- dplyr::mutate(data, survival_prob = as.character(survival_prob))
  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = bad_data,
      level = trauma_level,
      trauma_type = trauma_category,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob
    ),
    "probability_of_survival.*must be of class"
  )

  # Invalid group var
  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = data,
      level = trauma_level,
      trauma_type = trauma_category,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob,
      groups = "bad_col"
    ),
    "not valid columns"
  )

  # Invalid calculate_ci
  testthat::expect_error(
    traumar::seqic_indicator_3(
      data = data,
      level = trauma_level,
      trauma_type = trauma_category,
      unique_incident_id = unique_id,
      probability_of_survival = survival_prob,
      calculate_ci = "not_a_method"
    ),
    "must be"
  )
})

testthat::test_that("seqic_indicator_3: basic logic", {
  test_data <- tibble::tibble(
    unique_id = c("1", "2", "3", "4", "5", "6"),
    trauma_level = c("I", "II", "III", "IV", "I", "III"),
    trauma_category = c(
      "Blunt",
      "Penetrating",
      "Burn",
      "Blunt",
      "Burn",
      "Penetrating"
    ),
    survival_prob = c(0.9, 0.8, NA, 0.95, NA, 0.88)
  )

  res <- traumar::seqic_indicator_3(
    data = test_data,
    level = trauma_level,
    trauma_type = trauma_category,
    unique_incident_id = unique_id,
    probability_of_survival = survival_prob,
    groups = NULL
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("data", "numerator_3", "denominator_3", "seqic_3")
  )
  testthat::expect_equal(res$numerator_3, 4)
  testthat::expect_equal(res$denominator_3, 4) # burn cases excluded
  testthat::expect_equal(res$seqic_3, 4 / 4)
})

testthat::test_that("seqic_indicator_3: grouped results and confidence intervals", {
  test_data <- tibble::tibble(
    unique_id = as.character(1:6),
    trauma_level = c("I", "I", "II", "II", "III", "IV"),
    trauma_category = c(
      "Blunt",
      "Burn",
      "Penetrating",
      "Blunt",
      "Burn",
      "Blunt"
    ),
    survival_prob = c(0.9, NA, 0.85, NA, NA, 0.95)
  )

  res <- traumar::seqic_indicator_3(
    data = test_data,
    level = trauma_level,
    trauma_type = trauma_category,
    unique_incident_id = unique_id,
    probability_of_survival = survival_prob,
    groups = "trauma_level",
    calculate_ci = "wilson"
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true("trauma_level" %in% colnames(res))
  testthat::expect_true(all(
    c(
      "numerator_3",
      "denominator_3",
      "seqic_3",
      "lower_ci_3",
      "upper_ci_3"
    ) %in%
      names(res)
  ))
})
