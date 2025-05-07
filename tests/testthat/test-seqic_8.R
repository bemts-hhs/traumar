testthat::test_that("seqic_indicator_8() data validation - df must be a data.frame or tibble", {
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = "not_a_df",
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk
    ),
    "df.*must be a data frame or tibble"
  )
})

testthat::test_that("seqic_indicator_8() data validation - level must be character or factor", {
  df <- tibble::tibble(
    trauma_level = as.numeric(1:3),
    id = as.character(1:3),
    mortality = c(TRUE, FALSE, TRUE),
    risk = c("Low", "Moderate", "High")
  )
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk
    ),
    "level.*must be character or factor"
  )
})

testthat::test_that("seqic_indicator_8() data validation - unique_incident_id must be character, numeric, or factor", {
  df <- tibble::tibble(
    trauma_level = as.character(c("I", "II", "III")),
    id = list(1, 2, 3),
    mortality = c(TRUE, FALSE, TRUE),
    risk = c("Low", "Moderate", "High")
  )
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk
    ),
    "unique_incident_id.*must be of class"
  )
})

testthat::test_that("seqic_indicator_8() data validation - mortality_indicator must be character, factor, or logical", {
  df <- tibble::tibble(
    trauma_level = as.character(c("I", "II", "III")),
    id = as.character(1:3),
    mortality = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    risk = c("Low", "Moderate", "High")
  )
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk
    ),
    "mortality_indicator.*must be character, factor, or logical"
  )
})

testthat::test_that("seqic_indicator_8() data validation - risk_group must be character or factor", {
  df <- tibble::tibble(
    trauma_level = as.character(c("I", "II", "III")),
    id = as.character(1:3),
    mortality = c(TRUE, FALSE, TRUE),
    risk = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  )
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk
    ),
    "risk_group.*must be character or factor"
  )
})

testthat::test_that("seqic_indicator_8() data validation - groups must be character vector", {
  df <- tibble::tibble(
    trauma_level = as.character(c("I", "II", "III")),
    id = as.character(1:3),
    mortality = c(TRUE, FALSE, TRUE),
    risk = c("Low", "Moderate", "High")
  )
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk,
      groups = 1
    ),
    "groups.*must be strings"
  )
})

testthat::test_that("seqic_indicator_8() data validation - all groups must exist in df", {
  df <- tibble::tibble(
    trauma_level = as.character(c("I", "II", "III")),
    id = as.character(1:3),
    mortality = c(TRUE, FALSE, TRUE),
    risk = c("Low", "Moderate", "High")
  )
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk,
      groups = c("nonexistent_var")
    ),
    "Invalid grouping variable"
  )
})

testthat::test_that("seqic_indicator_8() data validation - calculate_ci must be wilson or clopper-pearson", {
  df <- tibble::tibble(
    trauma_level = as.character(c("I", "II", "III")),
    id = as.character(1:3),
    mortality = c(TRUE, FALSE, TRUE),
    risk = c("Low", "Moderate", "High")
  )
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk,
      calculate_ci = "invalid"
    ),
    "must be.*wilson.*clopper-pearson"
  )
})

testthat::test_that("seqic_indicator_8() data validation - included_levels must be character, numeric, or factor", {
  df <- tibble::tibble(
    trauma_level = as.character(c("I", "II", "III")),
    id = as.character(1:3),
    mortality = c(TRUE, FALSE, TRUE),
    risk = c("Low", "Moderate", "High")
  )
  testthat::expect_error(
    traumar::seqic_indicator_8(
      df = df,
      level = trauma_level,
      unique_incident_id = id,
      mortality_indicator = mortality,
      risk_group = risk,
      included_levels = list("I", "II")
    ),
    "included_levels.*must be of class"
  )
})

testthat::test_that("seqic_indicator_8 correctly calculates overall and risk group survival", {
  test_data <- dplyr::tibble(
    id = as.character(1:10),
    trauma_level = c("I", "II", "III", "IV", "V", "II", "I", "III", "IV", "II"),
    mortality = c(
      FALSE,
      "No",
      TRUE,
      "Yes",
      FALSE,
      TRUE,
      "No",
      FALSE,
      "Yes",
      TRUE
    ),
    risk = c(
      "High",
      "High",
      "Moderate",
      "Moderate",
      "Low",
      "Low",
      "High",
      "Moderate",
      "Low",
      "Moderate"
    )
  )

  result <- traumar::seqic_indicator_8(
    df = test_data,
    level = trauma_level,
    unique_incident_id = id,
    mortality_indicator = mortality,
    risk_group = risk
  )

  # Check output structure
  testthat::expect_named(result, c("overall", "risk_group"))
  testthat::expect_s3_class(result$overall, "tbl_df")
  testthat::expect_s3_class(result$risk_group, "tbl_df")

  # Check correct numerators/denominators
  testthat::expect_equal(result$overall$numerator_8_all, 4)
  testthat::expect_equal(result$overall$denominator_8_all, 9)
  testthat::expect_equal(result$overall$seqic_8_all, 4 / 9)

  # Check stratified values
  risk_summary <- result$risk_group
  testthat::expect_true(all(
    risk_summary$denominator_8_risk >= risk_summary$numerator_8_risk
  ))
  testthat::expect_true(all(risk_summary$seqic_8_risk <= 1))
})

testthat::test_that("seqic_indicator_8 filters by included_levels", {
  df <- dplyr::tibble(
    id = 1:5,
    trauma_level = c("I", "V", "II", "III", "IV"),
    mortality = c("No", "Yes", FALSE, TRUE, "No"),
    risk = rep("Moderate", 5)
  )

  result <- traumar::seqic_indicator_8(
    df = df,
    level = trauma_level,
    unique_incident_id = id,
    mortality_indicator = mortality,
    risk_group = risk
  )

  # Should exclude trauma_level "V"
  testthat::expect_equal(result$overall$denominator_8_all, 4)
})

testthat::test_that("seqic_indicator_8 includes confidence intervals when calculate_ci is specified", {
  df <- dplyr::tibble(
    id = 1:6,
    trauma_level = rep("II", 6),
    mortality = c("No", "Yes", "No", "No", "Yes", FALSE),
    risk = c("Low", "Low", "Low", "High", "High", "High")
  )

  result <- traumar::seqic_indicator_8(
    df = df,
    level = trauma_level,
    unique_incident_id = id,
    mortality_indicator = mortality,
    risk_group = risk,
    calculate_ci = "wilson"
  )

  testthat::expect_true(all(
    c("lower_ci_8", "upper_ci_8") %in% names(result$overall)
  ))
  testthat::expect_true(all(
    c("lower_ci_8_risk", "upper_ci_8_risk") %in% names(result$risk_group)
  ))
})

testthat::test_that("seqic_indicator_8 handles groupings correctly", {
  df <- dplyr::tibble(
    id = 1:6,
    trauma_level = rep("III", 6),
    mortality = c(TRUE, FALSE, FALSE, "Yes", "No", "No"),
    risk = rep("Low", 6),
    region = c("East", "East", "West", "West", "East", "West")
  )

  result <- traumar::seqic_indicator_8(
    df = df,
    level = trauma_level,
    unique_incident_id = id,
    mortality_indicator = mortality,
    risk_group = risk,
    groups = "region"
  )

  testthat::expect_true("region" %in% names(result$overall))
  testthat::expect_equal(nrow(result$overall), 2)
})
