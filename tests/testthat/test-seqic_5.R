testthat::test_that("seqic_indicator_5() data validation errors", {
  # Minimal valid input for baseline
  valid_df <- tibble::tibble(
    level = factor(c("I", "II")),
    id = c("a", "b"),
    bac = c(0.08, 0),
    drug = c("opioid", "none")
  )

  # df must be a data.frame or tibble
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = list(),
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug
    ),
    "must be of class.*data.frame"
  )

  # level must be character or factor
  df_bad_level <- dplyr::mutate(valid_df, level = as.numeric(c(1, 2)))
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = df_bad_level,
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug
    ),
    "must be of class.*character.*or.*factor"
  )

  # unique_incident_id must be character, factor, or numeric
  df_bad_id <- dplyr::mutate(valid_df, id = list(1:2))
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = df_bad_id,
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug
    ),
    "must be of class.*character.*numeric.*or.*factor"
  )

  # blood_alcohol_content must be numeric
  df_bad_bac <- dplyr::mutate(valid_df, bac = c("yes", "no"))
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = df_bad_bac,
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug
    ),
    "must be of class.*numeric"
  )

  # drug_screen must be character or factor
  df_bad_drug <- dplyr::mutate(
    valid_df,
    drug = as.Date(c("2022-01-01", "2022-01-02"))
  )
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = df_bad_drug,
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug
    ),
    "must be of class.*character.*or.*factor"
  )

  # groups must be character vector
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = valid_df,
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug,
      groups = 123
    ),
    "All elements in .*groups.* must be strings"
  )

  # groups must exist in df
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = valid_df,
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug,
      groups = "nonexistent_column"
    ),
    "not valid columns in .*df"
  )

  # calculate_ci must be "wilson", "clopper-pearson", or NULL
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = valid_df,
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug,
      calculate_ci = "bootstrap"
    ),
    "must be.*wilson.*or.*clopper-pearson"
  )

  # included_levels must be character, numeric, or factor
  testthat::expect_error(
    traumar::seqic_indicator_5(
      df = valid_df,
      level = level,
      unique_incident_id = id,
      blood_alcohol_content = bac,
      drug_screen = drug,
      included_levels = list("I", "II")
    ),
    "must be of class.*character.*factor.*or.*numeric"
  )
})

testthat::test_that("seqic_indicator_5() computes indicators correctly", {
  test_data <- tibble::tibble(
    id = as.character(1:10),
    trauma_level = rep(c("I", "II", "III", "IV", "V"), each = 2),
    bac = c(0.08, NA, 0, 0.02, NA, 0.15, NA, NA, 0, 0),
    drug = c(
      "opioid",
      "none",
      "cocaine",
      "none",
      NA,
      "benzodiazepine",
      "alcohol",
      "thc",
      "none",
      NA
    )
  )

  result <- traumar::seqic_indicator_5(
    df = test_data,
    level = trauma_level,
    unique_incident_id = id,
    blood_alcohol_content = bac,
    drug_screen = drug
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    object = result,
    expected = c(
      'Data',
      'numerator_5a',
      'denominator_5a',
      'seqic_5a',
      'numerator_5b',
      'denominator_5b',
      'seqic_5b',
      'numerator_5c',
      'denominator_5c',
      'seqic_5c',
      'numerator_5d',
      'denominator_5d',
      'seqic_5d'
    )
  )
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$numerator_5a, 4)
  testthat::expect_equal(result$denominator_5a, 8)
  testthat::expect_equal(result$seqic_5a, 4 / 8)
  testthat::expect_equal(result$numerator_5b, 3)
  testthat::expect_equal(result$denominator_5b, 4)
  testthat::expect_equal(result$seqic_5b, 3 / 4)
  testthat::expect_equal(result$numerator_5c, 7)
  testthat::expect_equal(result$denominator_5c, 8)
  testthat::expect_equal(result$seqic_5c, 7 / 8)
  testthat::expect_equal(result$numerator_5d, 5)
  testthat::expect_equal(result$denominator_5d, 7)
  testthat::expect_equal(result$seqic_5d, 5 / 7)
})

testthat::test_that("seqic_indicator_5() handles empty input after filter", {
  empty_data <- tibble::tibble(
    id = c("1", "2"),
    trauma_level = c("V", "V"),
    bac = c(0.1, NA),
    drug = c("opioid", "none")
  )

  result <- traumar::seqic_indicator_5(
    df = empty_data,
    level = trauma_level,
    unique_incident_id = id,
    blood_alcohol_content = bac,
    drug_screen = drug
  )

  testthat::expect_equal(result$denominator_5a, 0)
  testthat::expect_true(all(is.na(result[c(
    "seqic_5a",
    "seqic_5b",
    "seqic_5c",
    "seqic_5d"
  )])))
})

testthat::test_that("seqic_indicator_5() returns confidence intervals if requested", {
  test_data <- tibble::tibble(
    id = as.character(1:6),
    trauma_level = rep("I", 6),
    bac = c(NA, 0.05, 0.00, 0.08, 0, 0),
    drug = c("thc", "none", "opioid", "thc", "none", "none")
  )

  result <- traumar::seqic_indicator_5(
    df = test_data,
    level = trauma_level,
    unique_incident_id = id,
    blood_alcohol_content = bac,
    drug_screen = drug,
    calculate_ci = "wilson"
  )

  testthat::expect_true(all(
    c("lower_ci_5a", "upper_ci_5a", "lower_ci_5d", "upper_ci_5d") %in%
      names(result)
  ))
  testthat::expect_type(result$lower_ci_5a, "double")
  testthat::expect_true(result$lower_ci_5a <= result$seqic_5a)
  testthat::expect_true(result$upper_ci_5a >= result$seqic_5a)
})

testthat::test_that("seqic_indicator_5() works with groupings", {
  grouped_data <- tibble::tibble(
    id = as.character(1:8),
    trauma_level = rep(c("I", "II"), each = 4),
    bac = c(0.1, NA, 0, 0, 0.15, NA, 0, NA),
    drug = c("thc", NA, "none", "opioid", "none", "opioid", "none", "none"),
    region = c("East", "East", "West", "West", "East", "East", "West", "West")
  )

  result <- traumar::seqic_indicator_5(
    df = grouped_data,
    level = trauma_level,
    unique_incident_id = id,
    blood_alcohol_content = bac,
    drug_screen = drug,
    groups = "region"
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_setequal(result$region, c("East", "West"))
})
