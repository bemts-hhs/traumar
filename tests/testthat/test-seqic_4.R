# tests/testthat/test-seqic_indicator_4.R

testthat::test_that("seqic_indicator_4() correctly expects columns to be in the 'data'", {
  # Minimal valid data
  data <- tibble::tibble(
    id = as.character(1:8),
    trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV"),
    ed_disp = c(
      "Operating Room",
      "Admitted",
      "Deceased/Expired",
      "Transferred",
      "Deceased/Expired",
      "Deceased/Expired",
      "Admitted",
      "Deceased/Expired"
    ),
    ed_los = c(120, 200, 5000, 180, 3000, 4321, 60, 4000),
    hosp_disp = c(
      "Deceased/Expired",
      "Deceased/Expired",
      "Deceased/Expired",
      "Discharged",
      "Deceased/Expired",
      "Deceased/Expired",
      "Discharged",
      "Deceased/Expired"
    ),
    hosp_los = c(3000, 4500, 1000, 200, 5000, 4400, 150, 3000),
    autopsy_done = c("Yes", "No", "No", NA, "Yes", "No", NA, "Yes")
  )

  testthat::expect_error(
    traumar::seqic_indicator_4(
      data = data,
      level = FAKE,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_4(
      data = data,
      level = trauma_level,
      ed_disposition = NOT_TRUE,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_4(
      data = data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = "something else",
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_4(
      data = data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = other_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_4(
      data = data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = a_fake_column,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_4(
      data = data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = error,
      autopsy = autopsy_done
    ),
    regexp = "It was not possible to validate"
  )

  testthat::expect_error(
    traumar::seqic_indicator_4(
      data = data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = FALSE
    ),
    regexp = "It was not possible to validate"
  )
})

testthat::test_that("seqic_indicator_4() input validation works as expected", {
  valid_data <- tibble::tibble(
    trauma_level = factor(rep("I", 2)),
    ed_disp = c("Deceased/Expired", "Deceased/Expired"),
    ed_los = c(3000, 4000),
    hosp_disp = c("Deceased/Expired", "Deceased/Expired"),
    hosp_los = c(4000, 5000),
    autopsy_done = c("Yes", "No"),
    id = c("1", "2"),
    group_var = c("A", "B")
  )

  # data must be data.frame or tibble
  expect_error(
    traumar::seqic_indicator_4(
      data = list(),
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    "must be of class.*data.frame.*tibble"
  )

  # level must be character or factor
  bad_data <- valid_data |> dplyr::mutate(trauma_level = as.numeric(1:2))
  expect_error(
    traumar::seqic_indicator_4(
      data = bad_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    "must be of class.*character.*factor"
  )

  # ed_disposition must be character or factor
  bad_data <- valid_data |> dplyr::mutate(ed_disp = as.numeric(1:2))
  expect_error(
    traumar::seqic_indicator_4(
      data = bad_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    "ed_disposition.*character.*factor"
  )

  # hospital_disposition must be character or factor
  bad_data <- valid_data |> dplyr::mutate(hosp_disp = as.numeric(1:2))
  expect_error(
    traumar::seqic_indicator_4(
      data = bad_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    "hospital_disposition.*character.*factor"
  )

  # ed_LOS must be numeric
  bad_data <- valid_data |> dplyr::mutate(ed_los = as.character(ed_los))
  expect_error(
    traumar::seqic_indicator_4(
      data = bad_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    "ed_LOS.*numeric"
  )

  # hospital_LOS must be numeric
  bad_data <- valid_data |> dplyr::mutate(hosp_los = as.character(hosp_los))
  expect_error(
    traumar::seqic_indicator_4(
      data = bad_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    "hospital_LOS.*numeric"
  )

  # autopsy must be character or factor
  bad_data <- valid_data |> dplyr::mutate(autopsy_done = as.numeric(1:2))
  expect_error(
    traumar::seqic_indicator_4(
      data = bad_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    "autopsy.*character.*factor"
  )

  # unique_incident_id must be character, factor, or numeric
  bad_data <- valid_data |> dplyr::mutate(id = list(1, 2))
  expect_error(
    traumar::seqic_indicator_4(
      data = bad_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done
    ),
    "unique_incident_id.*character.*numeric.*factor"
  )

  # groups must be character vector and present in data
  expect_error(
    traumar::seqic_indicator_4(
      data = valid_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done,
      groups = list()
    ),
    ".*groups.*strings"
  )

  expect_error(
    traumar::seqic_indicator_4(
      data = valid_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done,
      groups = c("bad_col")
    ),
    "not valid columns in.*data"
  )

  # calculate_ci must be NULL, "wilson", or "clopper-pearson"
  expect_error(
    traumar::seqic_indicator_4(
      data = valid_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done,
      calculate_ci = "jackknife"
    ),
    "must be.*wilson.*clopper-pearson"
  )

  # included_levels must be character, factor, or numeric
  expect_error(
    traumar::seqic_indicator_4(
      data = valid_data,
      level = trauma_level,
      ed_disposition = ed_disp,
      ed_LOS = ed_los,
      hospital_disposition = hosp_disp,
      hospital_LOS = hosp_los,
      unique_incident_id = id,
      autopsy = autopsy_done,
      included_levels = list("I", "II")
    ),
    "included_levels.*character.*factor.*numeric"
  )
})

testthat::test_that("seqic_indicator_4 computes 4a and 4b correctly", {
  data <- tibble::tibble(
    id = as.character(1:8),
    trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV"),
    ed_disp = c(
      "Operating Room",
      "Admitted",
      "Deceased/Expired",
      "Transferred",
      "Deceased/Expired",
      "Deceased/Expired",
      "Admitted",
      "Deceased/Expired"
    ),
    ed_los = c(120, 200, 5000, 180, 3000, 4321, 60, 4000),
    hosp_disp = c(
      "Deceased/Expired",
      "Deceased/Expired",
      "Deceased/Expired",
      "Discharged",
      "Deceased/Expired",
      "Deceased/Expired",
      "Discharged",
      "Deceased/Expired"
    ),
    hosp_los = c(3000, 4500, 1000, 200, 5000, 4400, 150, 3000),
    autopsy_done = c("Yes", "No", "No", NA, "Yes", "No", NA, "Yes")
  )

  result <- traumar::seqic_indicator_4(
    data = data,
    level = trauma_level,
    ed_disposition = ed_disp,
    ed_LOS = ed_los,
    hospital_disposition = hosp_disp,
    hospital_LOS = hosp_los,
    unique_incident_id = id,
    autopsy = autopsy_done
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(all(
    c(
      "numerator_4a",
      "denominator_4a",
      "seqic_4a",
      "numerator_4b",
      "denominator_4b",
      "seqic_4b"
    ) %in%
      names(result)
  ))
  testthat::expect_equal(result$numerator_4a, 3) # 3 had autopsy = "Yes"
  testthat::expect_equal(result$denominator_4a, 6) # 6 deaths
  testthat::expect_equal(result$numerator_4b, 3) # 3 long LOS and no autopsy
  testthat::expect_equal(result$denominator_4b, 4)
})

testthat::test_that("seqic_indicator_4 computes correctly with grouping", {
  data <- tibble::tibble(
    id = as.character(1:4),
    trauma_level = c("I", "II", "I", "II"),
    ed_disp = rep("Deceased/Expired", 4),
    ed_los = c(100, 100, 5000, 5000),
    hosp_disp = rep("Deceased/Expired", 4),
    hosp_los = c(100, 100, 5000, 5000),
    autopsy_done = c("Yes", "No", "No", "No"),
    hospital = c("A", "A", "B", "B")
  )

  result <- traumar::seqic_indicator_4(
    data = data,
    level = trauma_level,
    ed_disposition = ed_disp,
    ed_LOS = ed_los,
    hospital_disposition = hosp_disp,
    hospital_LOS = hosp_los,
    unique_incident_id = id,
    autopsy = autopsy_done,
    groups = "hospital"
  )

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$numerator_4a[result$hospital == "A"], 1)
  testthat::expect_equal(result$numerator_4b[result$hospital == "B"], 2)
})

testthat::test_that("seqic_indicator_4 returns confidence intervals", {
  data <- tibble::tibble(
    id = as.character(1:5),
    trauma_level = rep("I", 5),
    ed_disp = rep("Deceased/Expired", 5),
    ed_los = c(100, 100, 100, 5000, 5000),
    hosp_disp = rep("Deceased/Expired", 5),
    hosp_los = c(100, 100, 100, 5000, 5000),
    autopsy_done = c("Yes", "Yes", "No", "No", "No")
  )

  result <- traumar::seqic_indicator_4(
    data = data,
    level = trauma_level,
    ed_disposition = ed_disp,
    ed_LOS = ed_los,
    hospital_disposition = hosp_disp,
    hospital_LOS = hosp_los,
    unique_incident_id = id,
    autopsy = autopsy_done,
    calculate_ci = "wilson"
  )

  testthat::expect_true("lower_ci_4a" %in% names(result))
  testthat::expect_true("upper_ci_4b" %in% names(result))
  testthat::expect_type(result$lower_ci_4a, "double")
})
