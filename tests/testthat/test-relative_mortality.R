###_____________________________________________________________________________
# test the rmm function
###_____________________________________________________________________________

testthat::test_that("rmm() input validation for Ps_col and outcome_col", {
  testthat::expect_error(
    traumar::rmm(data = dplyr::tibble(x = 1:5)),
    "Both `Ps_col` and `outcome_col` arguments must be provided."
  )
})

testthat::test_that("rmm() rejects outcome_col with wrong data type", {
  df <- dplyr::tibble(Ps = runif(10), outcome = letters[1:10])
  testthat::expect_error(
    traumar::rmm(data = df, Ps_col = Ps, outcome_col = outcome),
    "outcome_col.*must be of class.*numeric, logical, integer"
  )
})

testthat::test_that("rmm() validates bootstrap_ci is logical", {
  df <- dplyr::tibble(Ps = runif(10), outcome = rbinom(10, 1, 0.5))
  testthat::expect_error(
    traumar::rmm(
      data = df,
      Ps_col = Ps,
      outcome_col = outcome,
      bootstrap_ci = "yes"
    ),
    "bootstrap_ci.*logical.*"
  )
})

testthat::test_that("rmm() fails if seed is not numeric", {
  df <- dplyr::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    outcome = rbinom(1000, 1, 0.5)
  )
  testthat::expect_error(
    traumar::rmm(data = df, Ps_col = Ps, outcome_col = outcome, seed = "foo")
  )
})

testthat::test_that("rmm() validates group_vars are character vectors", {
  df <- dplyr::tibble(Ps = runif(10), outcome = rbinom(10, 1, 0.5))
  testthat::expect_error(
    traumar::rmm(
      data = df,
      Ps_col = Ps,
      outcome_col = outcome,
      group_vars = list(1)
    ),
    "group_vars.*must be of class.*character.*factor"
  )
})

testthat::test_that("rmm() validates group_vars exist in data", {
  df <- dplyr::tibble(Ps = runif(10), outcome = rbinom(10, 1, 0.5))
  testthat::expect_error(
    traumar::rmm(
      data = df,
      Ps_col = Ps,
      outcome_col = outcome,
      group_vars = c("fake_col")
    ),
    "group_vars.*contains invalid column names.*fake_col.*Valid column names are.*Ps, outcome"
  )
})

testthat::test_that("rmm function validates inputs correctly", {
  # Test if data is a data frame
  testthat::expect_error(rmm(data = NULL, Ps_col = Ps, outcome_col = survival))

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )
  testthat::expect_error(rmm(data = df, outcome_col = survival, n_samples = 5))

  # Test missing outcome_col
  testthat::expect_error(rmm(data = df, Ps_col = Ps))

  # Test non-binary outcome_col
  df_non_binary <- tibble::tibble(
    Ps = plogis(rnorm(5, mean = 2, sd = 1.5)),
    survival = c(1, 2, 3, 4, 5)
  )
  testthat::expect_error(rmm(
    data = df_non_binary,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 5
  ))

  # Test non-numeric Ps_col
  df_non_numeric <- tibble::tibble(Ps = c("a", "b", "c"), survival = c(1, 0, 1))
  testthat::expect_error(rmm(
    data = df_non_numeric,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 5
  ))

  # Test Ps values > 1
  df_ps_above_1 <- tibble::tibble(
    Ps = sample(30:150, size = 1000, replace = TRUE),
    survival = sample(c(1, 0), size = 1000, replace = TRUE)
  )
  testthat::expect_error(rmm(
    data = df_ps_above_1,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 5
  ))

  #Test incorrect input to n_samples
  testthat::expect_error(rmm(
    data = data.frame(Ps = c(0.1, 0.5, 0.9, .005), survival = c(1, 0, 1, 0)),
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = "1000"
  ))
})

testthat::test_that("rmm function computes binning correctly", {
  set.seed(01232025)

  # Test the binning process with some mock data
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  # Check if binning works as expected
  result <- rmm(
    data = df,
    Ps_col = Ps,
    outcome_col = survival,
    Divisor1 = 5,
    Divisor2 = 5,
    n_samples = 5
  )
  testthat::expect_true(all(
    c(
      "population_RMM_LL",
      "population_RMM",
      "population_RMM_UL",
      "population_CI",
      "bootstrap_RMM_LL",
      "bootstrap_RMM",
      "bootstrap_RMM_UL",
      "bootstrap_CI"
    ) %in%
      colnames(result)
  ))

  # Test that RMM is calculated
  testthat::expect_true(all(!is.na(result$population_RMM)))
  testthat::expect_true(all(!is.na(result$bootstrap_RMM)))
})

testthat::test_that("rmm function calculates RMM and its confidence intervals", {
  set.seed(01232025)

  # Test with mock data for RMM calculation
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  result <- rmm(
    data = df,
    Ps_col = Ps,
    outcome_col = survival,
    Divisor1 = 5,
    Divisor2 = 5,
    n_samples = 5
  )

  # Test for upper and lower bounds of RMM
  testthat::expect_true(all(result$population_RMM_LL <= result$population_RMM))
  testthat::expect_true(all(result$population_RMM_UL >= result$population_RMM))
  testthat::expect_true(all(result$bootstrap_RMM_LL <= result$bootstrap_RMM))
  testthat::expect_true(all(result$bootstrap_RMM_UL >= result$bootstrap_RMM))
})

testthat::test_that("rmm function handles the pivot argument correctly", {
  # Test with pivot = TRUE
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  result_pivot <- rmm(
    data = df,
    Ps_col = Ps,
    outcome_col = survival,
    pivot = TRUE,
    n_samples = 5
  )
  testthat::expect_true("stat" %in% colnames(result_pivot))
  testthat::expect_true("value" %in% colnames(result_pivot))

  # Test without pivot (default is FALSE)
  result_no_pivot <- rmm(
    data = df,
    Ps_col = Ps,
    outcome_col = survival,
    pivot = FALSE,
    n_samples = 5
  )
  testthat::expect_false("stat" %in% colnames(result_no_pivot))
  testthat::expect_false("value" %in% colnames(result_no_pivot))
})

testthat::test_that("rmm function handles edge cases correctly", {
  # Custom expectation function to test for the presence of a specific warning
  # pattern
  expect_warning_present <- function(expr, pattern) {
    # Initialize an empty character vector to store warning messages
    warnings <- character()

    # Evaluate the expression while intercepting all warning conditions
    withCallingHandlers(
      # Force evaluation of the expression to ensure any lazy evaluation is
      # triggered
      force(expr),

      # Define a handler specifically for warning conditions
      warning = function(w) {
        # Append the current warning message to the 'warnings' vector
        warnings <<- c(warnings, conditionMessage(w))

        # Prevent the warning from being printed to the console during tests
        invokeRestart("muffleWarning")
      }
    )

    # Assert that at least one warning matches the specified regular expression
    # pattern
    testthat::expect_true(
      # Logical test: does any warning message match the provided pattern?
      any(stringr::str_detect(warnings, pattern)),

      # Message to display if the expectation fails
      info = paste("Expected warning matching:", pattern)
    )
  }

  # Generate example data
  set.seed(123)

  # Parameters
  # Total number of patients
  n_patients <- 5000

  # Arbitrary group labels
  groups <- sample(x = LETTERS[1:2], size = n_patients, replace = TRUE)

  # Trauma types
  trauma_type_values <- sample(
    x = c("Blunt", "Penetrating"),
    size = n_patients,
    replace = TRUE
  )

  # RTS values
  rts_values <- sample(
    x = seq(from = 0, to = 7.8408, by = 0.005),
    size = n_patients,
    replace = TRUE
  )

  # patient ages
  ages <- sample(
    x = seq(from = 0, to = 100, by = 1),
    size = n_patients,
    replace = TRUE
  )

  # ISS scores
  iss_scores <- sample(
    x = seq(from = 0, to = 75, by = 1),
    size = n_patients,
    replace = TRUE
  )

  # Generate survival probabilities (Ps)
  Ps <- traumar::probability_of_survival(
    trauma_type = trauma_type_values,
    age = ages,
    rts = rts_values,
    iss = iss_scores
  )

  # Simulate survival outcomes based on Ps
  survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)

  # Create data frame
  data <- data.frame(
    Ps = Ps,
    survival = survival_outcomes,
    groups = groups
  ) |>
    dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))

  # Test with edge case: only one row
  df_one_row <- data |> dplyr::slice_sample(n = 1)
  testthat::expect_error(
    rmm(
      data = df_one_row,
      Ps_col = Ps,
      outcome_col = survival
    ),
    regexp = "outcome_col.*must have a length within range.*2, Inf"
  )

  # Test with all NA values in Ps
  df_na_ps <- data |>
    dplyr::mutate(Ps = dplyr::if_else(Ps < 0.01, NA_real_, Ps))

  # Test that the rmm() function emits a warning about missing values in Ps_col
  expect_warning_present(
    # Expression to evaluate: calling rmm() with deliberately missing Ps values
    rmm(
      data = df_na_ps, # Dataset with intentionally introduced NA values in Ps
      Ps_col = Ps, # Name of the probability score column being tested
      outcome_col = survival # Outcome variable for the model
    ),

    # Regular expression to match the expected warning message about missing Ps
    # values
    "Ps_col.*missing values detected"
  )

  # Create a dataset with deliberately missing values in the outcome column
  # (survival)
  df_na_outcome <- data |>
    dplyr::mutate(
      survival = dplyr::if_else(Ps < 0.01, NA_real_, survival)
    )

  # Use the custom expect_warning_present() function to test for the specific warning
  expect_warning_present(
    # Expression to evaluate: run rmm() on the dataset with missing outcome values
    rmm(
      data = df_na_outcome,
      Ps_col = Ps,
      outcome_col = survival
    ),

    # Regular expression to match the expected warning message about missing outcome values
    "outcome_col.*missing values detected"
  )
})

testthat::test_that("rmm function handles the seed argument correctly", {
  # Test with a fixed seed value to ensure reproducibility
  set.seed(123)
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  result_1 <- rmm(
    data = df,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 5,
    seed = 123
  )
  result_2 <- rmm(
    data = df,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 5,
    seed = 123
  )

  # Ensure the results are identical with the same seed
  testthat::expect_identical(result_1, result_2)

  # Test with a different seed to check if results change
  result_3 <- rmm(
    data = df,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 5,
    seed = 456
  )

  testthat::expect_false(identical(result_1, result_3))
})

testthat::test_that("rmm function handles the group_vars argument correctly", {
  set.seed(01232025)

  # Test with grouping by a variable (e.g., a factor)
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9),
    group = rep(letters[1:5], each = 200)
  )

  result_grouped <- rmm(
    data = df,
    Ps_col = Ps,
    outcome_col = survival,
    group_vars = "group",
    n_samples = 100
  )

  # Test if the output contains the group variable
  testthat::expect_true("group" %in% colnames(result_grouped))

  # Ensure that results are calculated per group
  group_means <- result_grouped %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(mean_rmm = mean(population_RMM, na.rm = TRUE))
  testthat::expect_true(all(!is.na(group_means$mean_rmm)))
})

testthat::test_that("rmm function handles edge cases with seed and group_vars", {
  # Test with missing group_vars
  df_missing_group <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )
  result_no_group <- rmm(
    data = df_missing_group,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 5,
    seed = 123
  )

  # Test that no error occurs and RMM is calculated
  testthat::expect_true(all(!is.na(result_no_group$population_RMM)))

  # Test with group_vars but no valid grouping
  df_invalid_group <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9),
    group = rep(NA, 1000)
  )

  testthat::expect_error(rmm(
    data = df_invalid_group,
    Ps_col = Ps,
    outcome_col = survival,
    group_vars = group,
    n_samples = 5,
    seed = 123
  ))
})

testthat::test_that("rmm function handles missing values with seed and group_vars correctly", {
  # Custom expectation function to test for the presence of a specific warning pattern
  expect_warning_present <- function(expr, pattern) {
    # Initialize an empty character vector to store warning messages
    warnings <- character()

    # Evaluate the expression while intercepting all warning conditions
    withCallingHandlers(
      # Force evaluation of the expression to ensure any lazy evaluation is triggered
      force(expr),

      # Define a handler specifically for warning conditions
      warning = function(w) {
        # Append the current warning message to the 'warnings' vector
        warnings <<- c(warnings, conditionMessage(w))

        # Prevent the warning from being printed to the console during tests
        invokeRestart("muffleWarning")
      }
    )

    # Assert that at least one warning matches the specified regular expression pattern
    testthat::expect_true(
      # Logical test: does any warning message match the provided pattern?
      any(stringr::str_detect(warnings, pattern)),

      # Message to display if the expectation fails
      info = paste("Expected warning matching:", pattern)
    )
  }

  # Create a larger dataset with missing values in Ps, survival, and group variables
  set.seed(123)
  df_missing_values <- tibble::tibble(
    Ps = c(rep(NA, 200), plogis(rnorm(800, mean = 2, sd = 1.5))), # 200 NAs in Ps
    survival = rbinom(1000, 1, prob = 0.8), # Binary outcome, no NAs here
    group = rep(c("A", "B", "C", "D", "E"), each = 200) # Group variable with 5 groups
  )

  # Expect a warning when running rmm() with missing values in the dataset (Ps
  # column missing)
  expect_warning_present(
    rmm(
      data = df_missing_values,
      Ps_col = Ps,
      outcome_col = survival,
      group_vars = "group",
      n_samples = 5,
      seed = 123
    ),
    "Ps_col.*missing values detected"
  )

  # Create a dataset with missing values in the *outcome* column
  df_missing_values <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), # Ps column fully populated
    survival = c(rep(NA, 200), rbinom(800, 1, prob = 0.8)), # 200 NAs in survival
    group = rep(c("A", "B", "C", "D", "E"), each = 200) # Same group structure
  )

  # Expect a warning when running rmm() with missing values in the *outcome* column
  expect_warning_present(
    rmm(
      data = df_missing_values,
      Ps_col = Ps,
      outcome_col = survival,
      group_vars = "group",
      n_samples = 5,
      seed = 123
    ),
    "outcome_col.*missing values detected"
  )
})

testthat::test_that("rm_bin_summary has correct validation of outcome_col when it is logical", {
  set.seed(01232025)

  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = sample(
      x = c(TRUE, FALSE),
      size = 1000,
      replace = TRUE,
      prob = c(0.9, 0.1)
    )
  )

  testthat::expect_no_error(
    rmm(data = df, Ps_col = Ps, outcome_col = survival)
  )
})

###_____________________________________________________________________________
# test the rm_bin_summary function
###_____________________________________________________________________________

testthat::test_that("rm_bin_summary() input validation for Ps_col and outcome_col", {
  testthat::expect_error(
    traumar::rm_bin_summary(data = dplyr::tibble(x = 1:5)),
    "Both `Ps_col` and `outcome_col` arguments must be provided."
  )
})

testthat::test_that("rm_bin_summary() rejects outcome_col with wrong data type", {
  df <- dplyr::tibble(Ps = runif(10), outcome = letters[1:10])
  testthat::expect_error(
    traumar::rm_bin_summary(data = df, Ps_col = Ps, outcome_col = outcome),
    "outcome_col.*must be of class.*numeric, logical, integer"
  )
})

testthat::test_that("rm_bin_summary() validates bootstrap_ci is logical", {
  df <- dplyr::tibble(Ps = runif(10), outcome = rbinom(10, 1, 0.5))
  testthat::expect_error(
    traumar::rm_bin_summary(
      data = df,
      Ps_col = Ps,
      outcome_col = outcome,
      bootstrap_ci = "yes"
    ),
    "bootstrap_ci.*logical.*"
  )
})

testthat::test_that("rm_bin_summary() warns if seed is not numeric", {
  df <- dplyr::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    outcome = rbinom(1000, 1, 0.5)
  )
  testthat::expect_error(
    traumar::rm_bin_summary(
      data = df,
      Ps_col = Ps,
      outcome_col = outcome,
      seed = "foo"
    ),
    "seed.*must be of class.*numeric, integer"
  )
})

testthat::test_that("rm_bin_summary() validates group_vars are character vectors", {
  df <- dplyr::tibble(Ps = runif(10), outcome = rbinom(10, 1, 0.5))
  testthat::expect_error(
    traumar::rm_bin_summary(
      data = df,
      Ps_col = Ps,
      outcome_col = outcome,
      group_vars = list(1)
    ),
    "group_vars.*must be of class.*character.*factor"
  )
})

testthat::test_that("rm_bin_summary() validates group_vars exist in data", {
  df <- dplyr::tibble(Ps = runif(10), outcome = rbinom(10, 1, 0.5))
  testthat::expect_error(
    traumar::rm_bin_summary(
      data = df,
      Ps_col = Ps,
      outcome_col = outcome,
      group_vars = c("fake_col")
    ),
    "group_vars.*contains invalid column names.*fake_col.*Valid column names are.*Ps, outcome"
  )
})

testthat::test_that("rm_bin_summary validates inputs correctly", {
  # Test if data is a data frame
  testthat::expect_error(rm_bin_summary(
    data = NULL,
    Ps_col = Ps,
    outcome_col = survival
  ))

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )
  testthat::expect_error(rm_bin_summary(data = df, outcome_col = survival))

  # Test missing outcome_col
  testthat::expect_error(rm_bin_summary(data = df, Ps_col = Ps))

  # Test non-binary outcome_col
  df_non_binary <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = sample(1:3, 1000, replace = TRUE)
  )
  testthat::expect_error(rm_bin_summary(
    data = df_non_binary,
    Ps_col = Ps,
    outcome_col = survival
  ))

  # Test non-numeric Ps_col
  df_non_numeric <- tibble::tibble(Ps = c("a", "b", "c"), survival = c(1, 0, 1))
  testthat::expect_error(rm_bin_summary(
    data = df_non_numeric,
    Ps_col = Ps,
    outcome_col = survival
  ))

  # Test Ps values > 1
  df_ps_above_1 <- tibble::tibble(Ps = c(150, 80, 30), survival = c(1, 0, 1))
  testthat::expect_error(rm_bin_summary(
    data = df_ps_above_1,
    Ps_col = Ps,
    outcome_col = survival
  ))
})

testthat::test_that("nonlinear_bins produces correct bin data", {
  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  bin_data <- nonlinear_bins(
    df,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 5,
    divisor2 = 5,
    threshold_1 = 0.9,
    threshold_2 = 0.99
  )

  testthat::expect_true("bin_stats" %in% names(bin_data))
  testthat::expect_true("intervals" %in% names(bin_data))
  testthat::expect_equal(nrow(bin_data$bin_stats), 10) # assuming 5 bins
})

testthat::test_that("bootstrap data has the correct number of samples", {
  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  bootstrap_data <- df |>
    infer::generate(reps = 100, type = "bootstrap")

  testthat::expect_equal(nrow(bootstrap_data), 100 * nrow(df))
})

testthat::test_that("bin statistics are calculated correctly", {
  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  try_function <- rm_bin_summary(df, Ps, survival, n_samples = 5)

  testthat::expect_true("TA_b" %in% names(try_function))
  testthat::expect_true("TD_b" %in% names(try_function))
  testthat::expect_true("EM_b" %in% names(try_function))
})

testthat::test_that("RMM is calculated correctly", {
  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  rmm_result <- df |>
    rm_bin_summary(Ps, survival, n_samples = 5)

  testthat::expect_true("population_RMM" %in% names(rmm_result))
  testthat::expect_true(
    all(rmm_result$population_RMM >= -1) || all(rmm_result$population_RMM <= 1)
  )
})

testthat::test_that("confidence intervals are correctly computed in final RMM", {
  set.seed(10232015)

  # Test missing Ps_col
  df <- tibble::tibble(
    Ps = plogis(rnorm(10000, mean = 2, sd = 1.5)),
    survival = rbinom(10000, 1, prob = 0.9)
  )

  rmm_result <- rm_bin_summary(df, Ps, survival, n_samples = 100)

  testthat::expect_true("bootstrap_RMM_LL" %in% names(rmm_result))
  testthat::expect_true("bootstrap_RMM_UL" %in% names(rmm_result))
  testthat::expect_true("bootstrap_CI" %in% names(rmm_result))
  testthat::expect_true(all(
    rmm_result$population_RMM_UL > rmm_result$population_RMM_LL
  )) # CI upper should be greater than lower
})

testthat::test_that("RMM final data is correctly sorted by bin_number", {
  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  rmm_result_final <- rm_bin_summary(df, Ps, survival, n_samples = 5)

  testthat::expect_equal(min(rmm_result_final$bin_number), 1)
  testthat::expect_equal(max(rmm_result_final$bin_number), 10) # Assuming 10 bins
})

testthat::test_that("rm_bin_summary validates group_vars correctly", {
  set.seed(01232025)

  # Test valid grouping
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9),
    group = sample(letters[1:5], 1000, replace = TRUE)
  )

  grouped_result <- rm_bin_summary(
    df,
    Ps,
    survival,
    group_vars = "group",
    n_samples = 5
  )
  testthat::expect_true("group" %in% names(grouped_result))
  testthat::expect_true(all(grouped_result$group %in% unique(df$group)))

  # Test invalid group_vars (non-existent column)
  testthat::expect_error(rm_bin_summary(
    df,
    Ps,
    survival,
    group_vars = non_existent_col
  ))
})

testthat::test_that("rm_bin_summary validates Ps_col and outcome_col correctly", {
  # Custom expectation function to test for the presence of a specific warning pattern
  expect_warning_present <- function(expr, pattern) {
    # Initialize an empty character vector to store warning messages
    warnings <- character()

    # Evaluate the expression while intercepting all warning conditions
    withCallingHandlers(
      # Force evaluation of the expression to ensure any lazy evaluation is triggered
      force(expr),

      # Define a handler specifically for warning conditions
      warning = function(w) {
        # Append the current warning message to the 'warnings' vector
        warnings <<- c(warnings, conditionMessage(w))

        # Prevent the warning from being printed to the console during tests
        invokeRestart("muffleWarning")
      }
    )

    # Assert that at least one warning matches the specified regular expression pattern
    testthat::expect_true(
      # Logical test: does any warning message match the provided pattern?
      any(stringr::str_detect(warnings, pattern)),

      # Message to display if the expectation fails
      info = paste("Expected warning matching:", pattern)
    )
  }

  # Set seed for reproducibility
  set.seed(01232025)

  # Create a dataset with valid grouping and complete data
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), # Probability scores (Ps)
    survival = rbinom(1000, 1, prob = 0.9), # Binary outcome variable
    group = sample(letters[1:5], 1000, replace = TRUE) # Random group assignment (5 groups)
  )

  # Introduce missing values into the Ps column to create an invalid input scenario
  bad_ps <- df |>
    dplyr::mutate(
      Ps = c(rep(NA, 50), plogis(rnorm(950, mean = 2, sd = 1.5)))
    )

  # Expect a warning for missing values in Ps_col when running rm_bin_summary()
  expect_warning_present(
    rm_bin_summary(
      bad_ps,
      Ps,
      survival,
      n_samples = 5
    ),
    "Ps_col.*missing values detected"
  )

  # Introduce missing values into the outcome (survival) column
  bad_outcome <- df |>
    dplyr::mutate(
      survival = c(rep(NA, 50), rbinom(950, size = 1, prob = 0.9))
    )

  # Expect a warning for missing values in outcome_col when running rm_bin_summary()
  expect_warning_present(
    rm_bin_summary(
      bad_outcome,
      Ps,
      survival,
      n_samples = 5
    ),
    "outcome_col.*missing values detected"
  )
})

testthat::test_that("rm_bin_summary handles the seed argument correctly", {
  # Test consistency with seed
  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  result1 <- rm_bin_summary(df, Ps, survival, n_samples = 5, seed = 12345)
  result2 <- rm_bin_summary(df, Ps, survival, n_samples = 5, seed = 12345)

  # Ensure the results are the same for the same seed
  testthat::expect_equal(result1, result2)

  # Test different seed values result in different outputs
  result3 <- rm_bin_summary(df, Ps, survival, n_samples = 5, seed = 67890)
  testthat::expect_false(identical(result1, result3))
})

testthat::test_that("rm_bin_summary handles extreme edge cases for Ps values", {
  # Test edge cases for Ps column (extremely low/high values)
  df_extreme <- tibble::tibble(Ps = c(1e-10, 1, 1e+10), survival = c(1, 0, 1))

  # Ensure that error is thrown with values above 1 or less than 0
  testthat::expect_error(rm_bin_summary(
    df_extreme,
    Ps,
    survival,
    n_samples = 5
  ))
})

testthat::test_that("rm_bin_summary produces error with invalid n_samples", {
  set.seed(01232025)

  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  # Invalid n_samples (non-integer, negative)
  testthat::expect_error(rm_bin_summary(df, Ps, survival, n_samples = -1))
  testthat::expect_error(rm_bin_summary(df, Ps, survival, n_samples = 3.5))
})

testthat::test_that("rm_bin_summary with missing group_vars argument", {
  set.seed(01232025)

  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = rbinom(1000, 1, prob = 0.9)
  )

  # Ensure no error when group_vars is not supplied
  no_group_result <- rm_bin_summary(df, Ps, survival, n_samples = 5)
  testthat::expect_true("bin_number" %in% names(no_group_result))
  testthat::expect_true(nrow(no_group_result) > 0)
})

testthat::test_that("rm_bin_summary has correct validation of outcome_col when it is logical", {
  set.seed(01232025)

  df <- tibble::tibble(
    Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
    survival = sample(
      x = c(TRUE, FALSE),
      size = 1000,
      replace = TRUE,
      prob = c(0.9, 0.1)
    )
  )

  testthat::expect_no_error(
    rm_bin_summary(data = df, Ps_col = Ps, outcome_col = survival)
  )
})
