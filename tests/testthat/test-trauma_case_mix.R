# Test 1: Basic functionality with valid input data
testthat::test_that("Valid input returns correct output", {
  set.seed(123)
  n_patients <- 100
  Ps <- plogis(rnorm(n_patients, mean = 2, sd = 1.5))
  survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
  data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
    dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))

  result <- trauma_case_mix(data, Ps_col = Ps, outcome_col = death)
  testthat::expect_equal(ncol(result), 8)
  testthat::expect_true("Ps_range" %in% colnames(result))
  testthat::expect_true("current_fraction" %in% colnames(result))
  testthat::expect_true("MTOS_distribution" %in% colnames(result))
})

# Test 2: Handles Ps values greater than 1, converting to decimal
testthat::test_that("Ps values > 1 cause an error", {
  set.seed(123)
  surv_probs <- sample(1:100, 100, replace = T)
  survival_outcomes <- rbinom(100, size = 1, prob = 0.9)
  data <- data.frame(Ps = surv_probs, death = survival_outcomes)

  testthat::expect_error(
    trauma_case_mix(data, Ps_col = Ps, outcome_col = death),
    regexp = "values must be between 0 and 1"
  )
})

# Test 3: Invalid outcome column (non-binary)
testthat::test_that("Invalid binary outcome column throws error", {
  set.seed(123)
  n_patients <- 100
  Ps <- plogis(rnorm(n_patients, mean = 2, sd = 1.5))
  survival_outcomes <- rnorm(n_patients, mean = 0.5, sd = 0.2) # Non-binary outcome
  data <- data.frame(Ps = Ps, survival = survival_outcomes)

  testthat::expect_error(
    trauma_case_mix(data, Ps_col = Ps, outcome_col = survival),
    regexp = "contains numeric values other than 0 and 1"
  )
})

# Test 4: Invalid Ps column (non-numeric)
testthat::test_that("Non-numeric Ps column throws error", {
  set.seed(123)
  n_patients <- 100
  Ps <- letters[1:100] # Non-numeric Ps values
  survival_outcomes <- rbinom(n_patients, size = 1, prob = 0.7)
  data <- data.frame(Ps = Ps, survival = survival_outcomes)

  testthat::expect_error(
    trauma_case_mix(data, Ps_col = Ps, outcome_col = survival),
    regexp = "column must be numeric"
  )
})

# Test 5: Ps column contains values outside the expected range (0 to 100)
testthat::test_that("Ps values outside 0-1 range throw error", {
  set.seed(123)
  n_patients <- 10
  Ps <- c(5, 10, 20, 105, -10, 0.85, -3, 200, 1.5, 80) # Invalid Ps values
  survival_outcomes <- rbinom(n_patients, size = 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
    dplyr::mutate(death = dplyr::if_else(survival >= 0.5, 0, 1))

  testthat::expect_error(
    trauma_case_mix(data, Ps_col = Ps, outcome_col = death),
    regexp = "values must be between 0 and 1"
  )
})

# Test 6: Non-data frame input should throw an error
testthat::test_that("Invalid df input throws error", {
  testthat::expect_error(
    trauma_case_mix(NULL, Ps_col = Ps, outcome_col = death),
    "The first argument must be a dataframe"
  )
})

# Test 7: Valid input data with missing values (NA) in Ps_col or outcome_col
testthat::test_that("NA values are correctly handled", {
  set.seed(123)
  n_patients <- 100
  Ps <- c(0.85, NA, 0.75, 0.6, 0.91)
  survival_outcomes <- c(1, 0, 0, 1, 0)
  data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
    dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))

  testthat::expect_warning(
    trauma_case_mix(data, Ps_col = Ps, outcome_col = death),
    regexp = "please apply an appropriate treatment to the missings and rerun"
  )

  Ps <- c(0.85, 0.2, 0.75, 0.6, 0.91)
  survival_outcomes <- c(1, NA, 0, 1, 0)
  data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
    dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
  testthat::expect_warning(
    trauma_case_mix(data, Ps_col = Ps, outcome_col = death),
    regexp = "please apply an appropriate treatment to the missings and rerun"
  )
})

# Test 8: non-logical and non-numeric argument to outcome_col
testthat::test_that("non-logical and non-numeric argument to outcome_col do not work", {
  set.seed(123)
  n_patients <- 100
  Ps <- c(0.85, NA, 0.75, 0.6, 0.91)
  survival_outcomes <- as.character(c(1, 0, 0, 1, 0))
  data <- data.frame(Ps = Ps, survival = survival_outcomes)

  testthat::expect_error(
    trauma_case_mix(data, Ps_col = Ps, outcome_col = survival),
    regexp = "must be of type logical.*or numeric"
  )
})
