# Tests for nonlinear_bins function

testthat::test_that("nonlinear_bins handles missing values and invalid data types", {
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Inject missing values into Ps
  data_na_Ps <- data
  data_na_Ps$Ps[1:5] <- NA
  testthat::expect_warning(
    nonlinear_bins(data = data_na_Ps, Ps_col = Ps, outcome_col = survival),
    regexp = "Missing values detected in .*Ps_col.*"
  )

  # Inject missing values into outcome_col
  data_na_surv <- data
  data_na_surv$survival[1:5] <- NA
  testthat::expect_warning(
    nonlinear_bins(data = data_na_surv, Ps_col = Ps, outcome_col = survival),
    regexp = "Missing values detected in .*outcome_col.*"
  )

  # Invalid logical values
  data_logical_invalid <- data |>
    dplyr::mutate(survival = as.logical(survival)) |>
    dplyr::mutate(survival = replace(survival, 1:3, NA)) # Valid NAs only
  testthat::expect_warning(
    nonlinear_bins(
      data = data_logical_invalid,
      Ps_col = Ps,
      outcome_col = survival
    ),
    regexp = "Missing values detected in .*outcome_col.*"
  )
})

testthat::test_that("nonlinear_bins handles error messaging appropriately", {
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Missing Ps_col and outcome_col arguments
  testthat::expect_error(
    nonlinear_bins(data = data),
    regexp = "Both.*Ps_col.*outcome_col.*must be provided"
  )

  # Missing Ps_col only
  testthat::expect_error(
    nonlinear_bins(data = data, outcome_col = survival),
    regexp = "Ps_col.*must be provided"
  )

  # Missing outcome_col only
  testthat::expect_error(
    nonlinear_bins(data = data, Ps_col = Ps),
    regexp = "outcome_col.*must be provided"
  )

  # Set up the survival variable so that it is of class character
  bad_data <- data |> dplyr::mutate(survival = ifelse(survival == 1, "a", "b"))

  # Test a character survival column
  testthat::expect_error(
    nonlinear_bins(data = bad_data, Ps_col = Ps, outcome_col = survival),
    regexp = "outcome_col.*must be of type logical"
  )

  # Test the case when the data are not sufficiently dispersed and cause errors
  # usually when the data do not come from the same distribution as the
  # probability of survival metric
  bad_data <- data.frame(
    Ps = rbinom(10, 1, prob = 0.9),
    survival = rbinom(10, 1, 0.9)
  )

  testthat::expect_error(
    nonlinear_bins(
      data = bad_data,
      Ps_col = Ps,
      outcome_col = survival,
      divisor1 = 4,
      divisor2 = 4
    ),
    regexp = "Unable to calculate valid step sizes"
  )
})

testthat::test_that("nonlinear_bins handles basic functionality correctly", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Run the function
  result <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 5,
    divisor2 = 5,
    threshold_1 = 0.9,
    threshold_2 = 0.99
  )

  # Check intervals
  testthat::expect_type(result$intervals, "double")
  testthat::expect_true(length(result$intervals) > 1)

  # Check bin_stats structure
  testthat::expect_s3_class(result$bin_stats, "tbl_df")
  testthat::expect_named(
    result$bin_stats,
    c(
      "bin_number",
      "bin_start",
      "bin_end",
      "mean",
      "sd",
      "Pred_Survivors_b",
      "Pred_Deaths_b",
      "AntiS_b",
      "AntiM_b",
      "alive",
      "dead",
      "count",
      "percent"
    )
  )
})

testthat::test_that("nonlinear_bins produces reasonable bins for uniform data", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Run the function
  result <- nonlinear_bins(data, Ps_col = Ps, outcome_col = survival)

  # Check intervals cover correct range
  testthat::expect_true(min(result$intervals) > 0)
  testthat::expect_true(max(result$intervals) < 1)

  # Check bin stats are populated
  testthat::expect_true(all(result$bin_stats$count > 0))
})

testthat::test_that("nonlinear_bins respects divisor and threshold parameters", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Run with different divisors and thresholds
  result1 <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 10,
    divisor2 = 10
  )
  result2 <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 2,
    divisor2 = 2
  )

  # Check intervals differ based on divisors
  testthat::expect_true(length(result1$intervals) > length(result2$intervals))

  # Check thresholds affect intervals
  result3 <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    threshold_1 = 0.8,
    threshold_2 = 0.95
  )
  testthat::expect_true(length(result3$intervals) < length(result1$intervals))
})

testthat::test_that("nonlinear_bins handles invalid inputs gracefully", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Missing column
  testthat::expect_error(nonlinear_bins(
    data,
    Ps_col = NotAColumn,
    outcome_col = survival
  ))

  # Non-numeric column, warning will happen
  data <- data.frame(Ps = c("d", "e", "f"), survival = c("a", "b", "c"))
  testthat::expect_error(nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival
  ))

  # Check a logical outcome_col is binary
})

testthat::test_that("nonlinear_bins produces accurate bin statistics", {
  # Generate example data
  set.seed(123)

  # Parameters
  # Total number of patients
  n_patients <- 1000

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
  data <- data.frame(Ps = Ps, survival = survival_outcomes, groups = groups) |>
    dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))

  # Run the function
  result <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 3,
    divisor2 = 3
  )

  # Validate statistics by comparing the summarized values
  result_summary <- result$bin_stats

  # Select and sort the column
  survival_data <- data |> dplyr::pull(Ps) |> sort()
  total <- length(survival_data)

  # Step 1: Find indices for level thresholds
  loc_9A <- which(survival_data > 0.9) # Everything above 0.9 or other threshold
  loc_9B <- which(survival_data > 0.99) # Everything above 0.99 or other threshold
  loc_9C <- which(survival_data > 0.9 & survival_data <= 0.99) # Between 0.9 and 0.99 or other thresholds

  # Step 2: Define step sizes based on the data
  step1 <- round(min(loc_9A, na.rm = TRUE) / 3)
  step2 <- round(length(loc_9C) / 3)

  # Step 3: Define intervals
  # Check that loc_9A and loc_9B are finite before using them in seq()
  len <- unique(c(
    seq(1, min(loc_9A, na.rm = TRUE), by = step1), # From start to level_1
    seq(min(loc_9A, na.rm = TRUE), min(loc_9B, na.rm = TRUE), by = step2), # From level_1 to level_2
    max(loc_9B, na.rm = TRUE) # Up to max
  ))

  # Generate intervals based on these positions
  intervals <- unique(survival_data[len])

  # Create the summarized objects
  # Apply binning to each group separately
  data <- data |>
    dplyr::mutate(
      bin_number = .bincode(Ps, breaks = intervals, include.lowest = TRUE),
      bin_start = intervals[bin_number], # Start of the bin
      bin_end = intervals[bin_number + 1] # End of the bin
    )

  # Optionally group data by dynamic group_vars
  # Or run the bin statistics on the whole dataset
  grouped_stats <- data |>
    dplyr::group_by(bin_number, bin_start, bin_end) |>
    dplyr::summarize(
      mean = mean(Ps, na.rm = TRUE),
      sd = stats::sd(Ps, na.rm = TRUE),
      Pred_Survivors_b = sum(Ps, na.rm = TRUE),
      Pred_Deaths_b = sum(1 - Ps, na.rm = TRUE),
      AntiS_b = dplyr::if_else(
        dplyr::n() > 0,
        Pred_Survivors_b / dplyr::n(),
        NA_real_
      ),
      AntiM_b = dplyr::if_else(
        dplyr::n() > 0,
        Pred_Deaths_b / dplyr::n(),
        NA_real_
      ),
      alive = sum(survival == 1, na.rm = TRUE),
      dead = sum(survival == 0, na.rm = TRUE),
      count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      percent = count / sum(count, na.rm = TRUE),
    ) |>
    dplyr::ungroup()

  # Validate the statistics
  testthat::expect_equal(grouped_stats, result_summary)
})

testthat::test_that("nonlinear_bins handles group_vars argument correctly", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  group_var <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  data <- data.frame(Ps = Ps, survival = survival, group_var = group_var)

  # Run the function with group_vars
  result <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 5,
    divisor2 = 5,
    group_vars = "group_var"
  )

  # Ensure the result is grouped correctly by the specified variable
  testthat::expect_true(all(
    levels(factor(result$bin_stats$group_var)) %in% unique(group_var)
  ))
  testthat::expect_equal(
    length(unique(result$bin_stats$group_var)),
    length(unique(group_var))
  )
})

testthat::test_that("nonlinear_bins handles empty group_vars correctly", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  data <- data.frame(Ps = Ps, survival = survival)

  # Run the function without group_vars
  result <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 5,
    divisor2 = 5
  )

  # Ensure no grouping is performed
  testthat::expect_false("group_var" %in% names(result$bin_stats))
})

testthat::test_that("nonlinear_bins handles multiple group_vars correctly", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  group_var1 <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  group_var2 <- sample(1:3, 1000, replace = TRUE)
  data <- data.frame(
    Ps = Ps,
    survival = survival,
    group_var1 = group_var1,
    group_var2 = group_var2
  )

  # Run the function with multiple group_vars
  result <- rm_bin_summary(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    Divisor1 = 3,
    Divisor2 = 3,
    group_vars = c("group_var1", "group_var2")
  )

  # Ensure the result is grouped correctly by the specified variables
  testthat::expect_equal(
    length(unique(result$group_var1)),
    length(unique(group_var1))
  )
  testthat::expect_equal(
    length(unique(result$group_var2)),
    length(unique(group_var2))
  )
})

testthat::test_that("nonlinear_bins handles group_vars with no variability", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  group_var <- rep("A", 1000) # No variability in group_var
  data <- data.frame(Ps = Ps, survival = survival, group_var = group_var)

  # Run the function with group_vars
  result <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 5,
    divisor2 = 5,
    group_vars = "group_var"
  )

  # Check that result is not grouped by a constant variable
  testthat::expect_equal(length(unique(result$bin_stats$group_var)), 1)
})

testthat::test_that("nonlinear_bins correctly handles group_vars when there are few groups", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  group_var <- sample(c("A", "B"), 1000, replace = TRUE)
  data <- data.frame(Ps = Ps, survival = survival, group_var = group_var)

  # Run the function with a few group variables
  result <- nonlinear_bins(
    data,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 5,
    divisor2 = 5,
    group_vars = "group_var"
  )

  # Ensure that the bin statistics are separated by group_var
  testthat::expect_true(length(unique(result$bin_stats$group_var)) == 2)
})

testthat::test_that("nonlinear_bins performs all validation checks correctly", {
  # Generate example data
  set.seed(123)
  Ps <- plogis(rnorm(1000, mean = 2, sd = 1.5))
  survival <- rbinom(1000, 1, prob = 0.9)
  group_var1 <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  group_var2 <- sample(1:3, 1000, replace = TRUE)
  data <- data.frame(
    Ps = Ps,
    survival = survival,
    group_var1 = group_var1,
    group_var2 = group_var2
  )

  # Test for non-data.frame input
  testthat::expect_error(
    nonlinear_bins(matrix(1:10, ncol = 2), Ps_col = Ps, outcome_col = survival),
    "The input data must be a data frame or tibble."
  )

  # Test for missing Ps_col and outcome_col
  testthat::expect_error(
    nonlinear_bins(data, Ps_col = NULL, outcome_col = NULL)
  )

  testthat::expect_error(
    nonlinear_bins(data, Ps_col = NULL, outcome_col = survival)
  )

  testthat::expect_error(
    nonlinear_bins(data, Ps_col = Ps, outcome_col = NULL)
  )

  # Test for non-numeric Ps_col
  testthat::expect_error(
    nonlinear_bins(data, Ps_col = "group_var1", outcome_col = survival)
  )

  # Test for Ps values outside the [0, 1] range
  data_invalid_ps <- data
  data_invalid_ps$Ps <- c(-0.1, 1.1)
  testthat::expect_error(
    nonlinear_bins(data_invalid_ps, Ps_col = Ps, outcome_col = survival)
  )

  # Test for non-binary outcome_col
  data_invalid_outcome <- data
  data_invalid_outcome$survival <- sample(1:3, 1000, replace = TRUE)
  testthat::expect_error(
    nonlinear_bins(data_invalid_outcome, Ps_col = Ps, outcome_col = survival),
    "contains numeric values other than 0 and 1"
  )

  # Test for non-character group_vars
  testthat::expect_error(
    nonlinear_bins(
      data,
      Ps_col = Ps,
      outcome_col = survival,
      group_vars = list(1, 2)
    ),
    "must be strings."
  )

  # Test for non-existent group_vars
  testthat::expect_error(
    nonlinear_bins(
      data,
      Ps_col = Ps,
      outcome_col = survival,
      group_vars = c("group_var1", "non_existent_var")
    ),
    "are not valid columns in the data: non_existent_var"
  )
})
