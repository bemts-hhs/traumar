#' Create Nonlinear Probability of Survival Bins
#'
#' This function generates nonlinear bins for probability of survival data based
#' on specified thresholds and divisors. It calculates bin statistics, including
#' mean, standard deviation, total alive, total dead, count, and percentage for
#' each bin.
#'
#' @param data A `data.frame` or `tibble` containing the probability of survival
#'   data for a set of patients.
#' @param Ps_col The column in `data` containing the probability of survival
#'   values for a set of patients.
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   should represent "alive" (survived), while `0` should
#'   represent "dead" (did not survive). Ensure the column contains only these
#'   two possible values.
#' @param divisor1 A parameter to control the width of the probability of
#'   survival range bins. Affects the creation of step sizes for the beginning
#'   of each bin range. Defaults to `5`.
#' @param divisor2 A parameter to control the width of the probability of
#'   survival range bins. Affects the creation of step sizes for the beginning
#'   of each bin range. Defaults to `5`.
#' @param threshold_1 A parameter to decide where data indices will begin to
#'   create step sizes. Defaults to `0.9`.
#' @param threshold_2 A parameter to decide where data indices will end to
#'   create step sizes. Defaults to `0.99`.
#'
#' @returns A list with `intervals` and `bin_stats` objects:
#' * `intervals`: A vector of start and end-points for the probability of survival bin ranges.
#' * `bin_stats`: A `tibble` with columns `bin_number`, `bin_start`, `bin_end`, `mean`, `sd`,
#' `alive`, `dead`, `count`, and `percent`.
#'
#' @export
#'
#' @examples
#' # Generate example data with high negative skewness
#' set.seed(123)
#'
#' # Parameters
#' n_patients <- 10000  # Total number of patients
#'
#' # Skewed towards higher values
#' Ps <- plogis(rnorm(n_patients, mean = 2, sd = 1.5))
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients,
#'                             size = 1,
#'                             prob = Ps
#'                             )
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
#' dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Apply the nonlinear_bins function
#' results <- nonlinear_bins(data = data,
#'                           Ps_col = Ps,
#'                           outcome_col = survival,
#'                           divisor1 = 5,
#'                           divisor2 = 5,
#'                           threshold_1 = 0.9,
#'                           threshold_2 = 0.99)
#'
#' # View results
#' results$intervals
#' results$bin_stats
#'
#' @author Nicolas Foss, Ed.D, MS, original paper and code in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
nonlinear_bins <- function(data,
                           Ps_col,
                           outcome_col,
                           divisor1 = 5,
                           divisor2 = 5,
                           threshold_1 = 0.9,
                           threshold_2 = 0.99
                           ) {

  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort("The input data must be a data frame or tibble.")
  }

  # No explicit validation for column existence; use tidy evaluation directly
  ps_data <- rlang::enquo(Ps_col)     # Capture Ps_col argument
  outcome_data <- rlang::enquo(outcome_col) # Capture outcome_col argument

  # Ensure Ps_col and outcome_col arguments are provided with tailored error messages
  if (missing(Ps_col) && missing(outcome_col)) {
    cli::cli_abort("Both {.var Ps_col} and {.var outcome_col} arguments must be provided.")
  } else if (missing(Ps_col)) {
    cli::cli_abort("The {.var Ps_col} argument must be provided.")
  } else if (missing(outcome_col)) {
    cli::cli_abort("The {.var outcome_col} argument must be provided.")
  }

  # Check if the outcome_col is binary
  binary_data <- data |>
    dplyr::pull({{ outcome_col }})

  # Validate binary data
  unique_values <- unique(stats::na.omit(binary_data))

  if (!all(unique_values %in% c(0, 1, TRUE, FALSE)) || length(unique_values) > 2 || length(unique_values) < 2) {
    cli::cli_abort("The {.var outcome_col} must be binary, such as 1/0, TRUE/FALSE, or a combination of these. Ensure the column has a binary structure.")
  }

  # Select and sort the column
  survival_data <- data |> dplyr::pull({{ Ps_col }}) |> sort()
  total <- length(survival_data)

  # Step 1: Find indices for level thresholds
  loc_9A <- which(survival_data > threshold_1) # Everything above 0.9 or other threshold
  loc_9B <- which(survival_data > threshold_2) # Everything above 0.99 or other threshold
  loc_9C <- which(survival_data > threshold_1 & survival_data <= threshold_2) # Between 0.9 and 0.99 or other thresholds

  # Step 2: Define step sizes based on the data
  step1 <- round(min(loc_9A) / divisor1)
  step2 <- round(length(loc_9C) / divisor2)

  # Step 3: Define intervals
  len <- unique(c(
    seq(1, min(loc_9A), by = step1),        # From start to level_1
    seq(min(loc_9A), min(loc_9B), by = step2), # From level_1 to level_2
    max(loc_9B)                             # Up to max
  ))

  # Generate intervals based on these positions
  intervals <- unique(survival_data[len])

  # Step 4: Bin statistics
  # Create empty tibble for bin stats
  bin_stats <- tibble::tibble(
    bin_number = seq_along(head(intervals, -1)),
    bin_start = utils::head(intervals, -1),
    bin_end = utils::tail(intervals, -1),
    mean = numeric(length(intervals) - 1),
    sd = numeric(length(intervals) - 1),
    alive = numeric(length(intervals) - 1),
    dead = numeric(length(intervals) - 1),
    count = numeric(length(intervals) - 1),
    percent = numeric(length(intervals) - 1)
  )


  # Populate bin stats
  for (i in seq_len(nrow(bin_stats))) {
    bin_data <- survival_data[survival_data >= bin_stats$bin_start[i] &
                                (survival_data < bin_stats$bin_end[i] | i == nrow(bin_stats))]
    if (length(bin_data) > 0) {
      bin_stats$bin_number[i] <- i
      bin_stats$mean[i] <- mean(bin_data)
      bin_stats$sd[i] <- stats::sd(bin_data)
      bin_stats$count[i] <- length(bin_data)
      bin_stats$percent[i] <- round(length(bin_data) / total, digits = 3)

      # Use quasiquotation to refer to columns correctly
      bin_outcome <- data |> dplyr::filter({{ Ps_col }} %in% bin_data) |> dplyr::pull({{ outcome_col }})

      # Add alive and dead counts based on the outcome column
      bin_stats$alive[i] <- sum(bin_outcome == 1)   # Count the number of alive (1)
      bin_stats$dead[i] <- sum(bin_outcome == 0)    # Count the number of dead (0)

    } else {
      bin_stats$mean[i] <- NA
      bin_stats$sd[i] <- NA
      bin_stats$count[i] <- 0
      bin_stats$alive[i] <- 0  # Ensure the 'alive' column is set to 0 for empty bins
      bin_stats$dead[i] <- 0   # Similarly, set 'dead' to 0 for empty bins
    }
  }

  return(list(intervals = intervals, bin_stats = bin_stats))

}

