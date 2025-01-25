#' @title Relative Mortality Metric (RMM) Calculation
#'
#' @description
#'
#' Calculates the Relative Mortality Metric (RMM) from Napoli et al. (2017)
#' based on patient survival probabilities (Ps) and actual outcomes. The
#' function groups patients into bins based on their survival probability scores
#' (Ps) and computes a weighted mortality metric along with confidence
#' intervals.
#'
#' The Relative Mortality Metric (RMM) quantifies the performance of a center in
#' comparison to the anticipated mortality based on the TRISS national
#' benchmark. The RMM measures the difference between observed and expected
#' mortality, with a range from -1 to 1.
#'
#' - An RMM of 0 indicates that the observed mortality aligns with the expected
#' national benchmark across all acuity levels.
#' - An RMM greater than 0 indicates better-than-expected performance, where
#' the center is outperforming the national benchmark.
#' - An RMM less than 0 indicates under-performance, where the center’s observed
#' mortality is higher than the expected benchmark.
#'
#' This metric helps assess how a center's mortality compares to the national
#' standards, guiding quality improvement efforts.  `rmm()` utilizes bootstrap
#' sampling to calculate the confidence intervals via the standard error method.
#'
#' @param data A data frame or tibble containing the data.
#' @param Ps_col The name of the column containing the survival probabilities
#'   (Ps). Should be numeric (values between 0 and 100).
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   should represent "alive" (survived), while `0` should represent "dead"
#'   (did not survive). Ensure the column contains only these two possible values.
#' @param n_samples A numeric value indicating the number of bootstrap samples
#' to take from the data source.
#' @param Divisor1 A divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Divisor2 A second divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Threshold_1 The first threshold for dividing the survival
#'   probabilities (default is 0.9).
#' @param Threshold_2 The second threshold for dividing the survival
#'   probabilities (default is 0.99).
#' @param pivot A logical indicating whether to return the results in a long
#'   format (pivot = TRUE) or wide format (pivot = FALSE, default).
#'
#' @returns A tibble containing the Relative Mortality Metric (RMM) and related
#'   statistics:
#'   - `population_RMM`: The final calculated Relative Mortality Metric for the population
#'   existing in `data`.
#'   - `lower_ci`: The lower bound of the 95% confidence interval calculated via a bootstrap
#'   sample.
#'   - `boostrap_RMM`: The average RMM value calculated for the boostrap sample.
#'   - `upper_ci`: The upper bound of the 95% confidence interval calculated via a bootstrap
#'   sample.
#'   - `sd_RMM`: The standard deviation of the RMM for the bootstrap distribution.
#'   - `se_RMM`: The standard error of the RMM for the bootstrap distribution.
#'   - If `pivot = TRUE`, the results will be in long format with two columns: `stat`
#'   and `value`, where each row corresponds to one of the calculated
#'   statistics.
#'   - If `pivot = FALSE` (default), the results will be returned in wide format,
#'   with each statistic as a separate column.
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
#' # Example usage of the `rmm` function
#' rmm(data = data, Ps_col = Ps,
#'     outcome_col = survival,
#'     Divisor1 = 5,
#'     Divisor2 = 5
#'     )
#'
#' # pivot!
#' rmm(data = data, Ps_col = Ps,
#'     outcome_col = survival,
#'     Divisor1 = 5,
#'     Divisor2 = 5,
#'     pivot = TRUE
#'     )
#'
#' @author Nicolas Foss, Ed.D, MS, original paper and code in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
rmm <- function(data,
                Ps_col,
                outcome_col,
                n_samples = 1000,
                Divisor1 = 5,
                Divisor2 = 5,
                Threshold_1 = 0.9,
                Threshold_2 = 0.99,
                pivot = FALSE
                ) {

  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort("The input data must be a data frame or tibble.")
  }

  # check the n_samples value
  if(!is.numeric(n_samples) && !is.integer(n_samples)) {

    cli::cli_abort("A value of class {.cls numeric} must be passed to {.var n_samples}. The value passed to {.var n_samples} was of class {.val {class(n_samples)}}, please provide a {.cls numeric} value.")

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

  # Check if Ps column is numeric

  # dplyr::pull the Ps data
  Ps_check <- data |> dplyr::pull({{ Ps_col }})

  # check the Ps_check remains continuous
  if (!is.numeric(Ps_check)) {
    cli::cli_abort("The {.var Ps_col} must contain numeric values.")
  }

  if (any(is.na(Ps_check))) {
    cli::cli_warn("Missing values detected in {.var Ps_col}; they will be ignored in calculations.")
  }

  # Check if Ps column is continuous (values between 0 and 1 or 0 and 100)
  if (any(Ps_check < 0 | Ps_check > 100, na.rm = T)) {
    cli::cli_abort("The probability of survival (Ps) values must be between 0 and 100.")
  }

  # Notify the user and convert Ps values if necessary
  if (any(Ps_check > 1, na.rm = TRUE)) {

    cli::cli_alert_info("Probability of survival (Ps) values will be divided by 100 to convert to decimal format.")

    data <- data |>
      dplyr::mutate(!!rlang::ensym(Ps_col) := dplyr::if_else(!!ps_data > 1, !!ps_data / 100, !!ps_data))
  }

  # Assume same distribution of POS scores over years
  # Dynamically assign bins for POS scores using non-linear process
  # specified by Napoli et al. 2017
  # those methods are adapted using this function

  # get the population level bins
  bin_data <- nonlinear_bins(
    data,
    Ps_col = {{ Ps_col }},
    outcome_col = {{ outcome_col }},
    divisor1 = Divisor1,
    divisor2 = Divisor2,
    threshold_1 = Threshold_1,
    threshold_2 = Threshold_2
  )

  set.seed(10232015)

  # Bootstrap process
  bootstrap_data <- data |>
    dplyr::select({{ Ps_col }}, {{ outcome_col }}) |>  # Select only relevant columns
    infer::generate(reps = n_samples, type = "bootstrap") |>   # Generate bootstrap samples
    dplyr::ungroup()

  # bootstrapping to get bins for the population to then create
  # the confidence intervals
  # Nest data by replicate and apply nonlinear_bins
  bin_data_boot <- bootstrap_data |>
    tidyr::nest(data = -replicate) |>  # Nest data by replicate
    dplyr::mutate(
      bins = purrr::map(
        data,
        ~ nonlinear_bins(
          .x, Ps_col = {{ Ps_col }}, outcome_col = {{ outcome_col }},
          divisor1 = Divisor1,
          divisor2 = Divisor2,
          threshold_1 = Threshold_1,
          threshold_2 = Threshold_2
        )
      )
    ) |>
    dplyr::mutate(
      bins_temp = purrr::map(bins, ~ .x$bin_stats)
    ) |>
    tidyr::unnest(bins_temp) |>
    dplyr::select(-bins)

  # Extract the bin intervals (start and end points of the bins)
  intervals_data <- bin_data$intervals

  # Initialize the bin_df to hold bin statistics
  bin_df <- bin_data$bin_stats |>
    dplyr::select(bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin using the start and end points
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(bin_number) # Sort the bins by bin_number

  # Initialize the bind_df_boot to hold bin statistics for each replicate
  # Initialize the bin_df with bootstrap samples
  bin_df_boot <- bin_data_boot |>
    dplyr::select(replicate, bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin for each bootstrap replicate
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(replicate, bin_number) # Sort by replicate and bin_number

  # Summarize bin-level statistics:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary <- bin_data$bin_stats |>
    dplyr::summarize(
      TA_b = sum(alive, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum(dead, na.rm = TRUE), # Total number of deaths in the bin
      N_b = sum(count), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)
      .by = bin_number # Perform this calculation for each bin
    ) |>
    dplyr::arrange(bin_number) # Arrange the bins by bin_number

  # Summarize bin-level statistics for the boostrapped data:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary_boot <- bin_data_boot |>
    dplyr::summarize(
      TA_b = sum(alive, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum(dead, na.rm = TRUE), # Total number of deaths in the bin
      N_b = sum(count), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)

      # Perform this calculation for each replicate and bin
      .by = c(replicate, bin_number)
    ) |>
    dplyr::arrange(replicate, bin_number) # Arrange the bins by bin_number

  # Join the bin statistics (bin_summary) with the bin_df for further calculations
  # The merged data will contain the bin information and corresponding statistics
  bin_stats <- bin_summary |>
    dplyr::left_join(bin_df, by = "bin_number") |>
    dplyr::mutate(
      R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
      AntiM_b = -1 * midpoint + 1, # Anticipated mortality (1 - midpoint, reversed scale)
      .by = bin_number
    )

  # For the bootstrapped data
  # Join the bin statistics (bin_summary) with the bin_df_boot for further calculations
  # The merged data will contain the bin information and corresponding statistics
  bin_stats_boot <- bin_summary_boot |>
    dplyr::left_join(bin_df_boot, by = c("replicate", "bin_number")) |>
    dplyr::mutate(
      R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
      AntiM_b = -1 * midpoint + 1, # Anticipated mortality (1 - midpoint, reversed scale)
      .by = c(replicate, bin_number)
    )

  # Calculate the Relative Mortality Metric (RMM):
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  rmm_result <- bin_stats |>
    dplyr::summarize(
      numerator = sum(R_b * (AntiM_b - EM_b), na.rm = TRUE), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = sum(R_b * AntiM_b, na.rm = TRUE), # Weighted denominator (anticipated mortality)
      population_RMM = numerator / denominator, # Final RMM calculation
    )

  # For the bootstrapped data
  # Calculate the Relative Mortality Metric (RMM) and its upper and lower confidence intervals:
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  # The confidence intervals are adjusted based on the weighted error bound.
  rmm_result_boot <- bin_stats_boot |>
    dplyr::summarize(
      numerator = sum(R_b * (AntiM_b - EM_b), na.rm = TRUE), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = sum(R_b * AntiM_b, na.rm = TRUE), # Weighted denominator (anticipated mortality)
      RMM = numerator / denominator, # Final RMM calculation
      .by = replicate
    )

  # Calculate mean, standard deviation, and 95% confidence intervals
  rmm_result_ci <- rmm_result_boot |>
    dplyr::summarize(
      bootstrap_RMM = mean(RMM, na.rm = TRUE),              # Mean RMM
      sd_RMM = sd(RMM, na.rm = TRUE),                       # Standard deviation of RMM
      se_RMM = sd_RMM / sqrt(n_samples),                    # Standard error
      lower_ci = bootstrap_RMM - (1.96 * se_RMM),           # Lower bound of 95% CI
      upper_ci = bootstrap_RMM + (1.96 * se_RMM)            # Upper bound of 95% CI
    )

  # add the confidence intervals from the bootstrap distribution
  # to the final result
  rmm_result_final <- rmm_result |>
    dplyr::bind_cols(rmm_result_ci) |>
    dplyr::relocate(lower_ci, .before = bootstrap_RMM) |>
    dplyr::relocate(upper_ci, .after = bootstrap_RMM) |>
    dplyr::select(-numerator, -denominator)

  # Return the final result containing the RMM and its confidence intervals
  # optionally, pivot
  if(pivot) {

    rmm_result_final <- rmm_result_final |>
      tidyr::pivot_longer(tidyselect::everything(),
                   names_to = "stat",
                   values_to = "value"
                   )

    return(rmm_result_final)

  } else if(!pivot) {

  # wide result
    return(rmm_result_final)

  }

}

#' Bin-Level Summary for Relative Mortality Metric (RMM)
#'
#' Calculates a bin-level summary for the Relative Mortality Metric (RMM) from
#' Napoli et al. (2017) by grouping data into bins based on survival
#' probabilities (Ps) and summarizing outcomes within each bin. This function
#' returns statistics such as total alive, total dead, estimated mortality,
#' anticipated mortality, and confidence intervals for each bin.
#'
#' The Relative Mortality Metric (RMM) quantifies the performance of a center in
#' comparison to the anticipated mortality based on the TRISS national
#' benchmark. The RMM measures the difference between observed and expected
#' mortality, with a range from -1 to 1.
#'
#' - An RMM of 0 indicates that the observed mortality aligns with the expected
#' national benchmark across all acuity levels.
#' - An RMM greater than 0 indicates better-than-expected performance, where
#' the center is outperforming the national benchmark.
#' - An RMM less than 0 indicates under-performance, where the center’s observed
#' mortality is higher than the expected benchmark.
#'
#' This metric helps assess how a center's mortality compares to the national
#' standards, guiding quality improvement efforts.`rm_bin_summary()` utilizes
#' bootstrap sampling to calculate the confidence intervals via the standard
#' error method.
#'
#' @param data A data frame or tibble containing the data.
#' @param Ps_col The name of the column containing the survival probabilities
#'   (Ps). Should be numeric (values between 0 and 100).
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   should represent "alive" (survived), while `0` should
#'   represent "dead" (did not survive). Ensure the column contains only these
#'   two possible values.
#' @param n_samples A numeric value indicating the number of bootstrap samples
#' to take from the data source.
#' @param Divisor1 A divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Divisor2 A second divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Threshold_1 The first threshold for dividing the survival
#'   probabilities (default is 0.9).
#' @param Threshold_2 The second threshold for dividing the survival
#'   probabilities (default is 0.99).
#'
#' @returns A tibble containing bin-level statistics including:
#'   - `bin_number`: The bin to which each record was assigned.
#'   - `TA_b`: Total alive in each bin (number of patients who survived).
#'   - `TD_b`: Total dead in each bin (number of patients who did not survive).
#'   - `N_b`: Total number of patients in each bin.
#'   - `EM_b`: Estimated mortality rate for each bin (TD_b / (TA_b + TD_b)).
#'   - `bin_start`: The lower bound of the survival probability range for each bin.
#'   - `bin_end`: The upper bound of the survival probability range for each bin.
#'   - `midpoint`: The midpoint of the bin range (calculated as (bin_start + bin_end) / 2).
#'   - `R_b`: The width of each bin (bin_end - bin_start).
#'   - `AntiM_b`: The anticipated mortality for each bin, based on the midpoint for each bin.
#'   - `numerator`: The weighted numerator for the RMM calculation for each bin.
#'   - `denominator`: The weighted denominator for the RMM calculation for each bin.
#'   - `population_RMM`: The final calculated Relative Mortality Metric for the population
#'   existing in `data` for each bin.
#'   - `lower_ci`: The lower bound of the 95% confidence interval calculated via a bootstrap
#'   sample for each bin.
#'   - `boostrap_RMM`: The average RMM value calculated for the boostrap sample for each bin.
#'   - `upper_ci`: The upper bound of the 95% confidence interval calculated via a bootstrap
#'   sample for each bin.
#'   - `sd_RMM`: The standard deviation of the RMM for the bootstrap distribution for each bin.
#'   - `se_RMM`: The standard error of the RMM for the bootstrap distribution for each bin.
#'
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
#' Ps <- plogis(rnorm(n_patients, mean = 2,
#'                     sd = 1.5)
#'                     )  # Skewed towards higher values
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
#' # Example usage of the `rm_bin_summary` function
#' rm_bin_summary(data = data, Ps_col = Ps,
#'                outcome_col = survival
#'                )
#'
#' @author Nicolas Foss, Ed.D, MS, original paper and code in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
rm_bin_summary <- function(data,
                           Ps_col,
                           outcome_col,
                           n_samples = 1000,
                           Divisor1 = 5,
                           Divisor2 = 5,
                           Threshold_1 = 0.9,
                           Threshold_2 = 0.99
                           ) {

  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort("The input data must be a data frame or tibble.")
  }

  # check the n_samples value
  if(!is.numeric(n_samples) && !is.integer(n_samples)) {

    cli::cli_abort("A value of class {.cls numeric} must be passed to {.var n_samples}. The value passed to {.var n_samples} was of class {.val {class(n_samples)}}, please provide a {.cls numeric} value.")

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

  # Check if Ps column is numeric

  # dplyr::pull the Ps data
  Ps_check <- data |> dplyr::pull({{ Ps_col }})

  # check the Ps_check remains continuous
  if (!is.numeric(Ps_check)) {
    cli::cli_abort("The {.var Ps_col} must contain numeric values.")
  }

  if (any(is.na(Ps_check))) {
    cli::cli_warn("Missing values detected in {.var Ps_col}; they will be ignored in calculations.")
  }

  # Check if Ps column is continuous (values between 0 and 1 or 0 and 100)
  if (any(Ps_check < 0 | Ps_check > 100, na.rm = T)) {
    cli::cli_abort("The probability of survival (Ps) values must be between 0 and 100.")
  }

  # Notify the user and convert Ps values if necessary
  if (any(Ps_check > 1, na.rm = TRUE)) {

    cli::cli_alert_info("Probability of survival (Ps) values will be divided by 100 to convert to decimal format.")

    data <- data |>
      dplyr::mutate(!!rlang::ensym(Ps_col) := dplyr::if_else(!!ps_data > 1, !!ps_data / 100, !!ps_data))
  }

  # Assume same distribution of POS scores over years
  # Dynamically assign bins for POS scores using non-linear process
  # specified by Napoli et al. 2017
  # those methods are adapted using this function

  # get the population level bins
  bin_data <- nonlinear_bins(
    data,
    Ps_col = {{ Ps_col }},
    outcome_col = {{ outcome_col }},
    divisor1 = Divisor1,
    divisor2 = Divisor2,
    threshold_1 = Threshold_1,
    threshold_2 = Threshold_2
  )

  set.seed(10232015)

  # Bootstrap process
  bootstrap_data <- data |>
    dplyr::select({{ Ps_col }}, {{ outcome_col }}) |>  # Select only relevant columns
    infer::generate(reps = n_samples, type = "bootstrap") |>   # Generate bootstrap samples
    dplyr::ungroup()

  # bootstrapping to get bins for the population to then create
  # the confidence intervals
  # Nest data by replicate and apply nonlinear_bins
  bin_data_boot <- bootstrap_data |>
    tidyr::nest(data = -replicate) |>  # Nest data by replicate
    dplyr::mutate(
      bins = purrr::map(
        data,
        ~ nonlinear_bins(
          .x, Ps_col = {{ Ps_col }}, outcome_col = {{ outcome_col }},
          divisor1 = Divisor1,
          divisor2 = Divisor2,
          threshold_1 = Threshold_1,
          threshold_2 = Threshold_2
        )
      )
    ) |>
    dplyr::mutate(
      bins_temp = purrr::map(bins, ~ .x$bin_stats)
    ) |>
    tidyr::unnest(bins_temp) |>
    dplyr::select(-bins)

  # Extract the bin intervals (start and end points of the bins)
  intervals_data <- bin_data$intervals

  # Initialize the bin_df to hold bin statistics
  bin_df <- bin_data$bin_stats |>
    dplyr::select(bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin using the start and end points
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(bin_number) # Sort the bins by bin_number

  # Initialize the bind_df_boot to hold bin statistics for each replicate
  # Initialize the bin_df with bootstrap samples
  bin_df_boot <- bin_data_boot |>
    dplyr::select(replicate, bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin for each bootstrap replicate
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(replicate, bin_number) # Sort by replicate and bin_number

  # Summarize bin-level statistics:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary <- bin_data$bin_stats |>
    dplyr::summarize(
      TA_b = sum(alive, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum(dead, na.rm = TRUE), # Total number of deaths in the bin
      N_b = sum(count), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)
      .by = bin_number # Perform this calculation for each bin
    ) |>
    dplyr::arrange(bin_number) # Arrange the bins by bin_number

  # Summarize bin-level statistics for the boostrapped data:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary_boot <- bin_data_boot |>
    dplyr::summarize(
      TA_b = sum(alive, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum(dead, na.rm = TRUE), # Total number of deaths in the bin
      N_b = sum(count), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)

      # Perform this calculation for each replicate and bin
      .by = c(replicate, bin_number)
    ) |>
    dplyr::arrange(replicate, bin_number) # Arrange the bins by bin_number

  # Join the bin statistics (bin_summary) with the bin_df for further calculations
  # The merged data will contain the bin information and corresponding statistics
  # Calculate the Relative Mortality Metric (RMM):
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  bin_stats <- bin_summary |>
    dplyr::left_join(bin_df, by = "bin_number") |>
    dplyr::mutate(
      R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
      AntiM_b = -1 * midpoint + 1, # Anticipated mortality (1 - midpoint, reversed scale)
      numerator = sum(R_b * (AntiM_b - EM_b), na.rm = TRUE), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = sum(R_b * AntiM_b, na.rm = TRUE), # Weighted denominator (anticipated mortality)
      population_RMM = numerator / denominator, # Final RMM calculation
      .by = bin_number
    )

  # For the bootstrapped data
  # Join the bin statistics (bin_summary) with the bin_df_boot for further calculations
  # The merged data will contain the bin information and corresponding statistics
  # Calculate the Relative Mortality Metric (RMM):
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  # Calculate mean, standard deviation, and 95% confidence intervals
  bin_stats_boot <- bin_summary_boot |>
    dplyr::left_join(bin_df_boot, by = c("replicate", "bin_number")) |>
    dplyr::mutate(
      R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
      AntiM_b = -1 * midpoint + 1, # Anticipated mortality (1 - midpoint, reversed scale)
      numerator = sum(R_b * (AntiM_b - EM_b), na.rm = TRUE), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = sum(R_b * AntiM_b, na.rm = TRUE), # Weighted denominator (anticipated mortality)
      RMM = numerator / denominator, # Final RMM calculation
      .by = c(replicate, bin_number)
    ) |>
    dplyr::summarize(
      bootstrap_RMM = mean(RMM, na.rm = T),
      sd_RMM = sd(RMM, na.rm = TRUE),                       # Standard deviation of RMM
      se_RMM = sd_RMM / sqrt(n_samples),                    # Standard error
      lower_ci = bootstrap_RMM - (1.96 * se_RMM),           # Lower bound of 95% CI
      upper_ci = bootstrap_RMM + (1.96 * se_RMM),            # Upper bound of 95% CI
      .by = bin_number
    )

  # add the confidence intervals from the bootstrap distribution
  # to the final result
  bin_stats_final <- bin_stats |>
    dplyr::left_join(bin_stats_boot, by = "bin_number") |>
    dplyr::relocate(lower_ci, .before = bootstrap_RMM) |>
    dplyr::relocate(upper_ci, .after = bootstrap_RMM)

}
