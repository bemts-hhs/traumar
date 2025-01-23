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
#' standards, guiding quality improvement efforts.
#'
#' @param data A data frame or tibble containing the data.
#' @param Ps_col The name of the column containing the survival probabilities
#'   (Ps). Should be numeric (values between 0 and 100).
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   or `TRUE` should represent "alive" (survived), while `0` or `FALSE` should
#'   represent "dead" (did not survive). Ensure the column contains only these
#'   two possible values.
#' @param Divisor1 A divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Divisor2 A second divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Threshold_1 The first threshold for dividing the survival
#'   probabilities (default is 0.9).
#' @param Threshold_2 The second threshold for dividing the survival
#'   probabilities (default is 0.99).
#' @param Z The Z value used for calculating confidence intervals (default is
#'   1.96).
#' @param pivot A logical indicating whether to return the results in a long
#'   format (pivot = TRUE) or wide format (pivot = FALSE, default).
#'
#' @returns A tibble containing the Relative Mortality Metric (RMM) and related
#'   statistics:
#'   - `numerator`: The weighted numerator used in the
#'   RMM calculation (difference between anticipated and observed mortality).
#'   - `denominator`: The weighted denominator used in the RMM calculation (anticipated mortality).
#'   - `E_b`: The error bound (confidence interval error) for the RMM.
#'   - `RMM`: The final calculated Relative Mortality Metric.
#'   - `RMM_LL`: The lower confidence limit for the RMM, adjusted with the error bound.
#'   - `RMM_UL`: The upper confidence limit for the RMM, adjusted with the error bound.
#'   - If `pivot = TRUE`, the results will be in long format with two columns: `coefficient`
#'    and `value`, where each row corresponds to one of the calculated statistics.
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
#' # Generate survival probabilities (Ps) using a logistic distribution
#' set.seed(123)  # For reproducibility
#' Ps <- plogis(rnorm(n_patients, mean = 2, sd = 1.5))  # Skewed towards higher values
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
#' dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Example usage of the `rmm` function
#' rmm(data = data, Ps_col = Ps, outcome_col = survival, Divisor1 = 5, Divisor2 = 5)
#' rmm(data = data, Ps_col = survival_probability, outcome_col = survival, Divisor1 = 5, Divisor2 = 5, pivot = TRUE)
#'
#' @author Nicolas Foss, Ed.D, MS, original paper and code in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
rmm <- function(data,
                Ps_col,
                outcome_col,
                Divisor1 = 5,
                Divisor2 = 5,
                Threshold_1 = 0.9,
                Threshold_2 = 0.99,
                Z = 1.96,
                pivot = FALSE
                ) {

  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  if (!"data.frame" %in% class(data)) {
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

  if (!all(unique_values %in% c(0, 1, TRUE, FALSE)) || length(unique_values) > 2) {
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

  # Inform the user that data validation has passed.
  cli::cli_alert_success("Data validation passed.")

  # Assume same distribution of POS scores over years
  # Dynamically assign bins for POS scores using non-linear process
  # specified by Napoli et al. 2017
  # those methods are adapted using this function

  bin_data <- nonlinear_bins(
    data,
    {{ Ps_col }},
    divisor1 = Divisor1,
    divisor2 = Divisor2,
    threshold_1 = Threshold_1,
    threshold_2 = Threshold_2
  )

  # Extract the bin intervals (start and end points of the bins)
  intervals_data <- bin_data$intervals

  # Initialize the bin_df to hold bin statistics
  bin_df <- bin_data$bin_stats |>
    dplyr::select(bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin using the start and end points
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(bin_number) # Sort the bins by bin_number

  # Initialize final_data with an additional column for bin assignments
  final_data <- data |>
    dplyr::mutate(bin_number = 0L) # Create a column to store bin labels (initialized to 0)

  # Classify rows from final_data into appropriate bins
  # Iterate over the rows of bin_df to assign bin numbers
  for (i in 1:max(bin_df$bin_number)) {

    # Update the bin_number for rows where the POS score falls within the current bin interval
    final_data <- final_data |>
      dplyr::mutate(
        bin_number = dplyr::if_else(
          {{ Ps_col }} >= bin_df$bin_start[i] & {{ Ps_col }} <= bin_df$bin_end[i],
          i, # Assign bin number i if the POS score fits within the interval
          bin_number
        )
      )
  }

  # Summarize bin-level statistics:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary <- final_data |>
    dplyr::summarize(
      TA_b = sum({{ outcome_col }}, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum({{ outcome_col }} == 0, na.rm = TRUE), # Total number of deaths in the bin
      N_b = dplyr::n(), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)
      .by = bin_number # Perform this calculation for each bin
    ) |>
    dplyr::arrange(bin_number) # Arrange the bins by bin_number

  # Join the bin statistics (bin_summary) with the bin_df for further calculations
  # The merged data will contain the bin information and corresponding statistics
  bin_stats <- bin_summary |>
    dplyr::left_join(bin_df, by = "bin_number") |>
    dplyr::mutate(
      R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
      AntiM_b = -1 * midpoint + 1, # Anticipated mortality (1 - midpoint, reversed scale)

      # Calculate the error bound (E_b) based on the bin size and confidence level
      E_b = Z * sqrt((AntiM_b * (1 - AntiM_b)) / N_b),
      .by = bin_number
    )

  # Calculate the Relative Mortality Metric (RMM) and its upper and lower confidence intervals:
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  # The confidence intervals are adjusted based on the weighted error bound.
  rmm_result <- bin_stats |>
    dplyr::summarize(
      numerator = sum(R_b * (AntiM_b - EM_b), na.rm = TRUE), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = sum(R_b * AntiM_b, na.rm = TRUE), # Weighted denominator (anticipated mortality)
      E_b = Z * sqrt((sum(AntiM_b, na.rm = TRUE) * sum(1 - AntiM_b, na.rm = TRUE)) / sum(N_b, na.rm = TRUE)),
      RMM = numerator / denominator, # Final RMM calculation
      # Calculate the upper confidence interval for RMM by adding the weighted error bound
      RMM_UL = dplyr::if_else((RMM + E_b) > 1, 1, RMM + E_b), # Ensure RMM_UL does not exceed 1
      # Calculate the lower confidence interval for RMM by subtracting the weighted error bound
      RMM_LL = dplyr::if_else((RMM - E_b) < -1, -1, RMM - E_b)  # Ensure RMM_LL does not fall below -1
    ) |>
    dplyr::relocate(RMM_LL, .before = RMM)

  # Return the final result containing the RMM and its confidence intervals
  # optionally, pivot
  if(pivot) {

    rmm_result |>
      tidyr::pivot_longer(tidyselect::everything(),
                   names_to = "coefficient",
                   values_to = "value"
                   )

  } else if(!pivot) {

  # wide result
  rmm_result

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
#' standards, guiding quality improvement efforts.
#'
#' @param data A data frame or tibble containing the data.
#' @param Ps_col The name of the column containing the survival probabilities
#'   (Ps). Should be numeric (values between 0 and 100).
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   or `TRUE` should represent "alive" (survived), while `0` or `FALSE` should
#'   represent "dead" (did not survive). Ensure the column contains only these
#'   two possible values.
#' @param Divisor1 A divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Divisor2 A second divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Threshold_1 The first threshold for dividing the survival
#'   probabilities (default is 0.9).
#' @param Threshold_2 The second threshold for dividing the survival
#'   probabilities (default is 0.99).
#' @param Z The Z value used for calculating confidence intervals (default is
#'   1.96).
#'
#' @returns A tibble containing bin-level statistics including:
#'   - `bin_number`: The bin to which each patient was assigned.
#'   - `TA_b`: Total alive in each bin (number of patients who survived).
#'   - `TD_b`: Total dead in each bin (number of patients who did not survive).
#'   - `N_b`: Total number of patients in each bin.
#'   - `EM_b`: Estimated mortality rate for each bin (TD_b / (TA_b + TD_b)).
#'   - `bin_start`: The lower bound of the survival probability range for each bin.
#'   - `bin_end`: The upper bound of the survival probability range for each bin.
#'   - `midpoint`: The midpoint of the bin range (calculated as (bin_start + bin_end) / 2).
#'   - `R_b`: The width of each bin (bin_end - bin_start).
#'   - `AntiM_b`: The anticipated mortality for each bin, based on the midpoint.
#'   - `E_b`: The error bound for the RMM estimate, calculated using the bin size
#'   and anticipated mortality.
#'   - `numerator`: The weighted numerator for the RMM calculation.
#'   - `denominator`: The weighted denominator for the RMM calculation.
#'   - `RMM`: The Relative Mortality Metric (RMM) for each bin,
#'   quantifying the difference between observed and expected mortality.
#'   - `RMM_UL`: The upper 95% confidence limit for RMM.
#'   - `RMM_LL`: The lower 95% confidence limit for RMM.
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
#' # Generate survival probabilities (Ps) using a logistic distribution
#' set.seed(123)  # For reproducibility
#' Ps <- plogis(rnorm(n_patients, mean = 2, sd = 1.5))  # Skewed towards higher values
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
#' dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Example usage of the `rm_bin_summary` function
#' rm_bin_summary(data = data, Ps_col = Ps, outcome_col = survival)
#'
#' @author Nicolas Foss, Ed.D, MS, original paper and code in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
rm_bin_summary <- function(data,
                           Ps_col,
                           outcome_col,
                           Divisor1 = 5,
                           Divisor2 = 5,
                           Threshold_1 = 0.9,
                           Threshold_2 = 0.99,
                           Z = 1.96
                           ) {

  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  if (!"data.frame" %in% class(data)) {
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

  if (!all(unique_values %in% c(0, 1, TRUE, FALSE)) || length(unique_values) > 2) {
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

  # Inform the user that data validation has passed.
  cli::cli_alert_success("Data validation passed.")

  # Assume the same distribution of POS scores over years.
  # Dynamically assign bins for POS scores using a non-linear process,
  # as specified in Napoli et al. (2017). This function `nonlinear_bins`
  # adapts the methodology and computes the appropriate bin intervals
  # for the given POS scores.

  bin_data <- nonlinear_bins(
    data,
    {{ Ps_col }},
    divisor1 = Divisor1, # The first divisor for binning, as defined by the method
    divisor2 = Divisor2,  # The second divisor for binning, as defined by the method
    threshold_1 = Threshold_1,
    threshold_2 = Threshold_2
  )

  # Extract the bin intervals (start and end points) from the bin_data object
  intervals_data <- bin_data$intervals

  # Initialize the bin_df to store bin-level statistics:
  # - `bin_number`: the identifier for each bin
  # - `bin_start`: the lower bound of each bin
  # - `bin_end`: the upper bound of each bin
  # Calculate the midpoint of each bin using the formula: (bin_start + bin_end) / 2
  bin_df <- bin_data$bin_stats |>
    dplyr::select(bin_number, bin_start, bin_end) |>
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(bin_number) # Sort the bins by bin_number to maintain the correct order

  # Initialize final_data by adding a new column `bin_number` to store the bin assignment
  # Initially, all rows are assigned to bin 0 (unclassified).
  final_data <- data |>
    dplyr::mutate(bin_number = 0L) # Create a column to store bin labels (initialized to 0)

  # Classify rows from final_data into appropriate bins based on POS score
  # Iterate over the rows of bin_df and update the bin_number for each row
  for (i in 1:nrow(bin_df)) {

    # Assign bin_number to rows where the POS score falls within the current bin's range
    final_data <- final_data |>
      dplyr::mutate(
        bin_number = dplyr::if_else(
          {{ Ps_col }} >= bin_df$bin_start[i] & {{ Ps_col }} <= bin_df$bin_end[i],
          i, # Assign the current bin_number (i) if the POS score fits within the bin's range
          bin_number # Otherwise, retain the existing bin_number
        )
      )
  }

  # Summarize bin-level statistics:
  # - `TA_b`: Total alive (patients in the bin that survived)
  # - `TD_b`: Total dead (patients in the bin that did not survive)
  # - `N_b`: Total number of observations (patients in the bin)
  # - `EM_b`: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary <- final_data |>
    dplyr::summarize(
      TA_b = sum({{ outcome_col }}, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum({{ outcome_col }} == 0, na.rm = TRUE), # Total number of deaths in the bin
      N_b = dplyr::n(), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality rate (TD_b / total patients in bin)
      .by = bin_number # Calculate these statistics for each bin
    ) |>
    dplyr::arrange(bin_number) # Sort the bins by bin_number

  # Join the bin-level statistics (bin_summary) with the bin information (bin_df)
  # This allows us to access both the statistics and the bin definitions for further calculations
  bin_summary <- bin_summary |>
    dplyr::left_join(bin_df, by = "bin_number") |>
    dplyr::mutate(
      R_b = bin_end - bin_start, # Bin width (R_b = bin_end - bin_start)
      AntiM_b = -1 * midpoint + 1, # Anticipated mortality (1 - midpoint, reversed scale to match the method)
      E_b = Z * sqrt((AntiM_b * (1 - AntiM_b)) / N_b), # Error bound (E_b) based on bin size and the anticipated mortality
      numerator = R_b * (AntiM_b - EM_b), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = R_b * AntiM_b, # Weighted denominator (anticipated mortality)
      RMM = numerator / denominator, # Final RMM calculation,
      RMM_UL = RMM + E_b, # upper 95% CI limit
      RMM_UL = dplyr::if_else(RMM_UL > 1, 1, RMM_UL), # correct CI > 1
      RMM_LL = RMM - E_b, # lower 95% CI limit
      RMM_LL = dplyr::if_else(RMM_LL < -1, -1, RMM_LL), # correct CI > 1
      .by = bin_number # Ensure calculations are done per bin
    ) |>
    dplyr::relocate(RMM_LL, .before = RMM)

  # Return the summarized bin metrics as a tibble, containing:
  # - Total alive (TA_b)
  # - Total dead (TD_b)
  # - Estimated mortality (EM_b)
  # - Anticipated mortality (AntiM_b)
  # - Error bound (E_b)
  # - Bin width (R_b)
  # - RMM numerator
  # - RMM denominator
  # - RMM calculation per bin
  # - RMM upper limit
  # - RMM lower limit
  bin_summary

}
