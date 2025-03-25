#' @title Relative Mortality Metric (RMM) Calculation
#'
#' @description
#' Calculates the Relative Mortality Metric (RMM) from Napoli et al. (2017)
#' based on patient survival probabilities (Ps) and actual outcomes. The
#' function groups patients into bins based on their survival probability scores
#' (Ps) and computes a weighted mortality metric along with confidence
#' intervals. For more information on the methods used in this function, see as
#' well Schroeder et al. (2019), and Kassar et al. (2016).
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
#' standards, guiding quality improvement efforts. `rmm()` utilizes bootstrap
#' sampling to calculate the confidence intervals via the standard error method.
#'
#' @param data A data frame or tibble containing the data.
#' @param Ps_col The name of the column containing the survival probabilities
#'   (Ps). Should be numeric on a scale from 0 to 1.
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   should represent "alive" (survived), while `0` should represent "dead" (did
#'   not survive). Ensure the column contains only these two possible values.
#' @param group_vars Optional character vector specifying grouping variables for
#'   stratified analysis. If `NULL`, the calculation is performed on the entire
#'   dataset.
#' @param n_samples A numeric value indicating the number of bootstrap samples
#'   to take from the data source.
#' @param Divisor1 A divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Divisor2 A second divisor used for binning the survival probabilities
#'   (default is 5).
#' @param Threshold_1 The first threshold for dividing the survival
#'   probabilities (default is 0.9).
#' @param Threshold_2 The second threshold for dividing the survival
#'   probabilities (default is 0.99).
#' @param pivot A logical indicating whether to return the results in a long
#'   format (`pivot = TRUE`) or wide format (`pivot = FALSE`, default). Use with
#'   caution in tandem with `group_vars` if the grouping variable is of a
#'   different class than `rmm()`'s outputs, such as `factor` or `character`
#'   grouping variables.
#' @param seed Optional numeric value to set a random seed for reproducibility.
#'   If `NULL` (default), no seed is set.
#'
#' @details
#' Like other statistical computing functions, `rmm()` is happiest
#' without missing data.  It is best to pass complete probability of survival
#' and outcome data to the function for optimal performance. With smaller
#' datasets, this is especially helpful.  However, `rmm()` will
#' handle `NA` values and throw a warning about missing probability of survival
#' values, if any exist in `Ps_col`.
#'
#' Due to the use of bootstrap sampling within the function, users should
#' consider setting the random number `seed` within `rmm()` for reproducibility.
#'
#' @returns A tibble containing the Relative Mortality Metric (RMM) and related
#'   statistics:
#'   - `population_RMM_LL`: The lower bound of the 95% confidence interval for
#'     the population RMM.
#'   - `population_RMM`: The final calculated Relative Mortality Metric for the
#'     population existing in `data`.
#'   - `population_RMM_UL`: The upper bound of the 95% confidence interval for
#'     the population RMM.
#'   - `population_CI`: The confidence interval width for the population RMM.
#'   - `bootstrap_RMM_LL`: The lower bound of the 95% confidence interval for
#'     the bootstrap RMM.
#'   - `bootstrap_RMM`: The average RMM value calculated for the bootstrap
#'     sample.
#'   - `bootstrap_RMM_UL`: The upper bound of the 95% confidence interval for
#'     the bootstrap RMM.
#'   - `bootstrap_CI`: The width of the 95% confidence interval for the
#'     bootstrap RMM.
#'   - If `pivot = TRUE`, the results will be in long format with two columns:
#'     `stat` and `value`, where each row corresponds to one of the calculated
#'     statistics.
#'   - If `pivot = FALSE` (default), the results will be returned in wide
#'     format, with each statistic as a separate column.
#'
#' @export
#'
#' @references
#'
#' Kassar, O.M., Eklund, E.A., Barnhardt, W.F., Napoli, N.J., Barnes, L.E.,
#' Young, J.S. (2016). Trauma survival margin analysis: A dissection of trauma
#' center performance through initial lactate. The American Surgeon, 82(7),
#' 649-653. <doi:10.1177/000313481608200733>
#'
#' Napoli, N. J., Barnhardt, W., Kotoriy, M. E., Young, J. S., & Barnes, L. E.
#' (2017). Relative mortality analysis: A new tool to evaluate clinical
#' performance in trauma centers. IISE Transactions on Healthcare Systems
#' Engineering, 7(3), 181–191. <doi:10.1080/24725579.2017.1325948>
#'
#' Schroeder, P. H., Napoli, N. J., Barnhardt, W. F., Barnes, L. E., & Young, J.
#' S. (2018). Relative mortality analysis of the “golden hour”: A comprehensive
#' acuity stratification approach to address disagreement in current literature.
#' Prehospital Emergency Care, 23(2), 254–262.
#' <doi:10.1080/10903127.2018.1489021>
#'
#' @seealso [rm_bin_summary()], and [nonlinear_bins()]
#'
#' @examples
#' # Generate example data with high negative skewness
#' set.seed(10232015)
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
#'     Divisor1 = 4,
#'     Divisor2 = 4,
#'     n_samples = 10
#'     )
#'
#' # pivot!
#' rmm(data = data, Ps_col = Ps,
#'     outcome_col = survival,
#'     Divisor1 = 4,
#'     Divisor2 = 4,
#'     n_samples = 10,
#'     pivot = TRUE
#'     )
#'
#' # Create example grouping variable (e.g., hospital)
#' hospital <- sample(c("Hospital A", "Hospital B"), n_patients, replace = TRUE)
#'
#' # Create data frame
#' data <- data.frame(
#'   Ps = Ps,
#'   survival = survival_outcomes,
#'   hospital = hospital
#' ) |>
#'   dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Example usage of the `rmm` function with grouping by hospital
#' rmm(
#'   data = data,
#'   Ps_col = Ps,
#'   outcome_col = survival,
#'   group_vars = "hospital",
#'   Divisor1 = 4,
#'   Divisor2 = 4,
#'   n_samples = 10
#' )
#'
#' # Pivoted output for easier visualization
#' rmm(
#'   data = data,
#'   Ps_col = Ps,
#'   outcome_col = survival,
#'   group_vars = "hospital",
#'   Divisor1 = 4,
#'   Divisor2 = 4,
#'   n_samples = 10,
#'   pivot = TRUE
#' )
#'
#' @author Nicolas Foss, Ed.D, MS, original implementation in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
rmm <- function(data,
                Ps_col,
                outcome_col,
                group_vars = NULL,
                n_samples = 100,
                Divisor1 = 5,
                Divisor2 = 5,
                Threshold_1 = 0.9,
                Threshold_2 = 0.99,
                pivot = FALSE,
                seed = NULL
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

  # Check if Ps column is continuous (values between 0 and 1)
  if (any(Ps_check < 0 | Ps_check > 1, na.rm = TRUE)) {
    cli::cli_abort("The probability of survival (Ps) values must be between 0 and 1.")
  }

  if (!is.null(seed) && !is.numeric(seed)) {

    cli::cli_warn(c(
      "The value passed to {.var seed} was of class {.cls {class(seed)}}, but it should be {.cls numeric}.",
      "i" = "The random seed will not be set."
    ))

  }

  # Check if all elements in group_vars are strings (i.e., character vectors)
  if (!all(sapply(group_vars, is.character))) {
    cli::cli_abort(c(
      "All elements in {.var group_vars} must be strings.",
      "i" = "You passed a {.cls {class(group_vars)}} variable to {.var group_vars}."
    ))
  }

  # Check if all group_vars exist in the data
  if (!all(group_vars %in% names(data))) {
    invalid_vars <- group_vars[!group_vars %in% names(data)]
    cli::cli_abort("The following group variable(s) are not valid columns in the data: {paste(invalid_vars, collapse = ', ')}")
  }

  # Treat the column-names-as-strings as symbols
  if(!is.null(group_vars)) {

    group_vars_syms <- rlang::syms(group_vars)

  } else if (is.null(group_vars)) {

    group_vars_syms <- NULL

  }

  # Set the random seed if a value is given
  if(!is.null(seed) && is.numeric(seed)) {

    set.seed(seed)

  }

  # Assume same distribution of POS scores over years
  # Dynamically assign bins for POS scores using non-linear process
  # specified by Napoli et al. 2017
  # those methods are adapted using this function

  # get the population level bins
  bin_data <- nonlinear_bins(
    data = data,
    Ps_col = {{ Ps_col }},
    outcome_col = {{ outcome_col }},
    group_vars = group_vars,
    divisor1 = Divisor1,
    divisor2 = Divisor2,
    threshold_1 = Threshold_1,
    threshold_2 = Threshold_2
  )

  # Bootstrap process
  bootstrap_data <- data |>
    dplyr::select({{ Ps_col }}, {{ outcome_col }}, !!!group_vars_syms) |>  # Select only relevant columns
    dplyr::group_by(!!!group_vars_syms) |>
    infer::generate(reps = n_samples, type = "bootstrap") |>   # Generate bootstrap samples
    dplyr::ungroup()

  # bootstrapping to get bins for the population to then create
  # the confidence intervals
  # Nest data by replicate and optional group_vars and apply nonlinear_bins
  bin_data_boot <- bootstrap_data |>
    tidyr::nest(data = -replicate) |> # Nest data
    dplyr::mutate(
      bins = purrr::map(
        data,
        ~ nonlinear_bins(
          .x, Ps_col = {{ Ps_col }}, outcome_col = {{ outcome_col }},
          group_vars = group_vars,
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

  # Initialize the bin_df to hold bin statistics
  bin_df <- bin_data$bin_stats |>
    dplyr::select(!!!group_vars_syms, bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin using the start and end points
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(!!!group_vars_syms, bin_number) # Sort the bins by bin_number

  # Initialize the bind_df_boot to hold bin statistics for each replicate
  # Initialize the bin_df with bootstrap samples
  bin_df_boot <- bin_data_boot |>
    dplyr::select(!!!group_vars_syms, replicate, bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin for each bootstrap replicate
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(!!!group_vars_syms, replicate, bin_number) # Sort by replicate and bin_number

  # Summarize bin-level statistics:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary <- bin_data$bin_stats |>
    dplyr::group_by(!!!group_vars_syms, bin_number) |> # Perform this calculation for each bin
    dplyr::summarize(
      TA_b = sum(alive, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum(dead, na.rm = TRUE), # Total number of deaths in the bin
      N_b = sum(count, na.rm = TRUE), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)
      AntiS_b = AntiS_b, # keep the predicted survival data
      AntiM_b = AntiM_b, # keep the predicted mortality data
      .groups = "drop"
    ) |>
    dplyr::arrange(!!!group_vars_syms, bin_number) # Arrange the bins by bin_number

  # Summarize bin-level statistics for the boostrapped data:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary_boot <- bin_data_boot |>
    # Perform this calculation for each replicate and bin
    dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
    dplyr::summarize(
      TA_b = sum(alive, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum(dead, na.rm = TRUE), # Total number of deaths in the bin
      N_b = sum(count, na.rm = TRUE), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)
      AntiS_b = AntiS_b, # keep the predicted survival data
      AntiM_b = AntiM_b, # keep the predicted mortality data
      .groups = "drop"
    ) |>
    dplyr::arrange(!!!group_vars_syms, replicate, bin_number) # Arrange the bins by bin_number

  if (is.null(group_vars)) {
    # Join the bin statistics (bin_summary) with the bin_df for further calculations
    # The merged data will contain the bin information and corresponding statistics
    # Not using AntiM_b = -1 * midpoint + 1
    # i.e., Anticipated mortality (1 - midpoint, reversed scale)
    bin_stats <- bin_summary |>
      dplyr::left_join(bin_df, by = dplyr::join_by(bin_number)) |>
      dplyr::group_by(!!!group_vars_syms, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()

  } else {

    bin_stats <- bin_summary |>
      dplyr::left_join(bin_df, by = dplyr::join_by(!!!rlang::syms(group_vars), bin_number)) |>
      dplyr::group_by(!!!group_vars_syms, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()

  }

  if (is.null(group_vars)) {
    # For the bootstrapped data
    # Join the bin statistics (bin_summary) with the bin_df_boot for further calculations
    # The merged data will contain the bin information and corresponding statistics
    # Not using AntiM_b = -1 * midpoint + 1
    # i.e., Anticipated mortality (1 - midpoint, reversed scale)
    bin_stats_boot <- bin_summary_boot |>
      dplyr::left_join(bin_df_boot, by = dplyr::join_by(replicate, bin_number)) |>
      dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()

  } else {

    bin_stats_boot <- bin_summary_boot |>
      dplyr::left_join(bin_df_boot, by = dplyr::join_by(!!!rlang::syms(group_vars), replicate, bin_number)) |>
      dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()

  }

  # Calculate the Relative Mortality Metric (RMM):
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  rmm_result <- bin_stats |>
    dplyr::group_by(!!!group_vars_syms) |>
    dplyr::summarize(
      numerator = sum(R_b * (AntiM_b - EM_b), na.rm = TRUE), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = sum(R_b * AntiM_b, na.rm = TRUE), # Weighted denominator (anticipated mortality)
      population_RMM = numerator / denominator, # Final RMM calculation
      population_RMM = pmin(1, pmax(-1, population_RMM, na.rm = TRUE), na.rm = TRUE), # Ensure RMM is within [-1, 1]
      population_CI = 1.96 * sqrt((sum(AntiM_b, na.rm = TRUE) * sum(AntiS_b, na.rm = TRUE)) / sum(N_b, na.rm = TRUE)),
      # Lower bound of 95% CI
      population_RMM_LL = pmax(-1, population_RMM - population_CI, na.rm = TRUE), # Clip LL
      # Upper bound of 95% CI
      population_RMM_UL = pmin(1, population_RMM + population_CI, na.rm = TRUE), # Clip UL,
      .groups = "drop"
    ) |>
    dplyr::relocate(population_RMM_LL, .before = population_RMM) |>
    dplyr::relocate(population_CI, .after = population_RMM_UL)

  # For the bootstrapped data
  # Calculate the Relative Mortality Metric (RMM) and its upper and lower confidence intervals:
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  # The confidence intervals are adjusted based on the weighted error bound.
  rmm_result_boot <- bin_stats_boot |>
    dplyr::group_by(!!!group_vars_syms, replicate) |>
    dplyr::summarize(
      numerator = sum(R_b * (AntiM_b - EM_b), na.rm = TRUE), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = sum(R_b * AntiM_b, na.rm = TRUE), # Weighted denominator (anticipated mortality)
      RMM = numerator / denominator, # Final RMM calculation
      RMM = pmin(1, pmax(-1, RMM, na.rm = TRUE), na.rm = TRUE), # Ensure RMM is within [-1, 1]
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  # Calculate mean, standard deviation, and 95% confidence intervals
  rmm_result_ci <- rmm_result_boot |>
    dplyr::group_by(!!!group_vars_syms) |>
    dplyr::summarize(
      bootstrap_RMM = mean(RMM, na.rm = TRUE), # Mean RMM
      sd_bootstrap_RMM = sd(RMM, na.rm = TRUE), # Standard deviation of RMM
      se_bootstrap_RMM = sd_bootstrap_RMM / sqrt(n_samples),
      bootstrap_CI = 1.96 * se_bootstrap_RMM, # Standard error
      # Lower bound of 95% CI
      bootstrap_RMM_LL = pmax(-1, bootstrap_RMM - bootstrap_CI, na.rm = TRUE), # Clip LL
      # Upper bound of 95% CI
      bootstrap_RMM_UL = pmin(1, bootstrap_RMM + bootstrap_CI, na.rm = TRUE), # Clip UL
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  if (is.null(group_vars)) {
    # add the confidence intervals from the bootstrap distribution
    # to the final result
    rmm_result_final <- rmm_result |>
      dplyr::bind_cols(rmm_result_ci) |>
      dplyr::relocate(bootstrap_RMM_LL, .before = bootstrap_RMM) |>
      dplyr::relocate(bootstrap_RMM_UL, .after = bootstrap_RMM) |>
      dplyr::relocate(bootstrap_CI, .after = bootstrap_RMM_UL) |>
      dplyr::select(-numerator, -denominator, -sd_bootstrap_RMM, -se_bootstrap_RMM)

  } else {

    rmm_result_final <- rmm_result |>
      dplyr::left_join(rmm_result_ci, by = dplyr::join_by(!!!rlang::syms(group_vars))) |>
      dplyr::relocate(bootstrap_RMM_LL, .before = bootstrap_RMM) |>
      dplyr::relocate(bootstrap_RMM_UL, .after = bootstrap_RMM) |>
      dplyr::relocate(bootstrap_CI, .after = bootstrap_RMM_UL) |>
      dplyr::select(-numerator, -denominator, -sd_bootstrap_RMM, -se_bootstrap_RMM)

  }

  # Return the final result containing the RMM and its confidence intervals
  # optionally, pivot
  if(pivot && is.null(group_vars)) {

    rmm_result_final <- rmm_result_final |>
      tidyr::pivot_longer(tidyselect::everything(),
                          names_to = "stat",
                          values_to = "value"
                          )

    return(rmm_result_final)

  } else if (pivot && !is.null(group_vars)){

      rmm_result_final <- rmm_result_final |>
      tidyr::pivot_longer(-dplyr::all_of(group_vars),
                          names_to = "stat",
                          values_to = "value"
                          )

    return(rmm_result_final)

  } else if (!pivot) {

    # wide result
    return(rmm_result_final)

  }

}

#' @title Bin-Level Summary for Relative Mortality Metric (RMM)
#'
#' @description
#'
#' Calculates a bin-level summary for the Relative Mortality Metric (RMM) from
#' Napoli et al. (2017) by grouping data into bins based on survival
#' probabilities (Ps) and summarizing outcomes within each bin. This function
#' returns statistics such as total alive, total dead, estimated mortality,
#' anticipated mortality, and confidence intervals for each bin. For more
#' information on the methods used in this function, see as well Schroeder et
#' al. (2019), and Kassar et al. (2016).
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
#'   (Ps). Should be numeric on a scale from 0 to 1.
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   should represent "alive" (survived), while `0` should represent "dead" (did
#'   not survive). Ensure the column contains only these two possible values.
#' @param group_vars Optional character vector specifying grouping variables for
#'   stratified analysis. If `NULL`, the calculation is performed on the entire
#'   dataset.
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
#' @param seed Optional numeric value to set a random seed for reproducibility.
#'   If `NULL` (default), no seed is set.
#'
#' @details
#' Like other statistical computing functions, `rm_bin_summary()` is happiest
#' without missing data.  It is best to pass complete probability of survival
#' and outcome data to the function for optimal performance. With smaller
#' datasets, this is especially helpful.  However, `rm_bin_summary()` will
#' handle `NA` values and throw a warning about missing probability of survival
#' values, if any exist in `Ps_col`.
#'
#' Due to the use of bootstrap sampling within the function, users should
#' consider setting the random number seed within `rm_bin_summary()` using the
#' `seed` argument for reproducibility.
#'
#' @returns A tibble containing bin-level statistics including:
#'   - `bin_number`: The bin to which each record was assigned.
#'   - `TA_b`: Total alive in each bin (number of patients who survived).
#'   - `TD_b`: Total dead in each bin (number of patients who did not survive).
#'   - `N_b`: Total number of patients in each bin.
#'   - `EM_b`: Estimated mortality rate for each bin (TD_b / (TA_b + TD_b)).
#'   - `AntiS_b`: The anticipated survival rate for each bin.
#'   - `AntiM_b`: The anticipated mortality rate for each bin.
#'   - `bin_start`: The lower bound of the survival probability range for each
#'      bin.
#'   - `bin_end`: The upper bound of the survival probability range for each
#'      bin.
#'   - `midpoint`: The midpoint of the bin range (calculated as
#'      (bin_start + bin_end) / 2).
#'   - `R_b`: The width of each bin (bin_end - bin_start).
#'   - `population_RMM_LL`: The lower bound of the 95% confidence interval for
#'      the population RMM.
#'   - `population_RMM`: The final calculated Relative Mortality Metric for the
#'      population existing in `data`.
#'   - `population_RMM_UL`: The upper bound of the 95% confidence interval for
#'      the population RMM.
#'   - `population_CI`: The confidence interval width for the population RMM.
#'   - `bootstrap_RMM_LL`: The lower bound of the 95% confidence interval for
#'      the bootstrap RMM.
#'   - `bootstrap_RMM`: The average RMM value calculated for the bootstrap
#'      sample.
#'   - `bootstrap_RMM_UL`: The upper bound of the 95% confidence interval for
#'      the bootstrap RMM.
#'   - `bootstrap_CI`: The width of the 95% confidence interval for the
#'      bootstrap RMM.
#'
#' @export
#'
#' @references
#'
#' Kassar, O.M., Eklund, E.A., Barnhardt, W.F., Napoli, N.J., Barnes, L.E.,
#' Young, J.S. (2016). Trauma survival margin analysis: A dissection of trauma
#' center performance through initial lactate. The American Surgeon, 82(7),
#' 649-653. <doi:10.1177/000313481608200733>
#'
#' Napoli, N. J., Barnhardt, W., Kotoriy, M. E., Young, J. S., & Barnes, L. E.
#' (2017). Relative mortality analysis: A new tool to evaluate clinical
#' performance in trauma centers. IISE Transactions on Healthcare Systems
#' Engineering, 7(3), 181–191. <doi:10.1080/24725579.2017.1325948>
#'
#' Schroeder, P. H., Napoli, N. J., Barnhardt, W. F., Barnes, L. E., & Young, J.
#' S. (2018). Relative mortality analysis of the “golden hour”: A comprehensive
#' acuity stratification approach to address disagreement in current literature.
#' Prehospital Emergency Care, 23(2), 254–262.
#' <doi:10.1080/10903127.2018.1489021>
#'
#' @seealso [rmm()], and [nonlinear_bins()]
#'
#' @examples
#' # Generate example data with high negative skewness
#' set.seed(10232015)
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
#'                outcome_col = survival,
#'                n_samples = 10,
#'                Divisor1 = 4,
#'                Divisor2 = 4
#'                )
#'
#' # Create example grouping variable (e.g., hospital)
#' hospital <- sample(c("Hospital A", "Hospital B"), n_patients, replace = TRUE)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps,
#'                   survival = survival_outcomes,
#'                   hospital = hospital
#'                   ) |>
#'   dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Example usage of the `rm_bin_summary` function with grouping
#' rm_bin_summary(
#'   data = data,
#'   Ps_col = Ps,
#'   outcome_col = survival,
#'   group_vars = "hospital", # Stratifies by hospital
#'   n_samples = 10,
#'   Divisor1 = 4,
#'   Divisor2 = 4
#' )
#'
#' @author Nicolas Foss, Ed.D, MS, original implementation in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
rm_bin_summary <- function(data,
                           Ps_col,
                           outcome_col,
                           group_vars = NULL,
                           n_samples = 100,
                           Divisor1 = 5,
                           Divisor2 = 5,
                           Threshold_1 = 0.9,
                           Threshold_2 = 0.99,
                           seed = NULL
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

  # Check if Ps column is continuous (values between 0 and 1)
  if (any(Ps_check < 0 | Ps_check > 1, na.rm = TRUE)) {
    cli::cli_abort("The probability of survival (Ps) values must be between 0 and 1.")
  }

  if (!is.null(seed) && !is.numeric(seed)) {

    cli::cli_warn(c(
      "The value passed to {.var seed} was of class {.cls {class(seed)}}, but it should be {.cls numeric}.",
      "i" = "The random seed will not be set."
    ))

  }

  # Check if all elements in group_vars are strings (i.e., character vectors)
  if (!all(sapply(group_vars, is.character))) {
    cli::cli_abort(c(
      "All elements in {.var group_vars} must be strings.",
      "i" = "You passed a {.cls {class(group_vars)}} variable to {.var group_vars}."
    ))
  }


  # Check if all elements in group_vars are strings (i.e., character vectors)
  if (!all(sapply(group_vars, is.character))) {
    cli::cli_abort(c(
      "All elements in {.var group_vars} must be strings.",
      "i" = "You passed a {.cls {class(group_vars)}} variable to {.var group_vars}."
    ))
  }

  # Check if all group_vars exist in the data
  if (!all(group_vars %in% names(data))) {
    invalid_vars <- group_vars[!group_vars %in% names(data)]
    cli::cli_abort("The following group variable(s) are not valid columns in the data: {paste(invalid_vars, collapse = ', ')}")
  }

  # Treat the column-names-as-strings as symbols
  if(!is.null(group_vars)) {

    group_vars_syms <- rlang::syms(group_vars)

  } else if (is.null(group_vars)) {

    group_vars_syms <- NULL

  }

  # Set the random seed if a value is given
  if(!is.null(seed) && is.numeric(seed)) {

    set.seed(seed)

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
    group_vars = group_vars,
    divisor1 = Divisor1,
    divisor2 = Divisor2,
    threshold_1 = Threshold_1,
    threshold_2 = Threshold_2
  )

  # Bootstrap process
  bootstrap_data <- data |>
    dplyr::select({{ Ps_col }}, {{ outcome_col }}, !!!group_vars_syms) |>  # Select only relevant columns
    dplyr::group_by(!!!group_vars_syms) |>
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
          group_vars = group_vars,
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

  # Initialize the bin_df to hold bin statistics
  bin_df <- bin_data$bin_stats |>
    dplyr::select(!!!group_vars_syms, bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin using the start and end points
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(!!!group_vars_syms, bin_number) # Sort the bins by bin_number

  # Initialize the bind_df_boot to hold bin statistics for each replicate
  # Initialize the bin_df with bootstrap samples
  bin_df_boot <- bin_data_boot |>
    dplyr::select(!!!group_vars_syms, replicate, bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin for each bootstrap replicate
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(!!!group_vars_syms, replicate, bin_number) # Sort by replicate and bin_number

  # Summarize bin-level statistics:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary <- bin_data$bin_stats |>
    dplyr::group_by(!!!group_vars_syms, bin_number) |> # Perform this calculation for each bin
    dplyr::summarize(
      TA_b = sum(alive, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum(dead, na.rm = TRUE), # Total number of deaths in the bin
      N_b = sum(count, na.rm = TRUE), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)
      AntiS_b = AntiS_b, # keep the predicted survival data
      AntiM_b = AntiM_b, # keep the predicted mortality data
      .groups = "drop"
    ) |>
    dplyr::arrange(!!!group_vars_syms, bin_number) # Arrange the bins by bin_number

  # Summarize bin-level statistics for the boostrapped data:
  # - TA_b: Total alive (patients in the bin that survived)
  # - TD_b: Total dead (patients in the bin that did not survive)
  # - N_b: Total number of observations (patients in the bin)
  # - EM_b: Estimated mortality for the bin (TD_b / (TA_b + TD_b))
  bin_summary_boot <- bin_data_boot |>
    # Perform this calculation for each replicate and bin
    dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
    dplyr::summarize(
      TA_b = sum(alive, na.rm = TRUE), # Total number of survivors in the bin
      TD_b = sum(dead, na.rm = TRUE), # Total number of deaths in the bin
      N_b = sum(count, na.rm = TRUE), # Total number of patients in the bin
      EM_b = TD_b / N_b, # Estimated mortality (TD_b / total patients)
      AntiS_b = AntiS_b, # keep the predicted survival data
      AntiM_b = AntiM_b, # keep the predicted mortality data
      .groups = "drop"
    ) |>
    dplyr::arrange(!!!group_vars_syms, replicate, bin_number) # Arrange the bins by bin_number

  if (is.null(group_vars)) {
    # Join the bin statistics (bin_summary) with the bin_df for further calculations
    # The merged data will contain the bin information and corresponding statistics
    # Not using AntiM_b = -1 * midpoint + 1
    # i.e., Anticipated mortality (1 - midpoint, reversed scale)
    bin_stats <- bin_summary |>
      dplyr::left_join(bin_df, by = dplyr::join_by(bin_number)) |>
      dplyr::group_by(!!!group_vars_syms, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()

  } else {

    bin_stats <- bin_summary |>
      dplyr::left_join(bin_df, by = dplyr::join_by(!!!rlang::syms(group_vars), bin_number)) |>
      dplyr::group_by(!!!group_vars_syms, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()

  }

  if (is.null(group_vars)) {
    # For the bootstrapped data
    # Join the bin statistics (bin_summary) with the bin_df_boot for further calculations
    # The merged data will contain the bin information and corresponding statistics
    # Not using AntiM_b = -1 * midpoint + 1
    # i.e., Anticipated mortality (1 - midpoint, reversed scale)
    bin_stats_boot <- bin_summary_boot |>
      dplyr::left_join(bin_df_boot, by = dplyr::join_by(replicate, bin_number)) |>
      dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()

  } else {

    bin_stats_boot <- bin_summary_boot |>
      dplyr::left_join(bin_df_boot, by = dplyr::join_by(!!!rlang::syms(group_vars), replicate, bin_number)) |>
      dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()

  }

  # Calculate the Relative Mortality Metric (RMM):
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  rmm_result <- bin_stats |>
    dplyr::group_by(!!!group_vars_syms, bin_number) |>
    dplyr::mutate(
      numerator = R_b * (AntiM_b - EM_b), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = R_b * AntiM_b, # Weighted denominator (anticipated mortality)
      population_RMM = numerator / denominator, # Final RMM calculation
      population_RMM = pmin(1, pmax(-1, population_RMM, na.rm = TRUE), na.rm = TRUE), # Ensure RMM is within [-1, 1]
      population_CI = 1.96 * sqrt((sum(AntiM_b, na.rm = TRUE) * sum(AntiS_b, na.rm = TRUE)) / sum(N_b, na.rm = TRUE)),
      # Lower bound of 95% CI
      population_RMM_LL = pmax(-1, population_RMM - population_CI, na.rm = TRUE), # Clip LL
      # Upper bound of 95% CI
      population_RMM_UL = pmin(1, population_RMM + population_CI, na.rm = TRUE), # Clip UL,
    ) |>
    dplyr::ungroup() |>
    dplyr::relocate(population_RMM_LL, .before = population_RMM) |>
    dplyr::relocate(population_CI, .after = population_RMM_UL)

  # For the bootstrapped data
  # Calculate the Relative Mortality Metric (RMM) and its upper and lower confidence intervals:
  # RMM is calculated by:
  # - Computing the weighted difference between anticipated and observed mortality.
  # - Normalizing by the weighted anticipated mortality.
  # The confidence intervals are adjusted based on the weighted error bound.
  rmm_result_boot <- bin_stats_boot |>
    dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
    dplyr::mutate(
      numerator = sum(R_b * (AntiM_b - EM_b), na.rm = TRUE), # Weighted numerator (difference between anticipated and observed mortality)
      denominator = sum(R_b * AntiM_b, na.rm = TRUE), # Weighted denominator (anticipated mortality)
      RMM = numerator / denominator, # Final RMM calculation
      RMM = pmin(1, pmax(-1, RMM, na.rm = TRUE), na.rm = TRUE), # Ensure RMM is within [-1, 1]
    ) |>
    dplyr::ungroup()

  # Calculate mean, standard deviation, and 95% confidence intervals
  rmm_result_ci <- rmm_result_boot |>
    dplyr::group_by(!!!group_vars_syms, bin_number) |>
    dplyr::summarize(
      bootstrap_RMM = mean(RMM, na.rm = TRUE), # Mean RMM
      sd_bootstrap_RMM = sd(RMM, na.rm = TRUE), # Standard deviation of RMM
      se_bootstrap_RMM = sd_bootstrap_RMM / sqrt(n_samples),
      bootstrap_CI = 1.96 * se_bootstrap_RMM, # Standard error
      # Lower bound of 95% CI
      bootstrap_RMM_LL = pmax(-1, bootstrap_RMM - bootstrap_CI, na.rm = TRUE), # Clip LL
      # Upper bound of 95% CI
      bootstrap_RMM_UL = pmin(1, bootstrap_RMM + bootstrap_CI, na.rm = TRUE), # Clip UL
      .groups = "drop"
    )

  # add the confidence intervals from the bootstrap distribution
  # to the final result
  rmm_result_final <- rmm_result |>
    dplyr::left_join(rmm_result_ci, by = dplyr::join_by(!!!rlang::syms(group_vars), bin_number)) |>
    dplyr::relocate(bootstrap_RMM_LL, .before = bootstrap_RMM) |>
    dplyr::relocate(bootstrap_RMM_UL, .after = bootstrap_RMM) |>
    dplyr::relocate(bootstrap_CI, .after = bootstrap_RMM_UL) |>
    dplyr::select(-numerator, -denominator, -sd_bootstrap_RMM, -se_bootstrap_RMM)

  # complete
  return(rmm_result_final)

}
