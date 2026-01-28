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
#'   not survive).  `TRUE/FALSE` are accepted as well. Ensure the column
#'   contains only these possible values.
#' @param group_vars Optional character vector specifying grouping variables for
#'   stratified analysis. If `NULL`, the calculation is performed on the entire
#'   dataset.
#' @param n_samples A numeric value indicating the number of bootstrap samples
#'   to take from the data source.
#' @param Divisor1 A positive numeric value controlling the coarseness of bins
#'   for Ps values below `Threshold_1`. It scales the number of steps from the
#'   start of the dataset up to the `Threshold_1` cut point. Larger values
#'   produce fewer, broader bins; smaller values produce more, narrower bins.
#'   Defaults to `5`.
#' @param Divisor2 A positive numeric value controlling the coarseness of bins
#'   for Ps values between `Threshold_1` and `Threshold_2`. Larger values yield
#'   wider bins, and smaller values yield narrower bins in this range. Defaults
#'   to `5`.
#' @param Threshold_1 A numeric value that defines the lower bound of the
#'   high-survival probability range in `Ps_col`. The function identifies the
#'   first index where `Ps_col` exceeds this value and begins applying smaller
#'   bin widths from that point onward. Defaults to `0.9`, meaning binning
#'   changes once Ps > 0.90.
#' @param Threshold_2 A numeric value that defines the upper bound of the
#'   high-survival probability range in `Ps_col`. The function identifies the
#'   first index where `Ps_col` exceeds this value. Between `Threshold_1` and
#'   `Threshold_2`, finer binning is applied; above `Threshold_2`, binning may
#'   again change. Defaults to `0.99`, meaning the special binning range is
#'   between Ps values of 0.90 and 0.99.
#' @param bootstrap_ci A logical indicating whether to return the relative
#'   mortality metric estimate and 95% confidence intervals using bootstrap
#'   sampling. Default is `TRUE`.
#' @param pivot A logical indicating whether to return the results in a long
#'   format (`pivot = TRUE`) or wide format (`pivot = FALSE`, default). Use with
#'   caution in tandem with `group_vars` if the grouping variable is of a
#'   different class than `rmm()`'s outputs, such as `factor` or `character`
#'   grouping variables.
#' @param seed Optional numeric value to set a random seed for reproducibility.
#'   If `NULL` (default), no seed is set.
#'
#' @details
#' Like other statistical computing functions, `rmm()` is happiest without
#' missing data.  It is best to pass complete probability of survival and
#' mortality outcome data to the function for optimal performance. With smaller
#' datasets, this is especially helpful.  However, `rmm()` will throw a warning
#' about missing values, if any exist in `Ps_col` and/or `outcome_col`.
#'
#' `rmm()` assumes `Ps_col` contains probabilities derived from
#' real-world inputs for the Trauma Injury Severity Score (TRISS) model.
#' Synthetic or low-variability data (especially with small sample sizes) may
#' not reflect the distribution of TRISS-derived survival probabilities. This
#' can result in unstable estimates or function failure due to insufficient
#' dispersion. With small sample sizes, it may be important to use smaller
#' values with the divisor arguments and adjust the thresholds (based on the
#' distribution of the `Ps_col` values) to create bins that better accommodate
#' the data.
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
#'     the bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)
#'   - `bootstrap_RMM`: The average RMM value calculated for the bootstrap
#'     sample. (optional, if `bootstrap_ci = TRUE`)
#'   - `bootstrap_RMM_UL`: The upper bound of the 95% confidence interval for
#'     the bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)
#'   - `bootstrap_CI`: The width of the 95% confidence interval for the
#'     bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)
#'   - If `pivot = TRUE`, the results will be in long format with two columns:
#'     `stat` and `value`, where each row corresponds to one of the calculated
#'     statistics.
#'   - If `pivot = FALSE` (default), the results will be returned in wide
#'     format, with each statistic as a separate column.
#'
#' @note
#'
#' This function will produce the most reliable and interpretable results when
#' using a dataset that has one row per patient, with each column being a
#' feature.
#'
#' By default, `rmm()` derives bin cut points from the full dataset’s
#' distribution. This ensures comparability across groups when `group_vars` is
#' used. To tailor results to a specific group (e.g., a single hospital), filter
#' the dataset to that subgroup before calling `rmm()`. The function will then
#' compute bins and related statistics using only that subset’s `Ps_col`
#' distribution. When `group_vars` is used, and ff a group lacks
#' observations within one or more bins, `rm_bin_summary()` will compute
#' statistics only for the bins that contain data. Bins with no observations are
#' excluded from the summary for that group.
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
#' @seealso [probability_of_survival()], [rm_bin_summary()], and
#'   [nonlinear_bins()]
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#'
#' # Parameters
#' # Total number of patients
#' n_patients <- 5000
#'
#' # Arbitrary group labels
#' groups <- sample(x = LETTERS[1:2], size = n_patients, replace = TRUE)
#'
#' # Trauma types
#' trauma_type_values <- sample(
#'   x = c("Blunt", "Penetrating"),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # RTS values
#' rts_values <- sample(
#'   x = seq(from = 0, to = 7.8408, by = 0.005),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # patient ages
#' ages <- sample(
#'   x = seq(from = 0, to = 100, by = 1),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # ISS scores
#' iss_scores <- sample(
#'   x = seq(from = 0, to = 75, by = 1),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # Generate survival probabilities (Ps)
#' Ps <- traumar::probability_of_survival(
#'   trauma_type = trauma_type_values,
#'   age = ages,
#'   rts = rts_values,
#'   iss = iss_scores
#' )
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes, groups = groups) |>
#'   dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
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
rmm <- function(
  data,
  Ps_col,
  outcome_col,
  group_vars = NULL,
  n_samples = 100,
  Divisor1 = 5,
  Divisor2 = 5,
  Threshold_1 = 0.9,
  Threshold_2 = 0.99,
  bootstrap_ci = TRUE,
  pivot = FALSE,
  seed = NULL
) {
  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  validate_data_structure(
    input = data,
    structure_type = c("data.frame", "tbl", "tbl_df"),
    logic = "or",
    type = "error"
  )

  # check the n_samples value
  validate_class(
    input = n_samples,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error"
  )

  # validate Divisor1
  validate_class(
    input = Divisor1,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error"
  )

  # validate Divisor2
  validate_class(
    input = Divisor2,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error"
  )

  # validate Threshold_1
  validate_numeric(input = Threshold_1, min = 0, max = 1, type = "error")

  # validate Threshold_2
  validate_numeric(input = Threshold_2, min = 0, max = 1, type = "error")

  # Validate the CI argument
  validate_class(input = bootstrap_ci, class_type = "logical", type = "error")

  # Validate the pivot argument
  validate_class(input = pivot, class_type = "logical", type = "error")

  # Ensure Ps_col and outcome_col arguments are provided with tailored error messages
  if (missing(Ps_col) && missing(outcome_col)) {
    cli::cli_abort(
      "Both {.var Ps_col} and {.var outcome_col} arguments must be provided."
    )
  } else if (missing(Ps_col)) {
    cli::cli_abort("The {.var Ps_col} argument must be provided.")
  } else if (missing(outcome_col)) {
    cli::cli_abort("The {.var outcome_col} argument must be provided.")
  }

  # Pull and check the outcome column
  binary_data <- data |> dplyr::pull({{ outcome_col }})

  # Ensure the column is either logical or numeric
  validate_class(
    input = binary_data,
    class_type = c("numeric", "logical", "integer"),
    logic = "or",
    type = "error",
    var_name = "outcome_col"
  )

  # Get unique non-missing values to use in subsequent data validation
  non_missing <- stats::na.omit(binary_data)

  # Validate type and values
  if (is.logical(non_missing)) {
    # Logical vector: ensure only TRUE/FALSE (no coercion needed)
    validate_set(
      input = non_missing,
      valid_set = c(TRUE, FALSE),
      type = "error",
      var_name = "outcome_col"
    )
  } else if (is.numeric(non_missing)) {
    # Numeric vector: ensure strictly 0 or 1
    validate_set(
      input = non_missing,
      valid_set = c(0, 1),
      type = "error",
      var_name = "outcome_col"
    )
  } else if (is.integer(non_missing)) {
    # Integer vector: ensure strictly 0 or 1
    validate_set(
      input = non_missing,
      valid_set = c(0L, 1L),
      type = "error",
      var_name = "outcome_col"
    )
  } else {
    # Not logical or numeric
    cli::cli_abort(
      "The {.var outcome_col} must be either logical (TRUE/FALSE) or numeric/integer (1/0)."
    )
  }

  # Warn if missing
  validate_complete(
    input = binary_data,
    type = "warning",
    var_name = "outcome_col"
  )

  # Check if Ps column is numeric

  # dplyr::pull the Ps data
  Ps_check <- data |> dplyr::pull({{ Ps_col }})

  # check the Ps_check remains continuous
  # Check if Ps column is continuous (values between 0 and 1)
  validate_numeric(
    input = Ps_check,
    min = 0,
    max = 1,
    type = "error",
    var_name = "Ps_col"
  )

  # Warn if any missings in Ps_col
  validate_complete(input = Ps_check, type = "warning", var_name = "Ps_col")

  # Validate the seed argument
  validate_class(
    input = seed,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error",
    null_ok = TRUE
  )

  # Set the random seed if a value is given
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Check if all elements in group_vars are strings (i.e., character vectors)
  validate_character_factor(input = group_vars, type = "error", null_ok = TRUE)

  # Check if all group_vars exist in the data
  validate_names(
    input = data,
    check_names = group_vars,
    type = "error",
    null_ok = TRUE
  )

  # Treat the column-names-as-strings as symbols
  if (!is.null(group_vars)) {
    group_vars_syms <- rlang::syms(group_vars)
  } else if (is.null(group_vars)) {
    group_vars_syms <- NULL
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

  if (bootstrap_ci) {
    # Bootstrap process
    bootstrap_data <- data |>
      dplyr::select({{ Ps_col }}, {{ outcome_col }}, !!!group_vars_syms) |> # Select only relevant columns
      dplyr::group_by(!!!group_vars_syms) |>
      infer::generate(reps = n_samples, type = "bootstrap") |> # Generate bootstrap samples
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
            .x,
            Ps_col = {{ Ps_col }},
            outcome_col = {{ outcome_col }},
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
  }

  # Initialize the bin_df to hold bin statistics
  bin_df <- bin_data$bin_stats |>
    dplyr::select(!!!group_vars_syms, bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin using the start and end points
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(!!!group_vars_syms, bin_number) # Sort the bins by bin_number

  if (bootstrap_ci) {
    # Initialize the bind_df_boot to hold bin statistics for each replicate
    # Initialize the bin_df with bootstrap samples
    bin_df_boot <- bin_data_boot |>
      dplyr::select(
        !!!group_vars_syms,
        replicate,
        bin_number,
        bin_start,
        bin_end
      ) |>
      # Calculate the midpoint of each bin for each bootstrap replicate
      dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
      dplyr::arrange(!!!group_vars_syms, replicate, bin_number) # Sort by replicate and bin_number
  }
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

  if (bootstrap_ci) {
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
  }

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
      dplyr::left_join(
        bin_df,
        by = dplyr::join_by(!!!rlang::syms(group_vars), bin_number)
      ) |>
      dplyr::group_by(!!!group_vars_syms, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()
  }

  if (bootstrap_ci) {
    if (is.null(group_vars)) {
      # For the bootstrapped data
      # Join the bin statistics (bin_summary) with the bin_df_boot for further calculations
      # The merged data will contain the bin information and corresponding statistics
      # Not using AntiM_b = -1 * midpoint + 1
      # i.e., Anticipated mortality (1 - midpoint, reversed scale)
      bin_stats_boot <- bin_summary_boot |>
        dplyr::left_join(
          bin_df_boot,
          by = dplyr::join_by(replicate, bin_number)
        ) |>
        dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
        dplyr::mutate(
          R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
        ) |>
        dplyr::ungroup()
    } else {
      bin_stats_boot <- bin_summary_boot |>
        dplyr::left_join(
          bin_df_boot,
          by = dplyr::join_by(!!!rlang::syms(group_vars), replicate, bin_number)
        ) |>
        dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
        dplyr::mutate(
          R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
        ) |>
        dplyr::ungroup()
    }
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
      population_RMM = pmin(
        1,
        pmax(-1, population_RMM, na.rm = TRUE),
        na.rm = TRUE
      ), # Ensure RMM is within [-1, 1]
      population_CI = 1.96 *
        sqrt(
          (sum(AntiM_b, na.rm = TRUE) * sum(AntiS_b, na.rm = TRUE)) /
            sum(N_b, na.rm = TRUE)
        ),
      # Lower bound of 95% CI
      population_RMM_LL = pmax(
        -1,
        population_RMM - population_CI,
        na.rm = TRUE
      ), # Clip LL
      # Upper bound of 95% CI
      population_RMM_UL = pmin(1, population_RMM + population_CI, na.rm = TRUE), # Clip UL,
      .groups = "drop"
    ) |>
    dplyr::relocate(population_RMM_LL, .before = population_RMM) |>
    dplyr::relocate(population_CI, .after = population_RMM_UL)

  if (bootstrap_ci) {
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
  }

  if (bootstrap_ci) {
    if (is.null(group_vars)) {
      # add the confidence intervals from the bootstrap distribution
      # to the final result
      rmm_result_final <- rmm_result |>
        dplyr::bind_cols(rmm_result_ci) |>
        dplyr::relocate(bootstrap_RMM_LL, .before = bootstrap_RMM) |>
        dplyr::relocate(bootstrap_RMM_UL, .after = bootstrap_RMM) |>
        dplyr::relocate(bootstrap_CI, .after = bootstrap_RMM_UL) |>
        dplyr::select(
          -numerator,
          -denominator,
          -sd_bootstrap_RMM,
          -se_bootstrap_RMM
        )
    } else {
      rmm_result_final <- rmm_result |>
        dplyr::left_join(
          rmm_result_ci,
          by = dplyr::join_by(!!!rlang::syms(group_vars))
        ) |>
        dplyr::relocate(bootstrap_RMM_LL, .before = bootstrap_RMM) |>
        dplyr::relocate(bootstrap_RMM_UL, .after = bootstrap_RMM) |>
        dplyr::relocate(bootstrap_CI, .after = bootstrap_RMM_UL) |>
        dplyr::select(
          -numerator,
          -denominator,
          -sd_bootstrap_RMM,
          -se_bootstrap_RMM
        )
    }
  } else {
    rmm_result_final <- rmm_result |>
      dplyr::select(
        -numerator,
        -denominator
      )
  }

  # Return the final result containing the RMM and its confidence intervals
  # optionally, pivot
  if (pivot && is.null(group_vars)) {
    rmm_result_final <- rmm_result_final |>
      tidyr::pivot_longer(
        tidyselect::everything(),
        names_to = "stat",
        values_to = "value"
      )

    return(rmm_result_final)
  } else if (pivot && !is.null(group_vars)) {
    rmm_result_final <- rmm_result_final |>
      tidyr::pivot_longer(
        -dplyr::all_of(group_vars),
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
#' @inheritParams rmm
#'
#' @details
#' Like other statistical computing functions, `rm_bin_summary()` is happiest
#' without missing data.  It is best to pass complete probability of survival
#' and mortality outcome data to the function for optimal performance. With
#' smaller datasets, this is especially helpful.  However, `rm_bin_summary()`
#' will throw a warning about missing values, if any exist in `Ps_col` and/or
#' `outcome_col`.
#'
#' `rm_bin_summary()` assumes `Ps_col` contains probabilities derived from
#' real-world inputs for the Trauma Injury Severity Score (TRISS) model.
#' Synthetic or low-variability data (especially with small sample sizes) may
#' not reflect the distribution of TRISS-derived survival probabilities. This
#' can result in unstable estimates or function failure due to insufficient
#' dispersion. With small sample sizes, it may be important to use smaller
#' values with the divisor arguments and adjust the thresholds (based on the
#' distribution of the `Ps_col` values) to create bins that better accommodate
#' the data.
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
#'      the bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)
#'   - `bootstrap_RMM`: The average RMM value calculated for the bootstrap
#'      sample. (optional, if `bootstrap_ci = TRUE`)
#'   - `bootstrap_RMM_UL`: The upper bound of the 95% confidence interval for
#'      the bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)
#'   - `bootstrap_CI`: The width of the 95% confidence interval for the
#'      bootstrap RMM. (optional, if `bootstrap_ci = TRUE`)
#'
#' @note
#'
#' This function will produce the most reliable and interpretable results when
#' using a dataset that has one row per patient, with each column being a
#' feature.
#'
#' By default, `rm_bin_summary()` derives bin cut points from the full dataset’s
#' distribution. This ensures comparability across groups when `group_vars` is
#' used. To tailor results to a specific group (e.g., a single hospital), filter
#' the dataset to that subgroup before calling `rm_bin_summary()`. The function
#' will then compute bins and related statistics using only that subset’s
#' `Ps_col` distribution. When `group_vars` is used, and ff a group lacks
#' observations within one or more bins, `rm_bin_summary()` will compute
#' statistics only for the bins that contain data. Bins with no observations are
#' excluded from the summary for that group.

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
#' @seealso [probability_of_survival()], [rmm()], and [nonlinear_bins()]
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#'
#' # Parameters
#' # Total number of patients
#' n_patients <- 5000
#'
#' # Arbitrary group labels
#' groups <- sample(x = LETTERS[1:2], size = n_patients, replace = TRUE)
#'
#' # Trauma types
#' trauma_type_values <- sample(
#'   x = c("Blunt", "Penetrating"),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # RTS values
#' rts_values <- sample(
#'   x = seq(from = 0, to = 7.8408, by = 0.005),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # patient ages
#' ages <- sample(
#'   x = seq(from = 0, to = 100, by = 1),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # ISS scores
#' iss_scores <- sample(
#'   x = seq(from = 0, to = 75, by = 1),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # Generate survival probabilities (Ps)
#' Ps <- traumar::probability_of_survival(
#'   trauma_type = trauma_type_values,
#'   age = ages,
#'   rts = rts_values,
#'   iss = iss_scores
#' )
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes, groups = groups) |>
#'   dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Example usage of the `rm_bin_summary()` function
#' rm_bin_summary(
#'   data = data,
#'   Ps_col = Ps,
#'   outcome_col = survival,
#'   n_samples = 10,
#'   Divisor1 = 4,
#'   Divisor2 = 4
#' )
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
#' # Example usage of the `rm_bin_summary()` function with grouping
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
rm_bin_summary <- function(
  data,
  Ps_col,
  outcome_col,
  group_vars = NULL,
  n_samples = 100,
  Divisor1 = 5,
  Divisor2 = 5,
  Threshold_1 = 0.9,
  Threshold_2 = 0.99,
  bootstrap_ci = TRUE,
  seed = NULL
) {
  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  validate_data_structure(
    input = data,
    structure_type = c("data.frame", "tbl", "tbl_df"),
    logic = "or",
    type = "error"
  )

  # check the n_samples value
  validate_class(
    input = n_samples,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error"
  )

  # validate Divisor1
  validate_class(
    input = Divisor1,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error"
  )

  # validate Divisor2
  validate_class(
    input = Divisor2,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error"
  )

  # validate Threshold_1
  validate_numeric(input = Threshold_1, min = 0, max = 1, type = "error")

  # validate Threshold_2
  validate_numeric(input = Threshold_2, min = 0, max = 1, type = "error")

  # Validate the CI argument
  validate_class(input = bootstrap_ci, class_type = "logical", type = "error")

  # Ensure Ps_col and outcome_col arguments are provided with tailored error messages
  if (missing(Ps_col) && missing(outcome_col)) {
    cli::cli_abort(
      "Both {.var Ps_col} and {.var outcome_col} arguments must be provided."
    )
  } else if (missing(Ps_col)) {
    cli::cli_abort("The {.var Ps_col} argument must be provided.")
  } else if (missing(outcome_col)) {
    cli::cli_abort("The {.var outcome_col} argument must be provided.")
  }

  # Pull and check the outcome column
  binary_data <- data |> dplyr::pull({{ outcome_col }})

  # Ensure the column is either logical or numeric
  validate_class(
    input = binary_data,
    class_type = c("numeric", "logical", "integer"),
    logic = "or",
    type = "error",
    var_name = "outcome_col"
  )

  # Get unique non-missing values
  non_missing <- stats::na.omit(binary_data)

  # Validate type and values
  if (is.logical(non_missing)) {
    # Logical vector: ensure only TRUE/FALSE (no coercion needed)
    validate_set(
      input = non_missing,
      valid_set = c(TRUE, FALSE),
      type = "error",
      var_name = "outcome_col"
    )
  } else if (is.numeric(non_missing)) {
    # Numeric vector: ensure strictly 0 or 1
    validate_set(
      input = non_missing,
      valid_set = c(0, 1),
      type = "error",
      var_name = "outcome_col"
    )
  } else if (is.integer(non_missing)) {
    # Integer vector: ensure strictly 0 or 1
    validate_set(
      input = non_missing,
      valid_set = c(0L, 1L),
      type = "error",
      var_name = "outcome_col"
    )
  } else {
    # Not logical or numeric
    cli::cli_abort(
      "The {.var outcome_col} must be either logical (TRUE/FALSE) or numeric (1/0)."
    )
  }

  # Warn if missing
  validate_complete(
    input = binary_data,
    type = "warning",
    var_name = "outcome_col"
  )

  # Check if Ps column is numeric

  # dplyr::pull the Ps data
  Ps_check <- data |> dplyr::pull({{ Ps_col }})

  # check the Ps_check remains continuous
  # Check if Ps column is continuous (values between 0 and 1)
  validate_numeric(
    input = Ps_check,
    min = 0,
    max = 1,
    type = "error",
    var_name = "Ps_col"
  )

  # Warn if any missings in Ps_col
  validate_complete(input = Ps_check, type = "warning", var_name = "Ps_col")

  # Validate the seed argument
  validate_class(
    input = seed,
    class_type = c("numeric", "integer"),
    logic = "or",
    type = "error",
    null_ok = TRUE
  )

  # Set the random seed if a value is given
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Check if all elements in group_vars are strings (i.e., character vectors)
  validate_character_factor(input = group_vars, type = "error", null_ok = TRUE)

  # Check if all group_vars exist in the data
  validate_names(
    input = data,
    check_names = group_vars,
    type = "error",
    null_ok = TRUE
  )

  # Treat the column-names-as-strings as symbols
  if (!is.null(group_vars)) {
    group_vars_syms <- rlang::syms(group_vars)
  } else if (is.null(group_vars)) {
    group_vars_syms <- NULL
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

  if (bootstrap_ci) {
    # Bootstrap process
    bootstrap_data <- data |>
      dplyr::select({{ Ps_col }}, {{ outcome_col }}, !!!group_vars_syms) |> # Select only relevant columns
      dplyr::group_by(!!!group_vars_syms) |>
      infer::generate(reps = n_samples, type = "bootstrap") |> # Generate bootstrap samples
      dplyr::ungroup()

    # bootstrapping to get bins for the population to then create
    # the confidence intervals
    # Nest data by replicate and apply nonlinear_bins
    bin_data_boot <- bootstrap_data |>
      tidyr::nest(data = -replicate) |> # Nest data by replicate
      dplyr::mutate(
        bins = purrr::map(
          data,
          ~ nonlinear_bins(
            .x,
            Ps_col = {{ Ps_col }},
            outcome_col = {{ outcome_col }},
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
  }

  # Initialize the bin_df to hold bin statistics
  bin_df <- bin_data$bin_stats |>
    dplyr::select(!!!group_vars_syms, bin_number, bin_start, bin_end) |>
    # Calculate the midpoint of each bin using the start and end points
    dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
    dplyr::arrange(!!!group_vars_syms, bin_number) # Sort the bins by bin_number

  if (bootstrap_ci) {
    # Initialize the bind_df_boot to hold bin statistics for each replicate
    # Initialize the bin_df with bootstrap samples
    bin_df_boot <- bin_data_boot |>
      dplyr::select(
        !!!group_vars_syms,
        replicate,
        bin_number,
        bin_start,
        bin_end
      ) |>
      # Calculate the midpoint of each bin for each bootstrap replicate
      dplyr::mutate(midpoint = (bin_end + bin_start) / 2) |>
      dplyr::arrange(!!!group_vars_syms, replicate, bin_number) # Sort by replicate and bin_number
  }

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

  if (bootstrap_ci) {
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
  }

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
      dplyr::left_join(
        bin_df,
        by = dplyr::join_by(!!!rlang::syms(group_vars), bin_number)
      ) |>
      dplyr::group_by(!!!group_vars_syms, bin_number) |>
      dplyr::mutate(
        R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
      ) |>
      dplyr::ungroup()
  }

  if (bootstrap_ci) {
    if (is.null(group_vars)) {
      # For the bootstrapped data
      # Join the bin statistics (bin_summary) with the bin_df_boot for further calculations
      # The merged data will contain the bin information and corresponding statistics
      # Not using AntiM_b = -1 * midpoint + 1
      # i.e., Anticipated mortality (1 - midpoint, reversed scale)
      bin_stats_boot <- bin_summary_boot |>
        dplyr::left_join(
          bin_df_boot,
          by = dplyr::join_by(replicate, bin_number)
        ) |>
        dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
        dplyr::mutate(
          R_b = bin_end - bin_start, # Calculate the bin width (R_b = end - start)
        ) |>
        dplyr::ungroup()
    } else {
      bin_stats_boot <- bin_summary_boot |>
        dplyr::left_join(
          bin_df_boot,
          by = dplyr::join_by(!!!rlang::syms(group_vars), replicate, bin_number)
        ) |>
        dplyr::group_by(!!!group_vars_syms, replicate, bin_number) |>
        dplyr::mutate(
          R_b = bin_end - bin_start # Calculate the bin width (R_b = end - start)
        ) |>
        dplyr::ungroup()
    }
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
      population_RMM = pmin(
        1,
        pmax(-1, population_RMM, na.rm = TRUE),
        na.rm = TRUE
      ), # Ensure RMM is within [-1, 1]
      population_CI = 1.96 *
        sqrt(
          (sum(AntiM_b, na.rm = TRUE) * sum(AntiS_b, na.rm = TRUE)) /
            sum(N_b, na.rm = TRUE)
        ),
      # Lower bound of 95% CI
      population_RMM_LL = pmax(
        -1,
        population_RMM - population_CI,
        na.rm = TRUE
      ), # Clip LL
      # Upper bound of 95% CI
      population_RMM_UL = pmin(1, population_RMM + population_CI, na.rm = TRUE), # Clip UL,
    ) |>
    dplyr::ungroup() |>
    dplyr::relocate(population_RMM_LL, .before = population_RMM) |>
    dplyr::relocate(population_CI, .after = population_RMM_UL)

  if (bootstrap_ci) {
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
      dplyr::left_join(
        rmm_result_ci,
        by = dplyr::join_by(!!!rlang::syms(group_vars), bin_number)
      ) |>
      dplyr::relocate(bootstrap_RMM_LL, .before = bootstrap_RMM) |>
      dplyr::relocate(bootstrap_RMM_UL, .after = bootstrap_RMM) |>
      dplyr::relocate(bootstrap_CI, .after = bootstrap_RMM_UL) |>
      dplyr::select(
        -numerator,
        -denominator,
        -sd_bootstrap_RMM,
        -se_bootstrap_RMM
      )
  } else {
    rmm_result_final <- rmm_result |>
      dplyr::select(-numerator, -denominator)
  }

  # complete
  return(rmm_result_final)
}
