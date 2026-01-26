#' @title Create Nonlinear Probability of Survival Bins
#'
#' @description
#'
#' This function generates nonlinear bins for probability of survival data based
#' on specified thresholds and divisors as specified in Napoli et al. (2017),
#' Schroeder et al. (2019), and Kassar et al. (2016). This function calculates
#' bin statistics, including mean, standard deviation, total alive, total dead,
#' count, and percentage for each bin.
#'
#' @param data A `data.frame` or `tibble` containing the probability of survival
#'   data for a set of patients.
#' @param Ps_col The name of the column containing the survival probabilities
#'   (Ps). Should be numeric on a scale from 0 to 1.
#' @param outcome_col The name of the column containing the outcome data. It
#'   should be binary, with values indicating patient survival. A value of `1`
#'   should represent "alive" (survived), while `0` should represent "dead" (did
#'   not survive).  `TRUE/FALSE` are accepted as well. Ensure the column
#'   contains only these possible values.
#' @param group_vars Optional grouping variables for bin statistics
#'   calculations. These should be specified as quoted column names.
#' @param divisor1 A positive numeric value controlling the coarseness of bins
#'   for Ps values below `threshold_1`. It scales the number of steps from the
#'   start of the dataset up to the `threshold_1` cut point. Larger values
#'   produce fewer, broader bins; smaller values produce more, narrower bins.
#'   Defaults to `5`.
#' @param divisor2 A positive numeric value controlling the coarseness of bins
#'   for Ps values between `threshold_1` and `threshold_2`. Larger values yield
#'   wider bins, and smaller values yield narrower bins in this range. Defaults
#'   to `5`.
#' @param threshold_1 A numeric value that defines the lower bound of the
#'   high-survival probability range in `Ps_col`. The function identifies the
#'   first index where `Ps_col` exceeds this value and begins applying smaller
#'   bin widths from that point onward. Defaults to `0.9`, meaning binning
#'   changes once Ps > 0.90.
#' @param threshold_2 A numeric value that defines the upper bound of the
#'   high-survival probability range in `Ps_col`. The function identifies the
#'   first index where `Ps_col` exceeds this value. Between `threshold_1` and
#'   `threshold_2`, finer binning is applied; above `threshold_2`, binning may
#'   again change. Defaults to `0.99`, meaning the special binning range is
#'   between Ps values of 0.90 and 0.99.
#'
#' @details
#' Like other statistical computing functions, `nonlinear_bins()` is happiest
#' without missing data.  It is best to pass complete probability of survival
#' and outcome data to the function for optimal performance. With smaller
#' datasets, this is especially helpful.  However, `nonlinear_bins()` will throw
#' a warning about missing values, if any exist in `Ps_col` and/or
#' `outcome_col`.
#'
#' `nonlinear_bins()` assumes `Ps_col` contains probabilities derived from
#' real-world inputs for the Trauma Injury Severity Score (TRISS) model.
#' Synthetic or low-variability data (especially with small sample sizes) may
#' not reflect the distribution of TRISS-derived survival probabilities. This
#' can result in unstable estimates or function failure due to insufficient
#' dispersion. With small sample sizes, it may be important to use smaller
#' values with the divisor arguments and adjust the thresholds (based on the
#' distribution of the `Ps_col` values) to create bins that better accommodate
#' the data.
#'
#' By default, `nonlinear_bins()` derives bin cut points from the full dataset’s
#' distribution. This ensures comparability across groups when `group_vars` is
#' used. To tailor binning to a specific group (e.g., a single hospital), filter
#' the dataset to that subgroup before calling `nonlinear_bins()`. The function
#' will then compute bins and related statistics using only that subset’s
#' `Ps_col` distribution. When `group_vars` is used, and ff a group lacks
#' observations within one or more bins, `rm_bin_summary()` will compute
#' statistics only for the bins that contain data. Bins with no observations are
#' excluded from the summary for that group.
#'
#' The `threshold_1` and `threshold_2` arguments set probability cut points
#' that define the start and end of a high-survival range where bin widths are
#' adjusted for finer resolution. The `divisor1` and `divisor2` arguments are
#' scaling factors that determine how many bins are created before and within
#' this high-survival range, respectively. Lower divisors yield narrower bins,
#' capturing more detail, while higher divisors yield broader bins, smoothing
#' the distribution.
#'
#' @returns A list with two elements:
#'   - `intervals`: A vector defining bin boundaries for probability of
#'        survival.
#'   - `bin_stats`: A `tibble` containing:
#'     - `bin_number`: Bin index.
#'     - `bin_start`, `bin_end`: Bin range.
#'     - `mean`, `sd`: Mean and standard deviation of `Ps_col` within the bin.
#'     - `Pred_Survivors_b`, `Pred_Deaths_b`: Predicted counts of survivors and
#'        decedents, respectively.
#'     - `AntiS_b`, `AntiM_b`: Anticipated proportion survived, and deceased,
#'        respectively.
#'     - `alive`, `dead`: Count of observed survivors and non-survivors.
#'     - `count`: Total records in the bin.
#'     - `percent`: Percentage of total records within each bin.
#'
#' @note
#'
#' This function will produce the most reliable and interpretable results when
#' using a dataset that has one row per patient, with each column being a
#' feature.
#'
#' The `mean` and `AntiS_b` are approximately equivalent in this context.  They
#' are kept in the output for clarity.
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
#' @seealso [probability_of_survival()], [rmm()], and [rm_bin_summary()]
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
#' # Apply the nonlinear_bins function
#' results <- nonlinear_bins(
#'   data = data,
#'   Ps_col = Ps,
#'   outcome_col = survival,
#'   divisor1 = 4,
#'   divisor2 = 4,
#'   threshold_1 = 0.9,
#'   threshold_2 = 0.99
#' )
#'
#' # View results
#' results$intervals
#' results$bin_stats
#'
#' # Example with grouping by a categorical variable
#'
#' # Run the function using a single grouping variable
#' results_grouped <- nonlinear_bins(
#'   data,
#'   Ps_col = Ps,
#'   outcome_col = survival,
#'   group_vars = "groups"
#' )
#'
#' # View grouped results
#' results_grouped$bin_stats
#'
#' @author Nicolas Foss, Ed.D, MS, original implementation in MATLAB by Nicholas
#'   J. Napoli, Ph.D., MS
#'
nonlinear_bins <- function(
  data,
  Ps_col,
  outcome_col,
  group_vars = NULL,
  divisor1 = 5,
  divisor2 = 5,
  threshold_1 = 0.9,
  threshold_2 = 0.99
) {
  # Validation checks using `cli` for robust error messaging:
  # Ensures the input data is a data frame or tibble.
  validate_data_structure(
    input = data,
    structure_type = c("data.frame", "tbl", "tbl_df"),
    logic = "or",
    type = "error"
  )

  # Ensure Ps_col and outcome_col arguments are provided with tailored error
  # messages
  if (missing(Ps_col) && missing(outcome_col)) {
    cli::cli_abort(
      "Both {.var Ps_col} and {.var outcome_col} arguments must be provided."
    )
  } else if (missing(Ps_col)) {
    cli::cli_abort("The {.var Ps_col} argument must be provided.")
  } else if (missing(outcome_col)) {
    cli::cli_abort("The {.var outcome_col} argument must be provided.")
  }

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

  # Check for missingness in the probability of survival data
  validate_complete(input = Ps_check, type = "warning", var_name = "Ps_col")

  # Pull and check the outcome column
  binary_data <- data |> dplyr::pull({{ outcome_col }})

  # Ensure the column is either logical or numeric
  validate_class(
    input = binary_data,
    class_type = c("logical", "numeric", "integer"),
    logic = "or",
    type = "error",
    var_name = "outcome_col"
  )

  # Get unique non-missing values
  non_missing <- stats::na.omit(binary_data)

  # Provide a warning about missing values
  validate_complete(
    input = binary_data,
    type = "warning",
    var_name = "outcome_col"
  )

  # Validate type and values
  if (is.logical(binary_data)) {
    validate_set(
      input = non_missing,
      valid_set = c(TRUE, FALSE),
      type = "error",
      var_name = "outcome_col"
    )
  } else if (is.numeric(binary_data)) {
    # Numeric vector: ensure strictly 0 or 1
    validate_set(
      input = non_missing,
      valid_set = c(0, 1),
      type = "error",
      var_name = "outcome_col"
    )
  } else if (is.integer(binary_data)) {
    # Integer vector: ensure strictly 0 or 1
    validate_set(
      input = non_missing,
      valid_set = c(0L, 1L),
      type = "error",
      var_name = "outcome_col"
    )
  }

  # Check if all elements in group_vars are strings (i.e., character vectors)
  if (!is.null(group_vars)) {
    validate_character_factor(input = group_vars, type = "error")
  }

  # Check if all group_vars exist in the data
  if (!is.null(group_vars)) {
    validate_names(input = data, check_names = group_vars, type = "error")
  }

  # Treat the column-names-as-strings as symbols
  if (!is.null(group_vars)) {
    group_vars <- rlang::syms(group_vars)
  }

  # Select and sort the column
  survival_data <- data |> dplyr::pull({{ Ps_col }}) |> sort()

  # length of non-missing `Ps_col` must be >= 2
  validate_length(
    input = survival_data,
    min_length = 2,
    max_length = Inf,
    type = "error",
    na_ok = TRUE,
    null_ok = TRUE,
    var_name = "outcome_col"
  )

  # Step 1: Find indices for level thresholds
  loc_9A <- which(survival_data > threshold_1) # Everything above 0.9 or other threshold
  loc_9B <- which(survival_data > threshold_2) # Everything above 0.99 or other threshold
  loc_9C <- which(survival_data > threshold_1 & survival_data <= threshold_2) # Between 0.9 and 0.99 or other thresholds

  # Step 2: Define step sizes based on the data
  step1 <- round(suppressWarnings(min(loc_9A, na.rm = TRUE)) / divisor1)
  step2 <- round(length(loc_9C) / divisor2)

  # Step 3: Define intervals
  # Check that loc_9A and loc_9B are finite before using them in seq()
  if (
    suppressWarnings(
      is.finite(min(loc_9A)) && is.finite(min(loc_9B))
    )
  ) {
    # Defensive approach to control for potential error when `Ps_col`
    # is not sufficiently dispersed and/or has a low sample size.
    attempt <- try(
      unique(
        c(
          seq(
            from = 1,
            to = suppressWarnings(min(loc_9A, na.rm = TRUE)),
            by = step1
          ),
          seq(
            from = suppressWarnings(min(loc_9A, na.rm = TRUE)),
            to = suppressWarnings(min(loc_9B, na.rm = TRUE)),
            by = step2
          ),
          suppressWarnings(max(loc_9B, na.rm = TRUE))
        )
      ),
      silent = TRUE
    )

    if (inherits(attempt, "try-error")) {
      cli::cli_abort(c(
        "Unable to calculate valid step sizes for defining survival probability intervals due to non-finite values.",
        "i" = "This typically occurs when {.var Ps_col} has low variability or the sample size is too small to support the requested binning.",
        "v" = "Try reducing {.var divisor1} and {.var divisor2} to create fewer, broader bins.",
        "v" = "Examine the distribution of {.var Ps_col} and consider adjusting {.var threshold_1} and {.var threshold_2} to better match the data.",
        "v" = "Increasing the sample size may also improve dispersion and allow for more stable interval boundaries."
      ))
    } else {
      # If `Ps_col` meets distributional assumptions, proceed
      len <- attempt
    }
  } else {
    # If either of the parts of the interval calculation are not finite throw an
    # error
    cli::cli_abort(
      c(
        "Unable to calculate valid step sizes for defining survival probability intervals due to non-finite values.",
        "i" = "This typically occurs when {.var Ps_col} has low variability or the sample size is too small to support the requested binning.",
        "v" = "Try reducing {.var divisor1} and {.var divisor2} to create fewer, broader bins.",
        "v" = "Examine the distribution of {.var Ps_col} and consider adjusting {.var threshold_1} and {.var threshold_2} to better match the data.",
        "v" = "Increasing the sample size may also improve dispersion and allow for more stable interval boundaries."
      )
    )
  }

  # Generate intervals based on these positions
  intervals <- unique(survival_data[len])

  # Step 4: Bin statistics

  # Apply binning to each group separately
  data <- data |>
    dplyr::mutate(
      bin_number = .bincode(
        {{ Ps_col }},
        breaks = intervals,
        include.lowest = TRUE
      ),
      bin_start = intervals[bin_number], # Start of the bin
      bin_end = intervals[bin_number + 1] # End of the bin
    )

  # Optionally group data by dynamic group_vars
  # Or run the bin statistics on the whole dataset
  if (!is.null(group_vars)) {
    # Group the data by the specified group_vars and bin-related columns
    grouped_stats <- data |>
      dplyr::group_by(!!!group_vars, bin_number, bin_start, bin_end) |>
      dplyr::summarize(
        # Calculate the mean of the probability of survival (Ps_col)
        mean = mean({{ Ps_col }}, na.rm = TRUE),
        # Calculate the standard deviation of the probability of survival (Ps_col)
        sd = stats::sd({{ Ps_col }}, na.rm = TRUE),
        # Sum the predicted survivors based on the probability of survival (Ps_col)
        Pred_Survivors_b = sum({{ Ps_col }}, na.rm = TRUE),
        # Sum the predicted deaths based on the probability of survival (Ps_col)
        Pred_Deaths_b = sum(1 - {{ Ps_col }}, na.rm = TRUE),
        # Calculate the anticipated survival rate
        AntiS_b = dplyr::if_else(
          dplyr::n() > 0,
          Pred_Survivors_b / dplyr::n(),
          NA_real_
        ),
        # Calculate the anticipated mortality rate
        AntiM_b = dplyr::if_else(
          dplyr::n() > 0,
          Pred_Deaths_b / dplyr::n(),
          NA_real_
        ),
        # Count the number of alive patients
        alive = sum({{ outcome_col }} == 1, na.rm = TRUE),
        # Count the number of dead patients
        dead = sum({{ outcome_col }} == 0, na.rm = TRUE),
        # Count the total number of observations
        count = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::ungroup() |>
      # Group the data again by the specified group_vars
      dplyr::group_by(!!!group_vars) |>
      dplyr::mutate(
        # Calculate the percentage of each group
        percent = count / sum(count, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  } else {
    # Group the data by bin-related columns only
    grouped_stats <- data |>
      dplyr::group_by(bin_number, bin_start, bin_end) |>
      dplyr::summarize(
        # Calculate the mean of the probability of survival (Ps_col)
        mean = mean({{ Ps_col }}, na.rm = TRUE),
        # Calculate the standard deviation of the probability of survival (Ps_col)
        sd = stats::sd({{ Ps_col }}, na.rm = TRUE),
        # Sum the predicted survivors based on the probability of survival (Ps_col)
        Pred_Survivors_b = sum({{ Ps_col }}, na.rm = TRUE),
        # Sum the predicted deaths based on the probability of survival (Ps_col)
        Pred_Deaths_b = sum(1 - {{ Ps_col }}, na.rm = TRUE),
        # Calculate the anticipated survival; rate
        AntiS_b = dplyr::if_else(
          dplyr::n() > 0,
          Pred_Survivors_b / dplyr::n(),
          NA_real_
        ),
        # Calculate the anticipated mortality rate
        AntiM_b = dplyr::if_else(
          dplyr::n() > 0,
          Pred_Deaths_b / dplyr::n(),
          NA_real_
        ),
        # Count the number of alive patients
        alive = sum({{ outcome_col }} == 1, na.rm = TRUE),
        # Count the number of dead patients
        dead = sum({{ outcome_col }} == 0, na.rm = TRUE),
        # Count the total number of observations
        count = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        # Calculate the percentage of each group
        percent = count / sum(count, na.rm = TRUE)
      )
  }

  # Return a list with intervals and the bin statistics
  return(list(intervals = intervals, bin_stats = grouped_stats))
}
