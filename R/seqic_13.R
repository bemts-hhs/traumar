#' @title SEQIC Indicator 13 – Validation of  Trauma Registry Records
#'
#' @description Calculates the proportion of trauma records that meet or exceed
#' a threshold for data validity among facilities at the specified trauma center
#' levels. Optionally computes confidence intervals.
#'
#' @inheritParams seqic_indicator_1
#' @param validity_score Numeric. The proportion of each trauma registry record
#'   that is valid, expressed as a percentage (0–100). Typically calculated by
#'   the registry system.
#' @param validity Numeric. The minimum acceptable validity percentage threshold
#'   for records to be counted in the numerator. Defaults to `85`.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details
#'
#' This function:
#' \itemize{
#'   \item Filters to include only patients treated at trauma centers with
#'   levels specified in `included_levels` (default: Levels I–IV).
#'   \item Deduplicates the dataset using `unique_incident_id` to ensure each
#'   incident is counted only once.
#'   \item Flags records with a `validity_score` greater than or equal to the
#'   specified `validity_threshold` threshold (default: 85).
#'   \item Calculates the proportion of valid records among all included
#'   records.
#'   \item Optionally calculates binomial confidence intervals using the method
#'   specified in `calculate_ci` via `nemsqa_binomial_confint()`.
#'   \item Adds a "Population/Sample" label unless grouping is applied via
#'   `groups`.
#' }
#'
#' Users must ensure that appropriate column names are passed using tidy
#' evaluation (bare column names) and that the input data has been cleaned and
#' includes no missing or malformed identifiers, trauma level classifications,
#' or validity scores.
#'
#' @returns
#' A tibble containing:
#' \itemize{
#'   \item `numerator_13`: Number of records with validity score >= `validity`.
#'   \item `denominator_13`: Total number of included trauma records.
#'   \item `seqic_13`: Proportion of records in `df` meeting the `validity`
#'   threshold.
#'   \item `lower_ci_13`,
#'   \item `upper_ci_13`: (Optional) 95% confidence interval bounds.
#'   \item `Data`: Label indicating population/sample-level summary.
#' }
#'
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Simulated data for SEQIC Indicator 13
#' test_data <- tibble::tibble(
#'   id = as.character(1:12),
#'   trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV", "I", "II",
#'   "III", "IV"),
#'   validity = c(90, 80, 88, 92, 86, 75, 89, 70, 95, 85, 83, 87)
#' )
#'
#' # Run the function
#' traumar::seqic_indicator_13(
#'   df = test_data,
#'   level = trauma_level,
#'   included_levels = c("I", "II", "III", "IV"),
#'   unique_incident_id = id,
#'   validity_score = validity,
#'   validity_threshold = 85,
#'   calculate_ci = "wilson"
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_13 <-
  function(
    df,
    level,
    included_levels = c("I", "II", "III", "IV"),
    unique_incident_id,
    validity_score,
    validity_threshold = 85,
    groups = NULL,
    calculate_ci = NULL,
    ...
  ) {
    ###___________________________________________________________________________
    ### Data validation
    ###___________________________________________________________________________

    # Ensure input is a data frame or tibble
    if (!is.data.frame(df) && !tibble::is_tibble(df)) {
      cli::cli_abort(c(
        "{.var df} must be a data frame or tibble.",
        "i" = "You provided an object of class {.cls {class(df)}}."
      ))
    }

    # Validate trauma level input column
    level_check <- df |> dplyr::pull({{ level }})
    if (!is.character(level_check) && !is.factor(level_check)) {
      cli::cli_abort(c(
        "{.var level} must be character or factor.",
        "i" = "Provided class: {.cls {class(level_check)}}."
      ))
    }

    # Make the `unique_incident_id` column accessible for validation.
    unique_incident_id_check <- df |>
      dplyr::pull({{ unique_incident_id }})

    # Validate `unique_incident_id` to ensure it's either character or factor.
    if (
      !is.character(unique_incident_id_check) &&
        !is.factor(unique_incident_id_check) &&
        !is.numeric(unique_incident_id_check)
    ) {
      cli::cli_abort(
        c(
          "{.var unique_incident_id} must be of class {.cls character}, {.cls numeric}, or {.cls factor}.",
          "i" = "{.var unique_incident_id} was an object of class {.cls {class(unique_incident_id_check)}}."
        )
      )
    }

    # Validate that validity_score is numeric
    validity_score_check <- df |> dplyr::pull({{ validity_score }})
    if (!is.numeric(validity_score_check)) {
      cli::cli_abort(
        c(
          "{.var validity_score} must be of class {.cls numeric}.",
          "i" = "{.var validity_score} was an object of class {.cls {class(validity_score_check)}}."
        )
      )
    }

    if (
      any(validity_score_check < 0, na.rm = TRUE) ||
        any(validity_score_check > 100, na.rm = TRUE)
    ) {
      cli::cli_abort(
        c(
          "{.var validity_score} must have a range of [{cli::col_blue('0, 100')}].",
          "i" = "{.var validity_score} was out of range, with a range of [{cli::col_red(round(min(validity_score_check, na.rm = TRUE), digits = 3))}, {cli::col_red(round(max(validity_score_check, na.rm = TRUE), digits = 3))}]."
        )
      )
    }

    # Validate that validity_score is numeric
    if (!is.numeric({{ validity_threshold }})) {
      cli::cli_abort(
        c(
          "{.var validity_threshold} must be of class {.cls numeric}.",
          "i" = "{.var validity_threshold} was an object of class {.cls {class({{ validity_threshold }})}}."
        )
      )
    }

    # Check if all elements in groups are strings (i.e., character vectors)
    if (!is.null(groups)) {
      if (!is.character(groups)) {
        cli::cli_abort(c(
          "All elements in {.var groups} must be strings.",
          "i" = "You passed an object of class {.cls {class(groups)}} to {.var groups}."
        ))
      }
    }

    # Check if all groups exist in the `df`
    if (!all(groups %in% names(df))) {
      invalid_vars <- groups[!groups %in% names(df)]
      cli::cli_abort(
        "Invalid grouping variable(s): {paste(invalid_vars, collapse = ', ')}"
      )
    }

    # Validate the `included_levels` argument
    if (
      !is.character(included_levels) &&
        !is.numeric(included_levels) &&
        !is.factor(included_levels)
    ) {
      cli::cli_abort(
        c(
          "{.var included_levels} must be of class {.cls character}, {.cls factor}, or {.cls numeric}.",
          "i" = "{.var included_levels} was an object of class {.cls {class(included_levels)}}."
        )
      )
    }

    # Validate confidence interval method
    if (!is.null(calculate_ci)) {
      attempt <- try(
        match.arg(calculate_ci, choices = c("wilson", "clopper-pearson")),
        silent = TRUE
      )
      if (inherits(attempt, "try-error")) {
        cli::cli_abort(c(
          "If {.var calculate_ci} is not NULL, it must be {.val wilson} or {.val clopper-pearson}.",
          "i" = "Provided value: {.val {calculate_ci}}"
        ))
      }
      calculate_ci <- attempt
    }

    ###_________________________________________________________________________
    ### Calculations
    ###_________________________________________________________________________

    seqic_13 <- df |>
      # Filter the dataset to include only records from facilities matching
      # the trauma levels specified in `included_levels` (default: I–IV).
      dplyr::filter({{ level }} %in% included_levels) |>

      # Deduplicate the dataset to ensure each trauma incident is represented
      # only once. This is essential to avoid overcounting when calculating proportions.
      # The `unique_incident_id` column is assumed to uniquely identify each incident.
      dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>

      # Summarize the filtered and deduplicated dataset:
      dplyr::summarize(
        # Count the number of valid records that meet or exceed the threshold
        # set by the `validity` argument (default: 85). Missing values are ignored.
        numerator_13 = sum(
          {{ validity_score }} >= {{ validity_threshold }},
          na.rm = TRUE
        ),

        # Count the total number of records in the denominator after filtering
        # and deduplication. This represents all valid candidate records.
        denominator_13 = dplyr::n(),

        # Calculate the proportion of valid records (numerator / denominator),
        # rounded to 3 decimal places for reporting clarity.
        seqic_13 = dplyr::if_else(
          denominator_13 > 0,
          numerator_13 / denominator_13,
          NA_real_
        ),
        .by = {{ groups }}
      )

    # Optionally calculate confidence intervals for the proportions:
    # - If `calculate_ci` is provided, apply a binomial confidence interval method
    # (Wilson or Clopper-Pearson).
    # - `nemsqa_binomial_confint()` calculates the confidence intervals for the
    # proportion.
    # - Select only the relevant columns and rename the CI columns for clarity.
    if (!is.null(calculate_ci)) {
      seqic_13 <- seqic_13 |>
        dplyr::bind_cols(
          nemsqar::nemsqa_binomial_confint(
            data = seqic_13,
            x = numerator_13, # Number of successes (numerator).
            n = denominator_13, # Number of trials (denominator).
            method = calculate_ci, # Confidence interval calculation method (e.g., "wilson").
            ...
          ) |>
            dplyr::select(lower_ci, upper_ci) |>
            dplyr::rename(lower_ci_13 = lower_ci, upper_ci_13 = upper_ci) # Rename CI columns.
        )
    }

    # Assign a label to indicate whether the data represents population or sample-level results:
    # - If no grouping is applied, label the data as "Population/Sample".
    if (is.null(groups)) {
      seqic_13 <- seqic_13 |>
        tibble::add_column(Data = "Population/Sample", .before = "numerator_13") # Add the label column.
    } else if (!is.null(groups)) {
      seqic_13 <- seqic_13 |>
        dplyr::arrange(!!!rlang::syms(groups))
    }

    return(seqic_13)
  }
