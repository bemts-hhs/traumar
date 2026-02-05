#' @title SEQIC Indicator 13 – Validation of  Trauma Registry Records
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates the proportion of trauma records that meet or exceed
#' a threshold for data validity among facilities at the specified trauma center
#' levels. Optionally computes confidence intervals.
#'
#' @inheritParams seqic_indicator_1
#' @param validity_score Numeric. The proportion of each trauma registry record
#'   that is valid, expressed as a percentage (0–100). Typically calculated by
#'   the registry system.
#' @param validity_threshold Numeric. The minimum acceptable validity percentage
#'   threshold for records to be counted in the numerator. Defaults to `85`.
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
#' @return A tibble summarizing SEQIC Indicator 13 results. Includes numerator,
#'   denominator, and performance rate 95% confidence intervals are included if
#'   requested.
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
#'   data = test_data,
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
    data,
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

    # Validate if `data` is a data frame or tibble. ----
    validate_data_structure(
      input = data,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error"
    )

    # make the `level` column accessible for validation ----
    level_check <- validate_data_pull(
      input = data,
      type = "error",
      col = {{ level }},
      var_name = "level"
    )

    # validate `level` ----
    validate_character_factor(
      input = level_check,
      type = "error",
      var_name = "level"
    )

    # make the `unique_incident_id` column accessible for validation ----
    unique_incident_id_check <- validate_data_pull(
      input = data,
      type = "error",
      col = {{ unique_incident_id }},
      var_name = "unique_incident_id"
    )

    # Validate `unique_incident_id` ----
    validate_class(
      input = unique_incident_id_check,
      class_type = c("numeric", "integer", "character", "factor"),
      logic = "or",
      type = "error",
      var_name = "unique_incident_id"
    )

    # Ensure that `validity_score` can be validated
    validity_score_check <- validate_data_pull(
      input = data,
      col = {{ validity_score }},
      type = "error",
      var_name = "validity_score"
    )

    # Validate that validity_score is numeric
    validate_numeric(
      input = validity_score_check,
      min = 0,
      max = 100,
      type = "error",
      var_name = "validity_score"
    )

    # Validate that validity_threshold is numeric
    validate_numeric(
      input = validity_threshold,
      min = 0,
      max = 100,
      type = "error"
    )

    # Check if all elements in groups are strings (i.e., character vectors)
    if (!is.null(groups)) {
      if (!is.character(groups)) {
        cli::cli_abort(c(
          "All elements in {.var groups} must be strings.",
          "i" = "You passed an object of class {.cls {class(groups)}} to {.var groups}."
        ))
      }
    }

    # Check if all elements in groups are strings (i.e., character vectors) ----
    validate_character_factor(input = groups, type = "error", null_ok = TRUE)

    # Check if all `groups` exist in the `data` ----
    validate_names(
      input = data,
      check_names = groups,
      type = "error",
      var_name = "groups",
      null_ok = TRUE
    )

    # Validate the `calculate_ci` argument ----
    calculate_ci <- validate_choice(
      input = calculate_ci,
      choices = c("wilson", "clopper-pearson"),
      several.ok = FALSE,
      type = "error",
      null_ok = TRUE,
      var_name = "calculate_ci"
    )

    # Validate the `included_levels` argument ----
    validate_class(
      input = included_levels,
      class_type = c("numeric", "character", "factor", "integer"),
      type = "error",
      logic = "or"
    )

    ###_________________________________________________________________________
    ### Calculations ----
    ###_________________________________________________________________________

    seqic_13 <- data |>
      # Filter the dataset to include only records from facilities matching ----
      # the trauma levels specified in `included_levels` (default: I–IV).
      dplyr::filter({{ level }} %in% included_levels) |>

      # Deduplicate the dataset to ensure each trauma incident ----
      # is represented only once. This is essential to avoid overcounting when
      # calculating proportions. The `unique_incident_id` column is assumed to
      # uniquely identify each incident.
      dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>

      # Summarize the filtered and deduplicated dataset: ----
      dplyr::summarize(
        # Count the number of valid records ----
        # that meet or exceed the threshold set by the `validity` argument
        # (default: 85). Missing values are ignored.
        numerator_13 = sum(
          {{ validity_score }} >= {{ validity_threshold }},
          na.rm = TRUE
        ),

        # Count the total number of records in the denominator ----
        # after filtering and deduplication. This represents all valid candidate
        # records.
        denominator_13 = dplyr::n(),

        # Calculate the proportion of valid records ----
        # (numerator / denominator), rounded to 3 decimal places for reporting
        # clarity.
        seqic_13 = dplyr::if_else(
          denominator_13 > 0,
          numerator_13 / denominator_13,
          NA_real_
        ),
        .by = {{ groups }}
      )

    # Optionally calculate confidence intervals for the proportions: ----
    # - If `calculate_ci` is provided, apply a binomial confidence interval
    #   method (Wilson or Clopper-Pearson).
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

    # Assign a label to indicate whether the data represents population or ----
    # sample-level results:
    # - If no grouping is applied, label the data as "Population/Sample". ----
    if (is.null(groups)) {
      seqic_13 <- seqic_13 |>
        tibble::add_column(data = "population/sample", .before = "numerator_13") # Add the label column.
    } else if (!is.null(groups)) {
      seqic_13 <- seqic_13 |>
        dplyr::arrange(!!!rlang::syms(groups))
    }

    # Return SEQIC Indicator 13 results as a tibble ----
    return(seqic_13)
  }
