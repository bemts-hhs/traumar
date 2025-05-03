#' @title SEQIC Indicator 2 – Missing Incident Time
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function calculates System Evaluation and Quality Improvement Committee
#' (SEQIC) Indicator 2. This indicator evaluates the proportion of trauma
#' incidents with missing documented incident time across Level I–IV trauma
#' centers. Confidence intervals can optionally be calculated for the
#' proportion, using either the Wilson or Clopper-Pearson method.
#'
#' @inheritParams seqic_indicator_1
#' @param incident_time The time the patient's injury occurred.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details This function:
#' \itemize{
#'   \item Filters trauma records to those with a trauma center level of I–IV.
#'   \item Deduplicates by `unique_incident_id` to ensure one record per
#'   incident.
#'   \item Calculates the proportion of cases missing `incident_time`.
#' }
#'
#' @note
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @return A tibble summarizing SEQIC Indicator 2 results. Includes numerator,
#'   denominator, and performance rate for the indicator. 95% confidence intervals
#'   are provided optionally.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_2 <- function(
  df,
  unique_incident_id,
  level,
  included_levels = c("I", "II", "III", "IV"),
  incident_time,
  groups = NULL,
  calculate_ci = NULL,
  ...
) {
  ###___________________________________________________________________________
  ### Data validation
  ###___________________________________________________________________________

  # validate `df`
  if (!is.data.frame(df) && tibble::is_tibble(df)) {
    cli::cli_abort(
      c(
        "{.var df} must be of class {.cls data.frame} or {.cls tibble}.",
        "i" = "{.var df} was an object of class {.cls {class(df}}."
      )
    )
  }

  # make the `unique_incident_id` column accessible for validation
  unique_incident_id_check <- df |>
    dplyr::pull({{ unique_incident_id }})

  # validate `unique_incident_id`
  if (
    !is.character(unique_incident_id_check) &&
      !is.factor(unique_incident_id_check)
  ) {
    cli::cli_abort(
      c(
        "{.var unique_incident_id} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var unique_incident_id} was an object of class {.cls {class(unique_incident_id_check)}}."
      )
    )
  }

  # make the `incident_time` column accessible for validation
  incident_time_check <- df |>
    dplyr::pull({{ incident_time }})

  # validate `incident_time`
  if (
    !is.character(incident_time_check) &&
      !is.factor(incident_time_check)
  ) {
    cli::cli_abort(
      c(
        "{.var incident_time} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var incident_time} was an object of class {.cls {class(incident_time_check)}}."
      )
    )
  }

  # make the `level` column accessible for validation
  level_check <- df |> dplyr::pull({{ level }})

  # validate `level`
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(
      c(
        "{.var level} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var level} was an object of class {.cls {class(level_check)}}."
      )
    )
  }

  # Check if all elements in groups are strings (i.e., character vectors)
  if (!all(sapply(groups, is.character))) {
    cli::cli_abort(c(
      "All elements in {.var groups} must be strings.",
      "i" = "You passed a {.cls {class(groups)}} variable to {.var groups}."
    ))
  }

  # Check if all groups exist in the `df`
  if (!all(groups %in% names(df))) {
    invalid_vars <- groups[!groups %in% names(df)]
    cli::cli_abort(
      "The following group variable(s) are not valid columns in {.var df}: {paste(invalid_vars, collapse = ', ')}"
    )
  }

  # Validate the `calculate_ci` argument
  # - If not NULL, must be either "wilson" or "clopper-pearson"
  # - Use match.arg() to enforce allowed values
  # - Catch invalid input silently and report cleanly with cli
  if (!is.null(calculate_ci)) {
    # Attempt to match the argument against allowed choices
    attempt <- try(
      match.arg(calculate_ci, choices = c("wilson", "clopper-pearson")),
      silent = TRUE
    )

    # If match.arg failed, provide a user-friendly error message
    if (inherits(attempt, "try-error")) {
      cli::cli_abort(
        c(
          "If {.var calculate_ci} is not {cli::col_blue('NULL')}, it must be {.val wilson} or {.val clopper-pearson}.",
          "i" = "{.var calculate_ci} was {.val {calculate_ci}}."
        )
      )
    }

    # If valid, overwrite calculate_ci with standardized value
    calculate_ci <- attempt
  }

  # Summarize the data for Indicator 2:
  # - Filter records to include only Level I–IV trauma centers.
  # - Remove duplicate incidents, keeping the first occurrence of each unique `unique_incident_id`.
  # - Summarize the data by calculating:
  #   - `numerator_2`: The number of incidents with missing `incident_time`.
  #   - `denominator_2`: The total number of unique incidents.
  #   - `seqic_2`: The proportion of incidents with missing `incident_time` (rounded to 3 decimal places).
  #   - Optionally, group results by columns specified in the `groups` argument.
  seqic_2 <- df |>
    dplyr::filter({{ level }} %in% included_levels) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::summarize(
      numerator_2 = sum(is.na({{ incident_time }})), # Calculate the number of missing incident_time values.
      denominator_2 = dplyr::n(), # Count the total number of unique incidents.
      seqic_2 = dplyr::if_else(
        denominator_2 > 0,
        numerator_2 / denominator_2,
        NA_real_
      ), # Calculate the proportion for Indicator 2.
      .by = {{ groups }} # Group by the specified columns (if any).
    )

  # Optionally calculate confidence intervals for the proportions:
  # - If `calculate_ci` is provided, apply a binomial confidence interval method (Wilson or Clopper-Pearson).
  # - `nemsqa_binomial_confint()` calculates the confidence intervals for the proportion.
  # - Select only the relevant columns and rename the CI columns for clarity.
  if (!is.null(calculate_ci)) {
    seqic_2 <- seqic_2 |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_2,
          x = numerator_2, # Number of successes (numerator).
          n = denominator_2, # Number of trials (denominator).
          method = calculate_ci, # Confidence interval calculation method (e.g., "wilson").
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_2 = lower_ci, upper_ci_2 = upper_ci) # Rename CI columns.
      )
  }

  # Assign a label to indicate whether the data represents population or sample-level results:
  # - If no grouping is applied, label the data as "Population/Sample".
  if (is.null(groups)) {
    seqic_2 <- seqic_2 |>
      tibble::add_column(Data = "Population/Sample", .before = "numerator_2") # Add the label column.
  } else if (!is.null(groups)) {
    seqic_2 <- seqic_2 |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  # Return the final summary dataframe for Indicator 2.
  return(seqic_2)
}
