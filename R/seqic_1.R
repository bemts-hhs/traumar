#' @title SEQIC Indicator 1 (a–f) – Trauma Team Response Evaluation
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function calculates System Evaluation and Quality Improvement Committee
#' (SEQIC) Indicator 1 (subparts a through f). These indicators assess the
#' timeliness and type of provider response (e.g., surgeon, midlevel, physician)
#' to trauma alerts based on trauma team activation level, hospital trauma
#' level, and time to provider presence. Confidence intervals can optionally be
#' calculated for the proportion, using either the Wilson or Clopper-Pearson
#' method.
#'
#' @param df A data frame containing trauma incident records.
#' @param trauma_team_activation_level Column identifying trauma team activation
#'   level (e.g., Level 1, Level 2).
#' @param trauma_team_physician_service_type Column indicating the type of
#'   medical provider (e.g., Surgery/Trauma, Emergency Medicine). For indicators
#'   1a, 1b, and 1c, `seqic_indicator_1()` will only look for records with the
#'   trauma team member service type documented as marked as 'Surgery/Trauma'.
#'   For Indicators 1d, 1e, and 1f, `seqic_indicator_1()` will look for the
#'   following service types:
#'   \itemize{
#'    \item "Surgery/Trauma",
#'    \item "Emergency Medicine",
#'    \item "Family Practice",
#'    \item "Nurse Practitioner",
#'    \item "Physician Assistant",
#'    \item "Surgery Senior Resident",
#'    \item "Hospitalist",
#'    \item "Internal Medicine"
#'    }
#' @param level Column indicating the trauma center designation level (e.g., I,
#'   II, III, IV).
#' @param unique_incident_id Unique identifier for each record.
#' @param response_time Numeric variable representing the time (in minutes)
#'   to provider response.
#' @param trauma_team_activation_provider Column identifying the responding
#'   provider for trauma activation.
#' @param groups Additional columns passed as strings to `dplyr::summarize()`
#'   via the `.by` argument for grouped summaries. Defaults to `NULL`.
#' @param calculate_ci If `NULL`, 95% confidence intervals will not be
#'   calculated for the performance estimates.  Otherwise, options of "wilson"
#'   or "clopper-pearson" can be supplied to utilize the corresponding methods
#'   to calculate the confidence intervals for the proportions. Defaults to
#'   `NULL`.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details This function filters and summarizes trauma records to calculate
#' SEQIC Indicators 1a through 1f:
#' \itemize{
#'   \item 1a: Proportion of Level 1 activations at Level I/II centers with
#'   surgical response ≤ 15 minutes.
#'   \item 1b: Same as 1a, but includes Level III centers and uses ≤ 30 minutes.
#'   \item 1c: Proportion of Level 1 activations with missing surgical response
#'   time.
#'   \item 1d/e: Response within 5 and 20 minutes, respectively, for specific
#'   provider types and activation levels.
#'   \item 1f: Proportion of missing response times among the group in 1d/e.
#' }
#'
#' @details This function:
#' \itemize{
#'   \item Filters trauma records to those with a trauma team activation level
#'   of "Level 1" and/or "Level 2" based on the indicator.
#'   \item Restricts provider type to surgical, physician, and mid-level
#'   provider roles.
#'   \item Filters trauma center levels to I–IV based on the measure.
#'   \item Calculates the proportion of cases where the response time is within
#'   5, 15, or 30 minutes, depending on the indicator.
#'   \item Computes proportions for trauma activation times, including missing
#'   times and within thresholds.
#' }
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @return A tibble summarizing SEQIC Indicator 1 results across sub-measures
#'   (1a–1f). Includes numerators, denominators, and performance rate for each
#'   indicator.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_1 <- function(
  df,
  trauma_team_activation_level,
  trauma_team_physician_service_type,
  level,
  unique_incident_id,
  response_time,
  trauma_team_activation_provider,
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

  # make the `trauma_team_activation_level` column accessible for validation
  trauma_team_activation_level_check <- df |>
    dplyr::pull({{ trauma_team_activation_level }})

  # validate `trauma_team_activation_level`
  if (
    !is.character(trauma_team_activation_level_check) &&
      !is.factor(trauma_team_activation_level_check)
  ) {
    cli::cli_abort(
      c(
        "{.var trauma_team_activation_level} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var trauma_team_activation_level} was an object of class {.cls {class(trauma_team_activation_level_check)}}."
      )
    )
  }

  # make the `trauma_team_physician_service_type` column accessible for validation
  trauma_team_physician_service_type_check <- df |>
    dplyr::pull({{ trauma_team_physician_service_type }})

  # validate `trauma_team_physician_service_type`
  if (
    !is.character(trauma_team_physician_service_type_check) &&
      !is.factor(trauma_team_physician_service_type_check)
  ) {
    cli::cli_abort(
      c(
        "{.var trauma_team_physician_service_type} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var trauma_team_physician_service_type} was an object of class {.cls {class(trauma_team_physician_service_type_check)}}."
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

  # make the `level` column accessible for validation
  level_check <- df |> dplyr::pull({{ level }})

  # validate `level`
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(
      c("{.var level} must be of class {.cls character} or {.cls factor}."),
      "i" = "{.var level} was an object of class {.cls {class(level_check)}}."
    )
  }

  # make the `response_time` column accessible for validation
  response_time_check <- df |> dplyr::pull({{ response_time }})

  # validate `response_time`
  if (!is.numeric(response_time_check)) {
    cli::cli_abort(
      c(
        "{.var response_time} must be of class {.cls numeric}.",
        "i" = "{.var response_time} was an object of class {.cls {class(response_time_check)}}."
      )
    )
  }

  # make the `trauma_team_activation_provider` column accessible for validation
  trauma_team_activation_provider_check <- df |>
    dplyr::pull({{ trauma_team_activation_provider }})

  # validate `trauma_team_activation_provider`
  if (
    !is.character(trauma_team_activation_provider_check) &&
      !is.factor(trauma_team_activation_provider_check)
  ) {
    cli::cli_abort(
      c(
        "{.var trauma_team_activation_provider} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var trauma_team_activation_provider} was an object of class {.cls {class(trauma_team_activation_provider_check)}}."
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

  # Indicator 1a – Proportion of Level 1 activations at Level I/II centers
  # where the first arriving Surgery/Trauma provider arrived within 15 minutes.
  seqic_1a <- df |>
    dplyr::filter(
      {{ trauma_team_activation_level }} == "Level 1",
      {{ trauma_team_physician_service_type }} == "Surgery/Trauma",
      {{ level }} %in% c("I", "II")
    ) |>
    dplyr::group_by({{ unique_incident_id }}) |>
    dplyr::slice_min({{ response_time }}, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::summarize(
      numerator_1a = sum({{ response_time }} <= 15, na.rm = TRUE),
      denominator_1a = sum(!is.na({{ response_time }})),
      seqic_1a = numerator_1a / denominator_1a,
      .by = {{ groups }}
    )

  # optionally calculate the confidence intervals for 1a
  if (!is.null(calculate_ci)) {
    seqic_1a <- seqic_1a |>
      nemsqar::nemsqa_binomial_confint(
        x = numerator_1a,
        n = denominator_1a,
        method = calculate_ci,
        ...
      ) |>
      dplyr::select(-prop, -prop_label) |>
      dplyr::rename(lower_ci_1a = lower_ci, upper_ci_1a = upper_ci)
  }

  # Indicator 1b – Same as 1a but for Level I/II/III centers and 30-minute
  # threshold.
  seqic_1b <- df |>
    dplyr::filter(
      {{ trauma_team_activation_level }} == "Level 1",
      {{ trauma_team_physician_service_type }} == "Surgery/Trauma",
      {{ level }} %in% c("I", "II", "III"),
      !is.na({{ response_time }})
    ) |>
    dplyr::group_by({{ unique_incident_id }}) |>
    dplyr::slice_min({{ response_time }}, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::summarize(
      numerator_1b = sum({{ response_time }} <= 30, na.rm = TRUE),
      denominator_1b = sum(!is.na({{ response_time }})),
      seqic_1b = numerator_1b / denominator_1b,
      .by = {{ groups }}
    )

  # optionally calculate the confidence intervals for 1b
  if (!is.null(calculate_ci)) {
    seqic_1b <- seqic_1b |>
      nemsqar::nemsqa_binomial_confint(
        x = numerator_1b,
        n = denominator_1b,
        method = calculate_ci,
        ...
      ) |>
      dplyr::select(-prop, -prop_label) |>
      dplyr::rename(lower_ci_1b = lower_ci, upper_ci_1b = upper_ci)
  }

  # Indicator 1c – Proportion of Level 1 activations where arrival time is
  # missing.
  seqic_1c <- df |>
    dplyr::filter(
      {{ trauma_team_activation_level }} == "Level 1",
      {{ trauma_team_physician_service_type }} == "Surgery/Trauma",
      {{ level }} %in% c("I", "II", "III")
    ) |>
    dplyr::distinct(
      {{ unique_incident_id }},
      {{ trauma_team_activation_provider }},
      .keep_all = TRUE
    ) |>
    dplyr::summarize(
      numerator_1c = sum(is.na({{ response_time }})),
      denominator_1c = dplyr::n(),
      seqic_1c = numerator_1c / denominator_1c,
      .by = {{ groups }}
    )

  # optionally calculate the confidence intervals for 1c
  if (!is.null(calculate_ci)) {
    seqic_1c <- seqic_1c |>
      nemsqar::nemsqa_binomial_confint(
        x = numerator_1c,
        n = denominator_1c,
        method = calculate_ci,
        ...
      ) |>
      dplyr::select(-prop, -prop_label) |>
      dplyr::rename(lower_ci_1c = lower_ci, upper_ci_1c = upper_ci)
  }

  # Combine 1a, 1b, and 1c results; assign label for state-level reporting.
  if (is.null(groups)) {
    seqic_1abc <- dplyr::bind_cols(
      seqic_1a,
      seqic_1b,
      seqic_1c
    ) |>
      tibble::add_column(Data = "Population/Sample", .before = "numerator_1a")
  } else {
    seqic_1abc <- seqic_1a |>
      dplyr::full_join(seqic_1b, by = dplyr::join_by(!!!rlang::syms(groups))) |>
      dplyr::full_join(seqic_1c, by = dplyr::join_by(!!!rlang::syms(groups)))
  }

  # Indicators 1d and 1e – Broader provider group, Level I-IV centers.
  # 1d: Arrival within 5 minutes; 1e: Arrival within 20 minutes.
  seqic_1de <- df |>
    dplyr::filter(
      {{ trauma_team_activation_level }} %in% c("Level 1", "Level 2"),
      {{ trauma_team_physician_service_type }} %in%
        c(
          "Surgery/Trauma",
          "Emergency Medicine",
          "Family Practice",
          "Nurse Practitioner",
          "Physician Assistant",
          "Surgery Senior Resident",
          "Hospitalist",
          "Internal Medicine"
        ),
      {{ level }} %in% c("I", "II", "III", "IV")
    ) |>
    dplyr::group_by({{ unique_incident_id }}) |>
    dplyr::slice_min({{ response_time }}, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::summarize(
      numerator_1d = sum({{ response_time }} <= 5, na.rm = TRUE),
      denominator_1d = sum(!is.na({{ response_time }})),
      seqic_1d = numerator_1d / denominator_1d,
      numerator_1e = sum({{ response_time }} <= 20, na.rm = TRUE),
      denominator_1e = sum(!is.na({{ response_time }})),
      seqic_1e = numerator_1e / denominator_1e,
      .by = {{ groups }}
    )

  # optionally calculate the confidence intervals for 1de
  if (!is.null(calculate_ci)) {
    seqic_1de <- seqic_1de |>
      nemsqar::nemsqa_binomial_confint(
        x = numerator_1d,
        n = denominator_1d,
        method = calculate_ci,
        ...
      ) |>
      dplyr::select(-prop, -prop_label) |>
      dplyr::rename(lower_ci_1d = lower_ci, upper_ci_1d = upper_ci) |>
      dplyr::relocate(lower_ci_1d, .after = seqic_1d) |>
      dplyr::relocate(upper_ci_1d, .after = lower_ci_1d) |>
      nemsqar::nemsqa_binomial_confint(
        x = numerator_1e,
        n = denominator_1e,
        method = calculate_ci,
        ...
      ) |>
      dplyr::select(-prop, -prop_label) |>
      dplyr::rename(lower_ci_1e = lower_ci, upper_ci_1e = upper_ci)
  }

  # Indicator 1f – Proportion of activations in 1d/e where arrival time is
  # missing.
  seqic_1f <- df |>
    dplyr::filter(
      {{ trauma_team_activation_level }} %in% c("Level 1", "Level 2"),
      {{ trauma_team_physician_service_type }} %in%
        c(
          "Surgery/Trauma",
          "Emergency Medicine",
          "Family Practice",
          "Nurse Practitioner",
          "Physician Assistant",
          "Surgery Senior Resident",
          "Hospitalist",
          "Internal Medicine"
        ),
      {{ level }} %in% c("I", "II", "III", "IV")
    ) |>
    dplyr::distinct(
      {{ unique_incident_id }},
      {{ trauma_team_activation_provider }},
      .keep_all = TRUE
    ) |>
    dplyr::summarize(
      numerator_1f = sum(is.na({{ response_time }})),
      denominator_1f = dplyr::n(),
      seqic_1f = numerator_1f / denominator_1f,
      .by = {{ groups }}
    )

  # optionally calculate the confidence intervals for 1f
  if (!is.null(calculate_ci)) {
    seqic_1f <- seqic_1f |>
      nemsqar::nemsqa_binomial_confint(
        x = numerator_1f,
        n = denominator_1f,
        method = calculate_ci,
        ...
      ) |>
      dplyr::select(-prop, -prop_label) |>
      dplyr::rename(lower_ci_1f = lower_ci, upper_ci_1f = upper_ci)
  }

  # Combine 1d, 1e, and 1f results; assign label for state-level reporting.
  if (is.null(groups)) {
    seqic_1def <- dplyr::bind_cols(seqic_1de, seqic_1f) |>
      tibble::add_column(
        Data = "Full Population/Sample",
        .before = "numerator_1d"
      )
  } else {
    seqic_1def <- seqic_1de |>
      dplyr::full_join(seqic_1f, by = dplyr::join_by(!!!rlang::syms(groups)))
  }

  # Final combination of all indicators into single summary.
  if (is.null(groups)) {
    seqic_1 <- dplyr::bind_cols(seqic_1abc, seqic_1def[, -1])
  } else {
    seqic_1 <- seqic_1abc |>
      dplyr::full_join(
        seqic_1def,
        by = dplyr::join_by(!!!rlang::syms(groups))
      ) |>
      dplyr::arrange(!!!rlang::syms(groups))
  }
  return(seqic_1)
}
