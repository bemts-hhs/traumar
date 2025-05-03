#' @title SEQIC Indicator 10 – Trauma Team Activation Appropriateness
#'
#' @description
#' Calculates two trauma system quality indicators related to trauma team
#' activation:
#' \itemize{
#'   \item 10a: Proportion of patients meeting triage criteria (based on Injury
#'   Severity Score or Need For Trauma Intervention) who received low-level
#'   or no activation (under-triage).
#'   \item 10b: Proportion of patients not meeting triage criteria who received
#'   highest-level trauma activation (over-triage).
#'   \item 10c: Proportion of patients meeting triage criteria (based on Injury
#'   Severity Score or Need For Trauma Intervention) who had a major trauma
#'   (under-triage via Peng & Xiang, 2019).
#' }
#'
#' Users may stratify results by one or more grouping variables and optionally
#' compute confidence intervals.
#'
#' @inheritParams seqic_indicator_1
#' @inheritParams seqic_indicator_6
#' @inheritParams seqic_indicator_9
#'
#' @param trauma_team_activation_level Column indicating the trauma team
#'   activation level (e.g., `"Level 1"`, `"Level 2"`, `"Level 3"`,
#'   `"Consultation"`). Must be character or factor.
#' @param iss Optional numeric column representing the Injury Severity Score.
#' @param nfti Optional column indicating Need For Trauma Intervention
#'   classification of positive or negative. Should be character, factor, or
#'   logical.
#' @param groups Optional character vector of column names used for grouping
#'   results.
#' @param calculate_ci Optional; if not `NULL`, must be `"wilson"` or
#'   `"clopper-pearson"` to compute confidence intervals.
#'
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details
#' This function:
#' \itemize{
#'   \item Restricts analysis to Level I–IV trauma centers.
#'   \item Removes duplicate incidents using `unique_incident_id`.
#'   \item Classifies each record as meeting or not meeting triage criteria
#'   based on ISS or NFTI logic.
#'   \item Calculates two quality indicators:
#'     \itemize{
#'       \item 10a: Under-triage — patients who met criteria but received low
#'       activation.
#'       \item 10b: Over-triage — patients who did not meet criteria but
#'       received highest activation.
#'     }
#'   \item Optionally computes binomial confidence intervals for each indicator.
#' }
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @returns A list of tibbles with counts and proportions for SEQIC Indicators
#'   10a, 10b, and 10c, along with model diagnostics for the Cribari or NFTI
#'   ouputs.  The proportions in 10a, 10b, and 10c will optionally include
#'   confidence intervals grouped by user-specified variables.
#'
#' @references
#'
#' Beam G, Gorman K, Nannapaneni S, Zipf J, Simunich T, et al. (2022) Need for
#' Trauma Intervention and Improving Under-Triaging in Geriatric Trauma
#' Patients: Under-Triaged or Misclassified. Int J Crit Care Emerg Med 8:136.
#' doi.org/10.23937/2474-3674/1510136
#'
#' Peng J, Xiang H. Trauma undertriage and overtriage rates: are we using the
#' wrong formulas? Am J Emerg Med. 2016 Nov;34(11):2191-2192. doi:
#' 10.1016/j.ajem.2016.08.061. Epub 2016 Aug 31. PMID: 27615156; PMCID:
#' PMC6469681.
#'
#' Roden-Foreman JW, Rapier NR, Yelverton L, Foreman ML. Asking a Better
#' Question: Development and Evaluation of the Need For Trauma Intervention
#' (NFTI) Metric as a Novel Indicator of Major Trauma. J Trauma Nurs. 2017
#' May/Jun;24(3):150-157. doi: 10.1097/JTN.0000000000000283. PMID: 28486318.
#'
#' @export
#'
seqic_indicator_10 <- function(
  df,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  trauma_team_activation_level,
  iss,
  nfti,
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

  # Validate the `level` column
  level_check <- df |> dplyr::pull({{ level }})
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(c(
      "{.var level} must be character or factor.",
      "i" = "Provided class: {.cls {class(level_check)}}."
    ))
  }

  # Validate the `unique_incident_id` column
  incident_id_check <- df |> dplyr::pull({{ unique_incident_id }})
  if (!is.character(incident_id_check) && !is.factor(incident_id_check)) {
    cli::cli_abort(c(
      "{.var unique_incident_id} must be character or factor.",
      "i" = "Provided class: {.cls {class(incident_id_check)}}."
    ))
  }

  # Validate that `trauma_team_activation_level` is character, factor, or logical.
  trauma_team_activation_level_check <- df |>
    dplyr::pull({{ trauma_team_activation_level }})
  if (
    !is.character(trauma_team_activation_level_check) &&
      !is.factor(trauma_team_activation_level_check)
  ) {
    cli::cli_abort(c(
      "{.var trauma_team_activation_level} must be character or factor.",
      "i" = "Provided class: {.cls {class(trauma_team_activation_level_check)}}."
    ))
  }

  # Validate that `iss` is numeric.
  if (!rlang::is_empty(iss)) {
    iss_check <- df |> dplyr::pull({{ iss }})
    if (!is.numeric(iss_check)) {
      cli::cli_abort(c(
        "{.var iss} must be numeric when provided.",
        "i" = "Provided class: {.cls {class(iss_check)}}."
      ))
    }
  }

  # Validate that `nfti` is character, factor, or logical.
  if (!is.null(nfti)) {
    nfti_check <- df |> dplyr::pull({{ nfti }})
    if (
      !is.character(nfti_check) &&
        !is.factor(nfti_check) &&
        !is.logical(nfti_check)
    ) {
      cli::cli_abort(c(
        "{.var nfti} must be character, factor, or logical when provided.",
        "i" = "Provided class: {.cls {class(nfti_check)}}."
      ))
    }
  }

  # Validate `groups` argument
  if (!is.null(groups)) {
    if (!all(sapply(groups, is.character))) {
      cli::cli_abort(c(
        "All elements in {.var groups} must be character strings.",
        "i" = "Provided object: {.cls {class(groups)}}."
      ))
    }
    if (!all(groups %in% names(df))) {
      invalid_vars <- groups[!groups %in% names(df)]
      cli::cli_abort(
        "Invalid grouping variable(s): {paste(invalid_vars, collapse = ', ')}"
      )
    }
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

  ###___________________________________________________________________________
  ### Data preparation
  ###___________________________________________________________________________

  # Preprocess the input dataset:
  # - Remove duplicate incidents to avoid double-counting (based on unique
  # incident ID)
  # - Restrict analysis to Level I-IV trauma centers
  # - Exclude transfers out (e.g., for whom definitive triage data may be
  # incomplete)

  df_prep <- df |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::filter(
      {{ level }} %in% included_levels
    ) |>
    dplyr::mutate(
      # Define cases with highest-level activation (Level 1)
      full_activation = grepl(
        pattern = "level 1",
        x = {{ trauma_team_activation_level }},
        ignore.case = TRUE
      ),

      # Define low activation:
      # - Activation not recorded; assume no activation called
      # - Any level other than "Level 1"
      limited_no_activation = is.na({{ trauma_team_activation_level }}) |
        !grepl(
          pattern = "level 1",
          x = {{ trauma_team_activation_level }},
          ignore.case = TRUE
        )
    )

  # Dynamically classify patients using either ISS or NFTI logic
  if (!rlang::is_empty(iss) && rlang::is_empty(nfti)) {
    df_prep <- df_prep |>
      dplyr::mutate(
        # Patients who should have had activation based on Cribari ISS > 15
        major_trauma = {{ iss }} > 15,
        # Patients who clearly did not require activation (ISS < 9)
        minor_trauma = {{ iss }} <= 15,
        # Over triage = full activation and minor truama
        overtriage = full_activation & minor_trauma,
        # Under triage = limited-to-no activation and major trauma
        undertriage = limited_no_activation & major_trauma
      )
  } else if (rlang::is_empty(iss) && !rlang::is_empty(nfti)) {
    df_prep <- df_prep |>
      dplyr::mutate(
        # Patients flagged by NFTI as needing activation
        major_trauma = {{ nfti }} %in% c("Positive", TRUE, "Yes"),
        # Patients flagged by NFTI as not needing activation
        minor_trauma = {{ nfti }} %in% c("Negative", FALSE, "No"),
        overtriage = full_activation & minor_trauma,
        undertriage = limited_no_activation & major_trauma
      )
  }

  # Get an identifier of how the triage classification was performed
  # Determine triage logic source as a scalar
  triage_logic_source <- if (!rlang::is_empty(nfti) && rlang::is_empty(iss)) {
    "NFTI"
  } else if (rlang::is_empty(nfti) && !rlang::is_empty(iss)) {
    "Cribari"
  }

  ###___________________________________________________________________________
  ### Calculations
  ###___________________________________________________________________________

  # Initiate the list for output
  seqic_10 <- list()

  # --- Measure 10a: Under-triage ---
  # Patients who met triage criteria (positive) but received low activation
  # Denominator: all limited-to-no trauma team activation cases
  # Numerator: major_trauma AND limited_no_activation
  seqic_10a <- df_prep |>
    dplyr::summarize(
      numerator_10a = sum(
        undertriage,
        na.rm = TRUE
      ),

      # Patients who had a limited or no trauma team activation
      denominator_10a = sum(limited_no_activation, na.rm = TRUE),
      seqic_10a = dplyr::if_else(
        denominator_10a > 0,
        numerator_10a / denominator_10a,
        NA_real_ # Return NA if no denominator
      ),
      .by = {{ groups }}
    )

  # --- Measure 10b: Over-triage ---
  # Patients who did NOT meet triage criteria (negative) but received highest
  # activation
  # Denominator: all full trauma team activations
  # Numerator: minor_trauma AND full_activation
  seqic_10b <- df_prep |>
    dplyr::summarize(
      numerator_10b = sum(
        overtriage,
        na.rm = TRUE
      ),
      denominator_10b = sum(full_activation, na.rm = TRUE),
      seqic_10b = dplyr::if_else(
        denominator_10b > 0,
        numerator_10b / denominator_10b,
        NA_real_
      ),
      .by = {{ groups }}
    )

  # --- Measure 10c: Under-triage ---
  # Patients who met triage criteria (positive) but received low activation
  # Denominator: all major trauma cases
  # Numerator: major_trauma AND limited_no_activation
  # This is Peng & Xiang's update to the Cribari method of calculating
  # under triage
  seqic_10c <- df_prep |>
    dplyr::summarize(
      numerator_10c = sum(
        undertriage,
        na.rm = TRUE
      ),

      # All major trauma patients as denominator
      denominator_10c = sum(major_trauma, na.rm = TRUE),
      seqic_10c = dplyr::if_else(
        denominator_10c > 0,
        numerator_10c / denominator_10c,
        NA_real_ # Return NA if no denominator
      ),
      .by = {{ groups }}
    )

  # --- Model Diagnostic Testing ---
  # Cribari 2x2 matrix to produce model diagnostic tests
  # Based on methods in Peng & Xiang (2016)
  diagnostics <- df_prep |>
    dplyr::summarize(
      a = sum(full_activation & minor_trauma, na.rm = TRUE), # False Positive
      b = sum(full_activation & major_trauma, na.rm = TRUE), # True Positive
      c = sum(limited_no_activation & minor_trauma, na.rm = TRUE), # True Negative
      d = sum(limited_no_activation & major_trauma, na.rm = TRUE), # False Negative
      .by = {{ groups }}
    ) |>
    dplyr::mutate(
      triage_logic = triage_logic_source,
      N = a + b + c + d,
      sensitivity = dplyr::if_else((b + d) > 0, b / (b + d), NA_real_),
      specificity = dplyr::if_else((a + c) > 0, c / (a + c), NA_real_),
      positive_predictive_value = dplyr::if_else(
        (a + b) > 0,
        b / (a + b),
        NA_real_
      ),
      negative_predictive_value = dplyr::if_else(
        (c + d) > 0,
        c / (c + d),
        NA_real_
      ),
      false_negative_rate = dplyr::if_else((b + d) > 0, d / (b + d), NA_real_), # 1 - sensitivity
      false_positive_rate = dplyr::if_else((a + c) > 0, a / (a + c), NA_real_), # 1 - specificity
      false_discovery_rate = dplyr::if_else((a + b) > 0, a / (a + b), NA_real_), # 1 - positive predictive value
      false_omission_rate = dplyr::if_else((c + d) > 0, d / (c + d), NA_real_) # 1 - negative predictive value
    )

  # Optionally compute confidence intervals
  if (!is.null(calculate_ci)) {
    # Apply binomial confidence interval function

    # 10a CIs
    seqic_10a <- seqic_10a |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_10a,
          x = numerator_10a,
          n = denominator_10a,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_10a = lower_ci, upper_ci_10a = upper_ci)
      )

    # 10b CIs
    seqic_10b <- seqic_10b |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_10b,
          x = numerator_10b,
          n = denominator_10b,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_10b = lower_ci, upper_ci_10b = upper_ci)
      )

    # 10c CIs
    seqic_10c <- seqic_10c |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_10c,
          x = numerator_10c,
          n = denominator_10c,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_10c = lower_ci, upper_ci_10c = upper_ci)
      )
  }

  # Add label if ungrouped
  if (is.null(groups)) {
    seqic_10$seqic_10 <- seqic_10a |>
      tibble::add_column(
        Data = "Population/Sample",
        Triage_Logic = triage_logic_source,
        .before = "numerator_10a"
      ) |>
      dplyr::bind_cols(seqic_10b, seqic_10c)

    seqic_10$diagnostics <- diagnostics
  } else {
    # Arrange by grouping variables
    seqic_10$seqic_10 <- seqic_10a |>
      dplyr::full_join(
        seqic_10b,
        by = dplyr::join_by(!!!rlang::syms(groups))
      ) |>
      dplyr::full_join(
        seqic_10c,
        by = dplyr::join_by(!!!rlang::syms(groups))
      ) |>
      dplyr::arrange(!!!rlang::syms(groups))

    seqic_10$diagnostics <- diagnostics |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  # Return both measures as a tibble
  return(seqic_10)
}
