#' @title SEQIC Indicator 10 – Trauma Team Activation Appropriateness
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates three trauma system quality indicators related to trauma team
#' activations where the patient was kept at the facility:
#' \itemize{
#'   \item 10a: Proportion of patients meeting triage criteria (based on Injury
#'   Severity Score or Need For Trauma Intervention) who received low-level
#'   or no activation (undertriage).
#'   \item 10b: Proportion of patients not meeting triage criteria who received
#'   highest-level trauma activation (overtriage).
#'   \item 10c: Proportion of major trauma patients receiving a full activation
#'   (undertriage via Peng & Xiang, 2019).
#'
#'   (10a, 10b, 10c can be based on Injury Severity Score or Need For Trauma
#'   Intervention based on user choice)
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
#'   \item Optionally computes 95% confidence intervals for each indicator.
#' }
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @returns A list of two tibbles with counts and proportions for SEQIC
#'   Indicators 10a, 10b, and 10c, along with model diagnostics for the Cribari
#'   or NFTI ouputs.  The proportions in 10a, 10b, and 10c will optionally
#'   include 95% confidence intervals.
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Simulated data for SEQIC Indicator 10
#' test_data <- tibble::tibble(
#'   id = as.character(1:12),
#'   trauma_level = c("I", "II", "III", "IV", "II", "I", "IV", "III", "II", "I",
#'   "III", "IV"),
#'   activation = c("Level 1", "Level 2", "None", "Consultation", "Level 1",
#'   "Level 1", "None", "Level 3", "Level 1", "Consultation", "None", "Level
#'   2"),
#'   acute_transfer = rep("No", 12),
#'   iss = c(25, 10, 16, 8, 30, 45, 12, 9, 28, 6, 17, 14),
#'   nfti = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE,
#'   TRUE, TRUE),
#'   region = rep(c("East", "West"), each = 6)
#' )
#'
#' # Run the function, this will succeed
#' traumar::seqic_indicator_10(
#'   data = test_data,
#'   level = trauma_level,
#'   included_levels = c("I", "II", "III", "IV"),
#'   unique_incident_id = id,
#'   transfer_out_indicator = acute_transfer,
#'   trauma_team_activation_level = activation,
#'   iss = iss,
#'   nfti = NULL,
#'   groups = "region",
#'   calculate_ci = "wilson"
#' )
#'
#' # Run the function, this will fail
#' try(
#'   traumar::seqic_indicator_10(
#'   data = test_data,
#'   level = trauma_level,
#'   included_levels = c("I", "II", "III", "IV"),
#'   unique_incident_id = id,
#'   transfer_out_indicator = acute_transfer,
#'   trauma_team_activation_level = activation,
#'   iss = iss,
#'   nfti = nfti,
#'   groups = "region",
#'   calculate_ci = "wilson"
#' ))
#'
#' @references
#'
#' Beam G, Gorman K, Nannapaneni S, Zipf J, Simunich T, et al. (2022) Need for
#' Trauma Intervention and Improving Under-Triaging in Geriatric Trauma
#' Patients: undertriaged or Misclassified. Int J Crit Care Emerg Med 8:136.
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
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_10 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  transfer_out_indicator,
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
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort(c(
      "{.var data} must be a data frame or tibble.",
      "i" = "You provided an object of class {.cls {class(data)}}."
    ))
  }

  # Validate the `level` column
  level_check <- tryCatch(
    {
      data |> dplyr::pull({{ level }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var level}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_10())
      )
    }
  )

  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(c(
      "{.var level} must be character or factor.",
      "i" = "Provided class: {.cls {class(level_check)}}."
    ))
  }

  # Make the `unique_incident_id` column accessible for validation.
  unique_incident_id_check <- tryCatch(
    {
      data |> dplyr::pull({{ unique_incident_id }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var unique_incident_id}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_10())
      )
    }
  )

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

  # Validate that `transfer_out_indicator` is character, factor, or logical.
  transfer_out_indicator_check <- tryCatch(
    {
      data |> dplyr::pull({{ transfer_out_indicator }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var transfer_out_indicator}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_10())
      )
    }
  )

  if (
    !is.character(transfer_out_indicator_check) &&
      !is.factor(transfer_out_indicator_check) &&
      !is.logical(transfer_out_indicator_check)
  ) {
    cli::cli_abort(
      c(
        "{.var transfer_out_indicator} must be of class {.cls character}, {.cls factor}, or {.cls logical}.",
        "i" = "{.var transfer_out_indicator} was an object of class {.cls {class(transfer_out_indicator_check)}}."
      )
    )
  }

  # Validate that `trauma_team_activation_level` is character, factor, or logical.
  trauma_team_activation_level_check <- tryCatch(
    {
      data |> dplyr::pull({{ trauma_team_activation_level }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var trauma_team_activation_level}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_10())
      )
    }
  )

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
  if (!rlang::quo_is_null(rlang::enquo(iss))) {
    iss_check <- tryCatch(
      {
        data |> dplyr::pull({{ iss }})
      },
      error = function(e) {
        cli::cli_abort(
          "It was not possible to validate {.var iss}, please check this column in the function call.",
          call = rlang::expr(seqic_indicator_10())
        )
      }
    )

    if (!is.numeric(iss_check)) {
      cli::cli_abort(c(
        "{.var iss} must be numeric when provided.",
        "i" = "Provided class: {.cls {class(iss_check)}}."
      ))
    }
  }

  # Validate that `nfti` is character, factor, or logical.
  if (!rlang::quo_is_null(rlang::enquo(nfti))) {
    nfti_check <- tryCatch(
      {
        data |> dplyr::pull({{ nfti }})
      },
      error = function(e) {
        cli::cli_abort(
          "It was not possible to validate {.var nfti}, please check this column in the function call.",
          call = rlang::expr(seqic_indicator_10())
        )
      }
    )

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

  # Check if all elements in groups are strings (i.e., character vectors)
  if (!is.null(groups)) {
    if (!is.character(groups)) {
      cli::cli_abort(c(
        "All elements in {.var groups} must be strings.",
        "i" = "You passed an object of class {.cls {class(groups)}} to {.var groups}."
      ))
    }
  }

  # Check if all groups exist in the `data`
  if (!all(groups %in% names(data))) {
    invalid_vars <- groups[!groups %in% names(data)]
    cli::cli_abort(
      "Invalid grouping variable(s): {paste(invalid_vars, collapse = ', ')}"
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
  ###___________________________________________________________________________
  ### Data preparation
  ###___________________________________________________________________________

  # Preprocess the input dataset:
  # - Remove duplicate incidents to avoid double-counting (based on unique
  # incident ID)
  # - Restrict analysis to Level I-IV trauma centers
  # - Exclude transfers out (e.g., for whom definitive triage data may be
  # incomplete)

  data_prep <- data |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::filter(
      {{ level }} %in% included_levels,
      {{ transfer_out_indicator }} %in% c("No", FALSE)
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
  if (
    !rlang::quo_is_null(rlang::enquo(iss)) &&
      rlang::quo_is_null(rlang::enquo(nfti))
  ) {
    data_prep <- data_prep |>
      dplyr::mutate(
        # Patients who should have had activation based on Cribari ISS > 15
        major_trauma = {{ iss }} > 15,
        # Patients who clearly did not require activation (ISS < 9)
        minor_trauma = {{ iss }} < 9,
        # Over triage = full activation and minor trauma
        overtriage = full_activation & minor_trauma,
        # Under triage = limited-to-no activation and major trauma
        undertriage = limited_no_activation & major_trauma
      )
  } else if (
    rlang::quo_is_null(rlang::enquo(iss)) &&
      !rlang::quo_is_null(rlang::enquo(nfti))
  ) {
    data_prep <- data_prep |>
      dplyr::mutate(
        # Patients flagged by NFTI as needing activation
        major_trauma = {{ nfti }} %in% c("Positive", TRUE, "Yes"),
        # Patients flagged by NFTI as NOT needing activation
        minor_trauma = {{ nfti }} %in% c("Negative", FALSE, "No"),
        # Over triage = full activation and minor trauma
        overtriage = full_activation & minor_trauma,
        # Under triage = limited-to-no activation and major trauma
        undertriage = limited_no_activation & major_trauma
      )
  } else {
    # Fail clearly if both or neither triage criteria are supplied
    cli::cli_abort(
      "Please supply exactly one of {.var iss} or {.var nfti}."
    )
  }

  # Get an identifier of how the triage classification was performed
  # Determine triage logic source as a scalar
  triage_logic_source <- if (
    !rlang::quo_is_null(rlang::enquo(nfti)) &&
      rlang::quo_is_null(rlang::enquo(iss))
  ) {
    "nfti"
  } else if (
    rlang::quo_is_null(rlang::enquo(nfti)) &&
      !rlang::quo_is_null(rlang::enquo(iss))
  ) {
    "cribari"
  } else if (
    !rlang::quo_is_null(rlang::enquo(nfti)) &&
      !rlang::quo_is_null(rlang::enquo(iss))
  ) {
    cli::cli_abort(
      "Please supply exactly one of {.var iss} or {.var nfti}."
    )
  }

  ###___________________________________________________________________________
  ### Calculations
  ###___________________________________________________________________________

  # Initiate the list for output
  seqic_10 <- list()

  # --- Measure 10a: undertriage ---
  # Patients who met triage criteria (positive) but received low activation
  # Denominator: all limited-to-no trauma team activation cases
  # Numerator: major_trauma AND limited_no_activation
  seqic_10a <- data_prep |>
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

  # --- Measure 10b: overtriage ---
  # Patients who did NOT meet triage criteria (negative) but received highest
  # activation
  # Denominator: all full trauma team activations
  # Numerator: minor_trauma AND full_activation
  seqic_10b <- data_prep |>
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

  # --- Measure 10c: undertriage ---
  # Patients who met triage criteria (positive) but received low activation
  # Denominator: all major trauma cases
  # Numerator: major_trauma AND limited_no_activation
  # This is Peng & Xiang's (2016) update to the Cribari method of calculating
  # under triage
  seqic_10c <- data_prep |>
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

  # The following is from Table 1 in Peng & Xiang (2016)
  # The Cribari matrix: Injury severity and trauma team activation.
  #
  #                            | Minor Trauma | Major Trauma |   Total
  # ---------------------------|--------------|--------------|---------
  # Full Trauma Team Activation|       a      |      b       |   a + b
  # Limited/No Activation      |       c      |      d       |   c + d
  # ---------------------------|--------------|--------------|---------
  # Total                      |     a + c    |    b + d     |     N
  #
  # Common statistical terms used in diagnostic testing:
  #
  # Sensitivity              = b / (b + d)
  # Specificity              = c / (a + c)
  #
  # False Negative Rate (FNR) = d / (b + d)      # 1 - Sensitivity
  # False Positive Rate (FPR) = a / (a + c)      # 1 - Specificity
  #
  # Positive Predictive Value (PPV) = b / (a + b)
  # Negative Predictive Value (NPV) = c / (c + d)
  #
  # False Discovery Rate (FDR) = a / (a + b)     # 1 - PPV
  # False Omission Rate (FOR)  = d / (c + d)     # 1 - NPV

  diagnostics <- data_prep |>
    dplyr::summarize(
      # Calculate the key confusion matrix values
      full_minor = sum(full_activation & minor_trauma, na.rm = TRUE), # False Positive
      full_major = sum(full_activation & major_trauma, na.rm = TRUE), # True Positive
      limited_minor = sum(limited_no_activation & minor_trauma, na.rm = TRUE), # True Negative
      limited_major = sum(limited_no_activation & major_trauma, na.rm = TRUE), # False Negative
      .by = {{ groups }}
    ) |>
    dplyr::mutate(
      # Total number of classified records
      # N here is total records not missing classification information
      N = full_minor + full_major + limited_minor + limited_major,

      # Sensitivity = b / (b + d)
      sensitivity = dplyr::if_else(
        (full_major + limited_major) > 0,
        full_major / (full_major + limited_major),
        NA_real_
      ),

      # Specificity = c / (a + c)
      specificity = dplyr::if_else(
        (full_minor + limited_minor) > 0,
        limited_minor / (full_minor + limited_minor),
        NA_real_
      ),

      # Positive Predictive Value (PPV) = b / (a + b)
      positive_predictive_value = dplyr::if_else(
        (full_minor + full_major) > 0,
        full_major / (full_minor + full_major),
        NA_real_
      ),

      # Negative Predictive Value (NPV) = c / (c + d)
      negative_predictive_value = dplyr::if_else(
        (limited_minor + limited_major) > 0,
        limited_minor / (limited_minor + limited_major),
        NA_real_
      ),

      # False Negative Rate (FNR) = d / (b + d); 1 - Sensitivity
      false_negative_rate = dplyr::if_else(
        (full_major + limited_major) > 0,
        limited_major / (full_major + limited_major),
        NA_real_
      ),

      # False Positive Rate (FPR) = a / (a + c); 1 - Specificity
      false_positive_rate = dplyr::if_else(
        (full_minor + limited_minor) > 0,
        full_minor / (full_minor + limited_minor),
        NA_real_
      ),

      # False Discovery Rate (FDR) = a / (a + b); 1 - Positive Predictive Value
      false_discovery_rate = dplyr::if_else(
        (full_minor + full_major) > 0,
        full_minor / (full_minor + full_major),
        NA_real_
      ),

      # False Omission Rate (FOR) = d / (c + d); 1 - Negative Predictive Value
      false_omission_rate = dplyr::if_else(
        (limited_minor + limited_major) > 0,
        limited_major / (limited_minor + limited_major),
        NA_real_
      )
    )

  # Optionally compute confidence intervals
  if (!is.null(calculate_ci)) {
    # Apply 95% confidence interval function

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
    seqic_10$seqic_10 <-
      tibble::tibble(
        data = "population/sample",
        triage_logic = triage_logic_source
      ) |>
      dplyr::bind_cols(seqic_10a, seqic_10b, seqic_10c)

    seqic_10$diagnostics <- tibble::tibble(
      data = "population/sample",
      triage_logic = triage_logic_source
    ) |>
      dplyr::bind_cols(diagnostics)
  } else {
    # Arrange by grouping variables
    seqic_10$seqic_10 <- tibble::tibble(
      triage_logic = triage_logic_source
    ) |>
      dplyr::bind_cols(seqic_10a) |>
      dplyr::full_join(
        seqic_10b,
        by = dplyr::join_by(!!!rlang::syms(groups))
      ) |>
      dplyr::full_join(
        seqic_10c,
        by = dplyr::join_by(!!!rlang::syms(groups))
      ) |>
      dplyr::arrange(!!!rlang::syms(groups))

    seqic_10$diagnostics <- tibble::tibble(
      triage_logic = triage_logic_source
    ) |>
      dplyr::bind_cols(diagnostics) |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  # Return both measures as a tibble
  return(seqic_10)
}
