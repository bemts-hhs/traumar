#' @title SEQIC Indicator 11 – Overtriage for Minor Trauma Patients
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates SEQIC Indicator 11, which estimates the proportion of minor trauma
#' patients who were transferred into a trauma center and remained in the
#' Emergency Department for less than 24 hours. This indicator is designed to
#' identify potential overtriage events within the trauma system. Minor trauma
#' patients are identified using the Injury Severity Score (ISS < 9). Patients
#' must not have been transferred out and must have been received at a trauma
#' center level included in `included_levels`.
#'
#' @inheritParams seqic_indicator_1
#' @inheritParams seqic_indicator_4
#' @inheritParams seqic_indicator_6
#' @inheritParams seqic_indicator_10
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @returns A tibble summarizing the numerator, denominator, and proportion of
#'   overtriaged patients (Indicator 11), with optional 95% confidence
#'   intervals.
#'
#' @details
#'
#' This function:
#' \itemize{
#'   \item Filters the dataset to include only patients treated at trauma
#'   centers designated Level I through IV.
#'   \item Excludes patients transferred out and retains only those received by
#'   the trauma center.
#'   \item Deduplicates incident-level records using `unique_incident_id`.
#'   \item Classifies patients as low-risk based on the  Injury Severity Score
#'   (ISS < 9).
#'   \item Flags low-risk patients who were discharged from the ED in under 24
#'   hours.
#'   \item Stratifies results by one or more user-defined grouping variables.
#'   \item Returns a summarized tibble with the number of eligible low-risk
#'   short-stay discharges (numerator), all received patients meeting inclusion
#'   criteria (denominator), and the resulting proportion.
#'   \item Optionally includes 95% confidence intervals if `calculate_ci` is
#'   specified.
#' }
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Simulated data for SEQIC Indicator 11
#' test_data <- tibble::tibble(
#'   id = as.character(1:10),
#'   trauma_level = c("I", "II", "III", "IV", "II", "I", "IV", "III", "II",
#'   "I"),
#'   transferred_out = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
#'   FALSE, FALSE),
#'   received = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#'   iss = c(4, 8, 10, 6, 5, 7, 6, 15, 3, 2),
#'   ed_LOS = c(6, 20, 30, 18, 8, 5, 22, 40, 2, 4),
#'   region = rep(c("East", "West"), each = 5)
#' )
#'
#' # Run the function
#' traumar::seqic_indicator_11(
#'   data = test_data,
#'   level = trauma_level,
#'   included_levels = c("I", "II", "III", "IV"),
#'   transfer_out_indicator = transferred_out,
#'   receiving_indicator = received,
#'   unique_incident_id = id,
#'   iss = iss,
#'   ed_LOS = ed_LOS,
#'   groups = "region",
#'   calculate_ci = "clopper-pearson"
#' )
#'
#' @references
#'
#' Roden-Foreman JW, Rapier NR, Yelverton L, Foreman ML. Asking a Better
#' Question: Development and Evaluation of the Need For Trauma Intervention
#' (NFTI) Metric as a Novel Indicator of Major Trauma. J Trauma Nurs. 2017
#' May/Jun;24(3):150-157. doi: 10.1097/JTN.0000000000000283. PMID: 28486318.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export

seqic_indicator_11 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  transfer_out_indicator,
  receiving_indicator,
  unique_incident_id,
  iss,
  ed_LOS,
  groups = NULL,
  calculate_ci = NULL,
  ...
) {
  ###___________________________________________________________________________
  ### Data validation ----
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

  # Ensure that `iss` can be validated ----
  iss_check <- validate_data_pull(
    input = data,
    col = {{ iss }},
    type = "error",
    var_name = "iss"
  )

  # Validation of `iss` ----
  validate_numeric(
    input = iss_check,
    min = 0,
    max = 75,
    type = "error",
    var_name = "iss"
  )

  # Ensure that `transfer_out_indicator` can be validated  ----
  transfer_out_indicator_check <- validate_data_pull(
    input = data,
    col = {{ transfer_out_indicator }},
    type = "error",
    var_name = "transfer_out_indicator"
  )

  # Validate `transfer_out_indicator` ----
  validate_class(
    input = transfer_out_indicator_check,
    class_type = c("logical", "character", "factor"),
    logic = "or",
    type = "error",
    var_name = "transfer_out_indicator"
  )

  # Ensure that `receiving_indicator` can be validated
  receiving_indicator_check <- validate_data_pull(
    input = data,
    col = {{ receiving_indicator }},
    type = "error",
    var_name = "receiving_indicator"
  )

  # Validate that `receiving_indicator` is character, factor, or logical. ----
  validate_class(
    input = receiving_indicator_check,
    class_type = c("logical", "character", "factor"),
    logic = "or",
    type = "error",
    var_name = "receiving_indicator"
  )

  # Ensure that ed_LOS can be validated ----
  ed_los_check <- validate_data_pull(
    input = data,
    col = {{ ed_LOS }},
    type = "error",
    var_name = "ed_LOS"
  )

  # Validate `ed_LOS` ----
  validate_numeric(
    input = ed_los_check,
    min = NULL,
    max = NULL,
    type = "error",
    var_name = "ed_LOS"
  )

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

  ###___________________________________________________________________________
  ### Data preparation ----
  ###___________________________________________________________________________

  # Dynamically classify patients using ISS logic ----
  data_prep <- data |>
    dplyr::filter(
      {{ level }} %in% included_levels,
      {{ transfer_out_indicator }} %in% c("No", FALSE),
      {{ receiving_indicator }} %in% c("Yes", TRUE)
    ) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::mutate(
      # Patients who clearly did not require activation (ISS < 9)
      minor_trauma = {{ iss }} < 9,
      los_less_24_hrs = {{ ed_LOS }} < 1440
    )

  ###___________________________________________________________________________
  ### Calculations ----
  ###___________________________________________________________________________

  # state 11 ----
  seqic_11 <- data_prep |>
    dplyr::summarize(
      numerator_11 = sum(
        minor_trauma &
          los_less_24_hrs,
        na.rm = TRUE
      ),
      denominator_11 = dplyr::n(),
      seqic_11 = dplyr::if_else(
        denominator_11 > 0,
        numerator_11 / denominator_11,
        NA_real_
      ),
      .by = {{ groups }}
    )

  # Optionally calculate confidence intervals for the proportions: ----
  # - If `calculate_ci` is provided, apply a binomial confidence interval method (Wilson or Clopper-Pearson).
  # - `nemsqa_binomial_confint()` calculates the confidence intervals for the proportion.
  # - Select only the relevant columns and rename the CI columns for clarity.
  if (!is.null(calculate_ci)) {
    seqic_11 <- seqic_11 |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_11,
          x = numerator_11, # Number of successes (numerator).
          n = denominator_11, # Number of trials (denominator).
          method = calculate_ci, # Confidence interval calculation method (e.g., "wilson").
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_11 = lower_ci, upper_ci_11 = upper_ci) # Rename CI columns.
      )
  }

  # Assign a label ----
  # to indicate whether the data represents population or sample-level results:
  # - If no grouping is applied, label the data as "Population/Sample".
  if (is.null(groups)) {
    seqic_11 <- seqic_11 |>
      tibble::add_column(data = "population/sample", .before = "numerator_11") # Add the label column.
  } else if (!is.null(groups)) {
    seqic_11 <- seqic_11 |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  return(seqic_11)
}
