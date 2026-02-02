#' @title SEQIC Indicator 8 - Survival by Risk Group
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates the proportion of patients who survived based on risk groups
#' existing in the data among trauma patients transported to Level I–IV trauma
#' centers.
#'
#' @inheritParams seqic_indicator_1
#' @param mortality_indicator A logical, character, or factor variable
#'   indicating whether the patient died at the trauma center. Accepts values
#'   like `TRUE`/`FALSE` or `"Yes"`/`"No"`.
#' @param risk_group A character or factor column indicating the patient's risk
#'   group (e.g., "High", "Moderate", "Low"). See risk definitions below.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details
#'
#' \itemize{
#'   \item Filters the dataset to include only trauma center levels I through
#'   IV.
#'   \item Deduplicates the dataset using `unique_incident_id` to ensure one
#'   record per incident.
#'   \item Accepts a mortality indicator that may be logical, character, or
#'   factor, and identifies survivors as those with values of `FALSE` or `"No"`.
#'   \item Requires a predefined `risk_group` variable representing categories
#'   such as "Low", "Moderate", or "High" risk.
#'   \item Calculates overall survival proportions and survival proportions
#'   stratified by risk group.
#'   \item Optionally includes 95% confidence intervals using binomial methods
#'   if `calculate_ci` is specified.
#' }
#'
#' @note
#'
#' This function calculates survival outcomes for patients transported
#' to trauma centers, stratified by risk of mortality. Risk groups—low,
#' moderate, and high— are defined by the Iowa System Evaluation and Quality
#' Improvement Committee (SEQIC) as described below. Users may also apply
#' alternative risk stratification methods if preferred.
#'
#' \itemize{
#'   \item Abnormal Physiology Criteria: GCS 3–5; Respirations <5 or >30 per
#'   minute; Systolic BP <60 mm Hg
#'   \item High Risk: Probability of Survival < 0.2; ISS > 41; ISS > 24 with
#'   abnormal physiology
#'   \item Moderate Risk: Probability of Survival 0.2–0.5; ISS 16–41
#'   \item Low Risk: Probability of Survival > 0.5; ISS < 16; Normal physiology
#' }
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @returns
#' A named list with two tibbles:
#'
#' `overall`: A tibble summarizing overall mortality among trauma patients,
#' grouped by the variables specified in `groups`. Columns include:
#'
#' \itemize{
#'   \item `numerator_8_all` (number of survivors),
#'   \item `denominator_8_all` (total number
#'   of unique trauma incidents),
#'   \item `seqic_8_all` (survival proportion), and optionally
#'   \item `lower_ci_8`,
#'   \item `upper_ci_8` (confidence interval bounds if `calculate_ci` is
#'   specified).
#'   }
#'
#' `risk_group`: A tibble summarizing mortality stratified by risk group and any
#' additional grouping variables. Columns include:
#'
#' \itemize{
#'   \item `risk_group` (used for stratification),
#'   \item `numerator_8_risk` (survivors per group),
#'   \item `denominator_8_risk` (total incidents per group),
#'   \item `seqic_8_risk` (survival proportion per group), and optionally
#'   \item `lower_ci_8_risk`,
#'   \item `upper_ci_8_risk` (confidence interval bounds if `calculate_ci` is
#'   specified).
#' }
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Simulated dataset for SEQIC Indicator 8
#' test_data <- tibble::tibble(
#'   id = as.character(1:12),
#'   trauma_level = c("I", "II", "III", "IV", "V", "II", "I", "III", "IV", "II",
#'   "I", "III"),
#'   mortality = c(FALSE, "No", TRUE, "Yes", FALSE, TRUE, "No", FALSE, "Yes",
#'   FALSE, TRUE, "No"),
#'   risk = c("High", "High", "Moderate", "Moderate", "Low", "Low", "High",
#'   "Moderate", "Low", "Moderate", "High", "Low")
#' )
#'
#' # Run indicator 8 function
#' traumar::seqic_indicator_8(
#'   data = test_data,
#'   level = trauma_level,
#'   unique_incident_id = id,
#'   mortality_indicator = mortality,
#'   risk_group = risk
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_8 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  mortality_indicator,
  risk_group,
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

  # Ensure that the `mortality_indicator`
  mortality_indicator_check <- validate_data_pull(
    input = data,
    col = {{ mortality_indicator }},
    type = "error",
    var_name = "mortality_indicator"
  )

  # Validate the `mortality_indicator` column
  validate_class(
    input = mortality_indicator_check,
    class_type = c("logical", "character", "factor"),
    logic = "or",
    type = "error",
    var_name = "mortality_indicator"
  )

  # Ensure that `risk_group` can be validated
  risk_group_check <- validate_data_pull(
    input = data,
    col = {{ risk_group }},
    type = "error",
    var_name = "risk_group"
  )

  # Validate the `risk_group` column
  validate_character_factor(
    input = risk_group_check,
    type = "error",
    var_name = "risk_group"
  )

  # Check if all elements in groups are strings (i.e., character vectors) ----
  validate_character_factor(input = groups, type = "error", null_ok = TRUE)

  # Check if all `groups` exist in the `data`.
  validate_names(input = data, check_names = groups, type = "error")

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
  ### Calculations ----
  ###___________________________________________________________________________

  # Initiate the output as a list ----
  seqic_8 <- list()

  # Overall mortality, one row per unique incident ----
  seqic_8_all <- data |>
    dplyr::filter({{ level }} %in% included_levels) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::summarize(
      numerator_8_all = sum(
        !({{ mortality_indicator }} %in% c(TRUE, "Yes")),
        na.rm = TRUE
      ),
      denominator_8_all = dplyr::n(),
      seqic_8_all = dplyr::if_else(
        denominator_8_all > 0,
        numerator_8_all / denominator_8_all,
        NA_real_
      ),
      .by = {{ groups }}
    )

  # Mortality stratified by risk group ----
  seqic_8_risk <- data |>
    dplyr::filter({{ level }} %in% included_levels) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::summarize(
      numerator_8_risk = sum(
        !({{ mortality_indicator }} %in% c(TRUE, "Yes")),
        na.rm = TRUE
      ),
      denominator_8_risk = dplyr::n(),
      seqic_8_risk = dplyr::if_else(
        denominator_8_risk > 0,
        numerator_8_risk / denominator_8_risk,
        NA_real_
      ),
      .by = c({{ groups }}, {{ risk_group }})
    )

  # Compute confidence intervals if requested ----
  if (!is.null(calculate_ci)) {
    seqic_8_all <- seqic_8_all |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_8_all,
          x = numerator_8_all,
          n = denominator_8_all,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_8 = lower_ci, upper_ci_8 = upper_ci)
      )

    seqic_8_risk <- seqic_8_risk |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_8_risk,
          x = numerator_8_risk,
          n = denominator_8_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_8_risk = lower_ci, upper_ci_8_risk = upper_ci)
      )
  }

  # Label output or arrange by grouping vars ----
  if (is.null(groups)) {
    seqic_8$overall <- seqic_8_all |>
      tibble::add_column(data = "population/sample", .before = 1)
    seqic_8$risk_group <- seqic_8_risk |>
      tibble::add_column(data = "population/sample risk groups", .before = 1) |>
      dplyr::arrange({{ risk_group }})
  } else {
    seqic_8$overall <- seqic_8_all |>
      dplyr::arrange(!!!rlang::syms(groups))
    seqic_8$risk_group <- seqic_8_risk |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  # Return the final summarized dataset. ----
  return(seqic_8)
}
