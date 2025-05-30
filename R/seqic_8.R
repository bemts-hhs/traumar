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

  # Ensure input is a data frame or tibble
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort(c(
      "{.var data} must be a data frame or tibble.",
      "i" = "You provided an object of class {.cls {class(data)}}."
    ))
  }

  # make the `level` column accessible for validation
  level_check <- tryCatch(
    {
      data |> dplyr::pull({{ level }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var level}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_8())
      )
    }
  )
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(c(
      "{.var level} must be character or factor.",
      "i" = "Provided class: {.cls {class(level_check)}}."
    ))
  }

  # make the `unique_incident_id` column accessible for validation
  unique_incident_id_check <- tryCatch(
    {
      data |> dplyr::pull({{ unique_incident_id }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var unique_incident_id}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_8())
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

  # Validate the `mortality_indicator` column
  mortality_indicator_check <- tryCatch(
    {
      data |> dplyr::pull({{ mortality_indicator }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var mortality_indicator}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_8())
      )
    }
  )
  if (
    !is.character(mortality_indicator_check) &&
      !is.factor(mortality_indicator_check) &&
      !is.logical(mortality_indicator_check)
  ) {
    cli::cli_abort(c(
      "{.var mortality_indicator} must be character, factor, or logical.",
      "i" = "Provided class: {.cls {class(mortality_indicator_check)}}."
    ))
  }

  # Validate the `risk_group` column
  risk_group_check <- tryCatch(
    {
      data |> dplyr::pull({{ risk_group }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var risk_group}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_8())
      )
    }
  )
  if (!is.character(risk_group_check) && !is.factor(risk_group_check)) {
    cli::cli_abort(c(
      "{.var risk_group} must be character or factor.",
      "i" = "Provided class: {.cls {class(risk_group_check)}}."
    ))
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
  ### Calculations
  ###___________________________________________________________________________

  # Initiate the output as a list
  seqic_8 <- list()

  # Overall mortality, one row per unique incident
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

  # Mortality stratified by risk group
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

  # Compute confidence intervals if requested
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

  # Label output or arrange by grouping vars
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

  return(seqic_8)
}
