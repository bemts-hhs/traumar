#' @title SEQIC Indicator 5 - Alcohol and Drug Screening
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Computes SEQIC Indicator 5a–5d for trauma system quality monitoring. These
#' indicators measure alcohol and drug screening rates among trauma patients at
#' trauma level I–IV facilities.
#'
#' @inheritParams seqic_indicator_1
#' @param blood_alcohol_content Unquoted column name for blood alcohol
#'   concentration. Numeric. A non-missing value indicates a test was performed.
#'   Values greater than zero are considered positive results.
#' @param drug_screen Unquoted column name for the drug screen result. Character
#'   or factor. May contain keywords (e.g., "opioid", "cocaine", "none"). The
#'   keywords used in this function correspond to the National Trauma Data Bank
#'   (NTDB) field values for the corresponding data element.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details This function:
#' \itemize{
#'   \item Filters to trauma records at trauma levels I–IV.
#'   \item Deduplicates by `unique_incident_id` to ensure one record per
#'     incident.
#'   \item Calculates four sub-measures:
#'     \itemize{
#'       \item {Indicator 5a:} Proportion of patients with a blood
#'         alcohol test performed.
#'       \item {Indicator 5b:} Among those tested, the proportion with
#'         BAC > 0.
#'       \item {Indicator 5c:} Proportion of patients with any recorded
#'         drug screen result.
#'       \item {Indicator 5d:} Among those with a drug result, the
#'         proportion that included a known positive drug (e.g., opioids,
#'         cocaine, THC).
#'     }
#'   \item Matches drug-related terms using regular expressions for a broad set
#'     of known substances. Matching is case-insensitive.
#' }
#'
#' @note
#'
#' Users must ensure input columns are correctly named and contain standardized
#' values where applicable. Drug screen values should ideally use consistent
#' naming or be mapped to recognizable substance terms prior to function use.
#'
#' @return A tibble summarizing SEQIC Indicator 5a–5d results. Includes
#'   numerator, denominator, and calculated proportion for each measure.
#'   Optionally includes 95% confidence intervals.
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Create synthetic test data for Indicators 5a–5d
#' test_data <- tibble::tibble(
#'   id = as.character(1:10),
#'   trauma_level = rep(c("I", "II", "III", "IV", "V"), each = 2),
#'   bac = c(0.08, NA, 0, 0.02, NA, 0.15, NA, NA, 0, 0),
#'   drug = c(
#'     "opioid", "none", "cocaine", "none", NA,
#'     "benzodiazepine", "alcohol", "thc", "none", NA
#'   )
#' )
#'
#' # Run the indicator function
#' traumar::seqic_indicator_5(
#'   data = test_data,
#'   level = trauma_level,
#'   unique_incident_id = id,
#'   blood_alcohol_content = bac,
#'   drug_screen = drug
#' ) |>
#'   tidyr::pivot_longer(cols = -1, names_to = "Indicator", values_to =
#'   "Values")
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export

seqic_indicator_5 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  blood_alcohol_content,
  drug_screen,
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
    type = "error",
    logic = "or"
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

  # Ensure `blood_alcohol_content` can be validated ----
  blood_alcohol_content_check <- validate_data_pull(
    input = data,
    type = "error",
    col = {{ blood_alcohol_content }},
    var_name = "blood_alcohol_content"
  )

  # Validate `blood_alcohol_content` ----
  validate_numeric(
    input = blood_alcohol_content_check,
    min = 0,
    type = "error",
    var_name = "blood_alcohol_content"
  )

  # Ensure `drug_screen` can be validated
  drug_screen_check <- validate_data_pull(
    input = data,
    type = "error",
    col = {{ drug_screen }},
    var_name = "drug_screen"
  )
  # Validate `drug_screen`
  validate_character_factor(
    input = drug_screen_check,
    type = "error",
    var_name = "drug_screen"
  )

  # Check if all elements in groups are strings (i.e., character vectors) ----
  validate_character_factor(input = groups, type = "error", null_ok = TRUE)

  # Check if all `groups` exist in the `data`.
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
  ### Set up drug-related keyword matching via regular expressions ----
  ###___________________________________________________________________________

  # Options are consistent with the National Trauma Data Bank ----
  # responses as of the 2025 release
  # Define keyword vectors
  drug_keywords <- c(
    "alcohol",
    "bzo",
    "benzodiazepine",
    "amp",
    "amphetamine",
    "coc",
    "cocaine",
    "thc",
    "cannabinoid",
    "opi",
    "opioid",
    "pcp",
    "phencyclidine",
    "bar",
    "barbiturate",
    "mamp",
    "methamphetamine",
    "mdma",
    "ectasy",
    "mtd",
    "methadone",
    "tca",
    "tricyclic antidepressant",
    "oxy",
    "oxycodone",
    "none",
    "other"
  )

  # keywords for a positive test ----
  positive_drug_keywords <- setdiff(drug_keywords, "none")

  # Collapse into regular expression strings ----
  drug_pattern_terms <- stringr::str_c(drug_keywords, collapse = "|")
  positive_drug_pattern_terms <- stringr::str_c(
    positive_drug_keywords,
    collapse = "|"
  )

  # Final patterns (case-insensitive, non-capturing) ----
  drug_pattern <- sprintf("(?:%s)", drug_pattern_terms)
  positive_drug_pattern <- sprintf("(?:%s)", positive_drug_pattern_terms)

  ###___________________________________________________________________________
  ### Calculations ----
  ###___________________________________________________________________________

  ###___________________________________________________________________________
  ### Compute numerator and denominator ----
  ### for each SEQIC Indicator 5 sub-measure
  ###___________________________________________________________________________
  seqic_5 <- data |>
    dplyr::filter({{ level }} %in% included_levels) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::summarize(
      # 5a: Proportion with BAC test performed
      numerator_5a = sum(!is.na({{ blood_alcohol_content }})),
      denominator_5a = dplyr::n(),
      seqic_5a = dplyr::if_else(
        denominator_5a > 0,
        numerator_5a / denominator_5a,
        NA_real_
      ),

      # 5b: Among those tested, proportion with BAC > 0 ----
      numerator_5b = sum({{ blood_alcohol_content }} > 0, na.rm = TRUE),
      denominator_5b = sum(!is.na({{ blood_alcohol_content }})),
      seqic_5b = dplyr::if_else(
        denominator_5b > 0,
        numerator_5b / denominator_5b,
        NA_real_
      ),

      # 5c: Proportion with any drug result (positive, none, or other) ----
      numerator_5c = sum(
        grepl(
          pattern = drug_pattern,
          x = {{ drug_screen }},
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      denominator_5c = dplyr::n(),
      seqic_5c = dplyr::if_else(
        denominator_5c > 0,
        numerator_5c / denominator_5c,
        NA_real_
      ),

      # 5d: Among those with a result, proportion with a positive  ----
      # drug result
      numerator_5d = sum(
        grepl(
          pattern = positive_drug_pattern,
          x = {{ drug_screen }},
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      denominator_5d = sum(
        grepl(
          pattern = drug_pattern,
          x = {{ drug_screen }},
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      seqic_5d = dplyr::if_else(
        denominator_5d > 0,
        numerator_5d / denominator_5d,
        NA_real_
      ),
      .by = {{ groups }}
    )

  if (!is.null(calculate_ci)) {
    seqic_5 <- seqic_5 |>
      dplyr::bind_cols(
        # Compute and bind all CI columns
        nemsqar::nemsqa_binomial_confint(
          data = seqic_5,
          x = numerator_5a,
          n = denominator_5a,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_5a = lower_ci, upper_ci_5a = upper_ci),

        nemsqar::nemsqa_binomial_confint(
          data = seqic_5,
          x = numerator_5b,
          n = denominator_5b,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_5b = lower_ci, upper_ci_5b = upper_ci),

        nemsqar::nemsqa_binomial_confint(
          data = seqic_5,
          x = numerator_5c,
          n = denominator_5c,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_5c = lower_ci, upper_ci_5c = upper_ci),

        nemsqar::nemsqa_binomial_confint(
          data = seqic_5,
          x = numerator_5d,
          n = denominator_5d,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_5d = lower_ci, upper_ci_5d = upper_ci)
      ) |>
      # Relocate CI columns immediately after their respective proportion ----
      # columns
      dplyr::relocate(lower_ci_5a, upper_ci_5a, .after = seqic_5a) |>
      dplyr::relocate(lower_ci_5b, upper_ci_5b, .after = seqic_5b) |>
      dplyr::relocate(lower_ci_5c, upper_ci_5c, .after = seqic_5c) |>
      dplyr::relocate(lower_ci_5d, upper_ci_5d, .after = seqic_5d)
  }

  # Assign label for ungrouped reporting, or sort grouped reporting ----
  if (is.null(groups)) {
    seqic_5 <- seqic_5 |>
      tibble::add_column(data = "population/sample", .before = "numerator_5a")
  } else {
    seqic_5 <- seqic_5 |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  # Return the final summarized data for Indicator 5. ----
  return(seqic_5)
}
