#' @title SEQIC Indicator 3 - Presence of Probability of Survival Calculations
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function calculates Indicator 3, a measure of the proportion of trauma
#' incidents where the probability of survival is recorded. It filters the data
#' by trauma center level (I-IV), excluding burn cases, and computes the
#' proportion of incidents with a valid probability of survival value.
#'
#' @inheritParams seqic_indicator_1
#' @param trauma_type A column name indicating the type of trauma. The function
#'   filters out "Burn" cases.
#' @param probability_of_survival A column name for the probability of survival
#'   for each incident.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details This function:
#' \itemize{
#'   \item Filters trauma records to those with a trauma center level of I–IV.
#'   \item Excludes records with a trauma type of "Burn".
#'   \item Deduplicates by `unique_incident_id` to ensure one record per
#'   incident.
#'   \item Calculates the proportion of records with a non-missing
#'   `probability_of_survival`.
#' }
#'
#' @note
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @return A tibble summarizing SEQIC Indicator 3 results. Includes numerator,
#'   denominator, and performance rate for the indicator. 95% confidence
#'   intervals are provided optionally.
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Create a synthetic test dataset
#' test_data <- tibble::tibble(
#'   unique_id = as.character(1:10),
#'   trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV", "I",
#'   "II"),
#'   trauma_category = c("Blunt", "Penetrating", "Burn", "Blunt", "Penetrating",
#'                       "Burn", "Blunt", "Penetrating", "Blunt", "Blunt"),
#'   survival_prob = c(0.95, 0.89, NA, 0.76, NA, 0.92, 0.88, NA, 0.97, 0.91)
#' )
#'
#' # Run the indicator function
#' traumar::seqic_indicator_3(
#'   data = test_data,
#'   level = trauma_level,
#'   trauma_type = trauma_category,
#'   unique_incident_id = unique_id,
#'   probability_of_survival = survival_prob,
#'   groups = "trauma_level"
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_3 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  trauma_type,
  unique_incident_id,
  probability_of_survival,
  groups = NULL,
  calculate_ci = NULL,
  ...
) {
  ###___________________________________________________________________________
  ### Data validation ----
  ###___________________________________________________________________________

  # Validate if `data` is a data frame or tibble.
  validate_data_structure(
    input = data,
    structure_type = c("data.frame", "tbl", "tbl_df"),
    type = "error",
    logic = "or"
  )

  # Make the `trauma_type` column accessible for validation. ----
  trauma_type_check <- validate_data_pull(
    input = data,
    type = "error",
    col = {{ trauma_type }},
    var_name = "trauma_type"
  )

  # Validate `trauma_type` to ensure it's either character or factor. ----
  validate_character_factor(
    input = trauma_type_check,
    type = "error",
    var_name = "trauma_type"
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

  # Make the `probability_of_survival` column accessible for validation. ----
  probability_of_survival_check <- validate_data_pull(
    input = data,
    type = "error",
    col = {{ probability_of_survival }},
    var_name = "probability_of_survival"
  )

  # Validate `probability_of_survival` ----
  validate_numeric(
    input = probability_of_survival_check,
    min = 0,
    max = 1,
    type = "error",
    var_name = "probability_of_survival"
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
  ### Calculations ----
  ###___________________________________________________________________________

  # Filter the data for valid levels and exclude "Burn" trauma types. ----
  seqic_3 <- data |>
    dplyr::filter(
      {{ level }} %in% included_levels,
      {{ trauma_type }} != "Burn"
    ) |>
    dplyr::distinct(
      {{ unique_incident_id }},
      .keep_all = TRUE
    ) |>
    # Calculate the numerator and denominator for Indicator 3. ----
    dplyr::summarize(
      numerator_3 = sum(!is.na({{ probability_of_survival }})), # Count non-missing values in `probability_of_survival`.
      denominator_3 = dplyr::n(), # Count the total number of unique incidents.
      seqic_3 = dplyr::if_else(
        denominator_3 > 0,
        numerator_3 / denominator_3,
        NA_real_
      ), # Calculate the proportion of incidents with survival probability recorded.
      .by = {{ groups }} # Optionally group by specified columns.
    )

  # Optionally calculate confidence intervals for the proportions: ----
  if (!is.null(calculate_ci)) {
    seqic_3 <- seqic_3 |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_3,
          x = numerator_3, # Number of successes (non-missing probability of survival).
          n = denominator_3, # Total number of incidents.
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_3 = lower_ci, upper_ci_3 = upper_ci) # Rename the CI columns.
      )
  }

  # Add a label column ----
  # to indicate whether the data represents population or sample-level results.
  if (is.null(groups)) {
    seqic_3 <- seqic_3 |>
      tibble::add_column(data = "population/sample", .before = "numerator_3") # Add the label column.
  } else if (!is.null(groups)) {
    seqic_3 <- seqic_3 |>
      dplyr::arrange(!!!rlang::syms(groups)) # Arrange the results by the specified grouping variables.
  }

  # Return the final summarized data for Indicator 3. ----
  return(seqic_3)
}
