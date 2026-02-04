#' @title SEQIC Indicator 2 – Missing Incident Time
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function calculates System Evaluation and Quality Improvement Committee
#' (SEQIC) Indicator 2. This indicator evaluates the proportion of trauma
#' incidents with missing documented incident time across Level I–IV trauma
#' centers.
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
#'   denominator, and performance rate for the indicator. 95% confidence
#'   intervals are provided optionally.
#'
#' @examples
#'
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Data
#' data <- tibble::tibble(
#'   incident_id = as.character(101:106),
#'   trauma_level = c("I", "II", "III", "IV", "II", "I"),
#'   incident_time = as.POSIXct(c("2023-01-01 12:00", NA, "2023-01-02 14:15",
#'                                NA, "2023-01-03 09:30", "2023-01-04 16:45"))
#' )
#'
#' # Run the function
#' traumar::seqic_indicator_2(
#'   data = data,
#'   unique_incident_id = incident_id,
#'   level = trauma_level,
#'   incident_time = incident_time,
#'   calculate_ci = "clopper-pearson"
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_2 <- function(
  data,
  unique_incident_id,
  level,
  included_levels = c("I", "II", "III", "IV"),
  incident_time,
  groups = NULL,
  calculate_ci = NULL,
  ...
) {
  ###___________________________________________________________________________
  ### Data validation ----
  ###___________________________________________________________________________

  # validate `data` ----
  validate_data_structure(
    input = data,
    structure_type = c("data.frame", "tbl", "tbl_df"),
    logic = "or",
    type = "error"
  )

  # make the `unique_incident_id` column accessible for validation ----
  unique_incident_id_check <- validate_data_pull(
    input = data,
    type = "error",
    col = {{ unique_incident_id }},
    var_name = "unique_incident_id"
  )

  # validate `unique_incident_id` ----
  validate_class(
    input = unique_incident_id_check,
    class_type = c("numeric", "factor", "character"),
    logic = "or",
    type = "error"
  )

  # make the `incident_time` column accessible for validation ----
  incident_time_check <- validate_data_pull(
    input = data,
    type = "error",
    col = {{ incident_time }},
    var_name = "incident_time"
  )

  # validate `incident_time` ----
  validate_class(
    input = incident_time_check,
    class_type = c("date", "date-time", "hms", "character"),
    logic = "or",
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

  # Check if all elements in groups are strings (i.e., character vectors) ----
  validate_character_factor(input = groups, type = "error", null_ok = TRUE)

  # Check if all groups exist in the `data` ----
  validate_names(
    input = data,
    check_names = groups,
    type = "error",
    var_name = "groups",
    null_ok = TRUE
  )

  # Validate the `calculate_ci` argument ----
  # - If not NULL, must be either "wilson" or "clopper-pearson"
  # - Use match.arg() to enforce allowed values
  # - Catch invalid input silently and report cleanly with cli
  calculate_ci <- validate_choice(
    input = calculate_ci,
    choices = c("wilson", "clopper-pearson"),
    several.ok = FALSE,
    type = "error",
    null_ok = TRUE
  )

  # Validate the `included_levels` argument ----
  validate_class(
    input = included_levels,
    class_type = c("numeric", "character", "factor"),
    logic = "or",
    type = "error"
  )

  ###___________________________________________________________________________
  ### Calculations ----
  ###___________________________________________________________________________

  # Summarize the data for Indicator 2: ----
  # - Filter records to include only Level I–IV trauma centers.
  # - Remove duplicate incidents, keeping the first occurrence of each unique
  #   `unique_incident_id`.
  # - Summarize the data by calculating:
  #   - `numerator_2`: The number of incidents with missing `incident_time`.
  #   - `denominator_2`: The total number of unique incidents.
  #   - `seqic_2`: The proportion of incidents with missing `incident_time`
  #     (rounded to 3 decimal places).
  #   - Optionally, group results by columns specified in the `groups` argument.
  seqic_2 <- data |>
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

  # Optionally calculate confidence intervals for the proportions: ----
  # - If `calculate_ci` is provided, apply a binomial confidence interval method
  #   (Wilson or Clopper-Pearson).
  # - `nemsqa_binomial_confint()` calculates the confidence intervals for the
  #   proportion.
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

  # Assign a label to indicate whether the data represents population or ----
  # sample-level results:
  # - If no grouping is applied, label the data as "Population/Sample".
  if (is.null(groups)) {
    seqic_2 <- seqic_2 |>
      tibble::add_column(data = "population/sample", .before = "numerator_2") # Add the label column.
  } else if (!is.null(groups)) {
    seqic_2 <- seqic_2 |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  # Return the final summary dataframe for Indicator 2. ----
  return(seqic_2)
}
