#' @title SEQIC Indicator 7 - Delayed Arrival to Definitive Care
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Computes SEQIC Indicator 7, which measures the proportion of trauma patients
#' arriving at the definitive care facility trauma centers (level I–IV) more
#' than 180 minutes after injury. This indicator identifies delays in definitive
#' care.
#'
#' @inheritParams seqic_indicator_1
#' @inheritParams seqic_indicator_6
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details This function:
#' \itemize{
#'   \item Filters the dataset to trauma center levels I through IV.
#'   \item Deduplicates the dataset by `unique_incident_id`.
#'   \item Creates a logical flag for arrivals occurring more than 180 minutes
#'   after injury.
#'   \item Identifies definitive care records where the patient arrived greater
#'   than 180 minutes after the time of injury.
#'   \item Returns a summarized tibble with the number of such cases
#'   (numerator), total eligible records (denominator), and the proportion.
#'   \item Optionally includes 95% confidence intervals if `calculate_ci` is
#'   specified.
#' }
#'
#' @note
#'
#' The user must ensure all columns are correctly passed and that time values
#' are numeric and measured in minutes.
#'
#' @return A tibble summarizing SEQIC Indicator 7 results. Includes numerator,
#'   denominator, and proportion. 95% confidence intervals are included if
#'   requested.
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Create test data for Indicator 7
#' test_data <- tibble::tibble(
#'   id = as.character(1:10),
#'   trauma_level = rep(c("I", "II", "III", "IV", "V"), times = 2),
#'   time_to_arrival = c(200, 100, 220, 150, 400, 181, 90, 179, 240, 178),
#'   transfer_out = c("No", "No", "No", "No", "Yes", "No", "No", "No", "No",
#'   "No")
#' )
#'
#' # Run the indicator function
#' traumar::seqic_indicator_7(
#'   data = test_data,
#'   level = trauma_level,
#'   unique_incident_id = id,
#'   time_from_injury_to_arrival = time_to_arrival,
#'   transfer_out_indicator = transfer_out
#' )
#'
#' @author Nicolas Foss Ed.D., MS
#'
#' @export
seqic_indicator_7 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  time_from_injury_to_arrival,
  transfer_out_indicator,
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

  # Ensure that `time_from_injury_to_arrival` can be validated
  time_from_injury_to_arrival_check <- validate_data_pull(
    input = data,
    col = {{ time_from_injury_to_arrival }},
    type = "error",
    var_name = "time_from_injury_to_arrival"
  )

  # Validate that `time_from_injury_to_arrival` is numeric.
  validate_numeric(
    input = time_from_injury_to_arrival_check,
    type = "error",
    var_name = "time_from_injury_to_arrival"
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
  ### Measure Calculation ----
  ###___________________________________________________________________________

  # Filter only trauma center levels I–IV ----
  seqic_7 <- data |>
    dplyr::filter({{ level }} %in% included_levels) |>

    # Deduplicate records by unique incident ID ----
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>

    # Create flag for arrivals >180 minutes after injury ----
    dplyr::mutate(
      arrive_greater_than_180 = {{ time_from_injury_to_arrival }} > 180
    ) |>

    # Summarize: count patients meeting the criteria (numerator) and total ----
    # (denominator)
    dplyr::summarize(
      numerator_7 = sum(
        {{ transfer_out_indicator }} %in%
          c(FALSE, "No") &
          arrive_greater_than_180 == TRUE,
        na.rm = TRUE
      ),
      denominator_7 = dplyr::n(),
      seqic_7 = dplyr::if_else(
        denominator_7 > 0,
        numerator_7 / denominator_7,
        NA_real_
      ),
      .by = {{ groups }}
    )

  # Optionally compute confidence intervals ----
  if (!is.null(calculate_ci)) {
    # Apply binomial confidence interval function
    seqic_7 <- seqic_7 |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_7,
          x = numerator_7,
          n = denominator_7,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_7 = lower_ci, upper_ci_7 = upper_ci)
      )
  }

  # Add label if ungrouped ----
  if (is.null(groups)) {
    seqic_7 <- seqic_7 |>
      tibble::add_column(data = "population/sample", .before = "numerator_7")
  } else {
    # Arrange by grouping variables ----
    seqic_7 <- seqic_7 |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  # Return the final summarized dataset. ----
  return(seqic_7)
}
