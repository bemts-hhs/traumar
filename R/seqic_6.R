#' @title SEQIC Indicator 6 - Delayed Arrival Following Low GCS
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Computes SEQIC Indicator 6 for trauma system quality monitoring. This
#' indicator measures the proportion of patients presenting with a Glasgow Coma
#' Scale (GCS) score < 9 who arrive at a trauma level I–IV center more than 180
#' minutes after injury. It excludes patients transferred out of the facility
#' and focuses on those transferred into a facility.
#'
#' @inheritParams seqic_indicator_1
#' @param transfer_out_indicator Column name indicating whether the
#'   patient was transferred out of the initial trauma center. Logical,
#'   character, or factor type. Values representing "No" (e.g., FALSE, "No")
#'   indicate no transfer out.
#' @param receiving_indicator Column name indicating whether the
#'   patient was transferred into the trauma center. Logical, character, or
#'   factor type. Values representing "Yes" (e.g., TRUE, "Yes") indicate
#'   transfer in.
#' @param low_GCS_indicator Column name for identifying patients with a
#'   Glasgow Coma Scale score less than 9. Logical, character, or factor type.
#' @param time_from_injury_to_arrival Column name representing the time
#'   in minutes from injury occurrence to arrival at the trauma center. Numeric
#'   type.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details This function:
#' \itemize{
#'   \item Filters to trauma center records from facilities at trauma levels
#'   I–IV.
#'   \item Deduplicates records using `unique_incident_id`.
#'   \item Calculates:
#'     \itemize{
#'       \item {Numerator:} Patients with low GCS (< 9) who arrived more than
#'         180 minutes after injury, were transferred in, and not transferred
#'         out.
#'       \item {Denominator:} All patients with low GCS (< 9) who were
#'         transferred in and not transferred out.
#'     }
#'   \item Optionally calculates Wilson or Clopper-Pearson confidence intervals
#'     for the resulting proportion if `calculate_ci` is specified.
#' }
#'
#' @note
#'
#' Users must ensure input columns are appropriately coded and standardized.
#' Transfer and GCS indicators should use consistent logical or textual
#' representations.
#'
#' @return A tibble summarizing SEQIC Indicator 6 results. Includes numerator,
#'   denominator, calculated proportion, and optionally 95% confidence
#'   intervals.
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Create test data for Indicator 6
#' test_data <- tibble::tibble(
#'   id = as.character(1:10),
#'   trauma_level = rep(c("I", "II", "III", "IV", "V"), times = 2),
#'   transfer_out = c("No", "No", "Yes", "No", "No", "No", "No", "No", "No",
#'   "No"),
#'   transfer_in = c("Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes",
#'   "Yes", "Yes"),
#'   gcs_low = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#'   time_to_arrival = c(200, 100, 300, 190, 400, 181, 100, 179, 240, 178)
#' )
#'
#' # Run the indicator function
#' traumar::seqic_indicator_6(
#'   df = test_data,
#'   level = trauma_level,
#'   unique_incident_id = id,
#'   transfer_out_indicator = transfer_out,
#'   receiving_indicator = transfer_in,
#'   low_GCS_indicator = gcs_low,
#'   time_from_injury_to_arrival = time_to_arrival
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_6 <- function(
  df,
  level,
  included_levels = c("I", "II", "III", "IV"),
  unique_incident_id,
  transfer_out_indicator,
  receiving_indicator,
  low_GCS_indicator,
  time_from_injury_to_arrival,
  groups = NULL,
  calculate_ci = NULL,
  ...
) {
  ###___________________________________________________________________________
  ### Data validation
  ###___________________________________________________________________________

  # Validate that `df` is a data frame or tibble.
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      c(
        "{.var df} must be of class {.cls data.frame} or {.cls tibble}.",
        "i" = "{.var df} was an object of class {.cls {class(df)}}."
      )
    )
  }

  # Validate that `level` column is character or factor.
  level_check <- df |> dplyr::pull({{ level }})
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(
      c(
        "{.var level} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var level} was an object of class {.cls {class(level_check)}}."
      )
    )
  }

  # Make the `unique_incident_id` column accessible for validation.
  unique_incident_id_check <- df |>
    dplyr::pull({{ unique_incident_id }})

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
  transfer_out_indicator_check <- df |>
    dplyr::pull({{ transfer_out_indicator }})
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

  # Validate that `receiving_indicator` is character, factor, or logical.
  receiving_indicator_check <- df |> dplyr::pull({{ receiving_indicator }})
  if (
    !is.character(receiving_indicator_check) &&
      !is.factor(receiving_indicator_check) &&
      !is.logical(receiving_indicator_check)
  ) {
    cli::cli_abort(
      c(
        "{.var receiving_indicator} must be of class {.cls character}, {.cls factor}, or {.cls logical}.",
        "i" = "{.var receiving_indicator} was an object of class {.cls {class(receiving_indicator_check)}}."
      )
    )
  }

  # Validate that `low_GCS_indicator` is character, factor, or logical.
  low_GCS_indicator_check <- df |> dplyr::pull({{ low_GCS_indicator }})
  if (
    !is.character(low_GCS_indicator_check) &&
      !is.factor(low_GCS_indicator_check) &&
      !is.logical(low_GCS_indicator_check)
  ) {
    cli::cli_abort(
      c(
        "{.var low_GCS_indicator} must be of class {.cls character}, {.cls factor}, or {.cls logical}.",
        "i" = "{.var low_GCS_indicator} was an object of class {.cls {class(low_GCS_indicator_check)}}."
      )
    )
  }

  # Validate that `time_from_injury_to_arrival` is numeric.
  time_from_injury_to_arrival_check <- df |>
    dplyr::pull({{ time_from_injury_to_arrival }})
  if (!is.numeric(time_from_injury_to_arrival_check)) {
    cli::cli_abort(
      c(
        "{.var time_from_injury_to_arrival} must be of class {.cls numeric}.",
        "i" = "{.var time_from_injury_to_arrival} was an object of class {.cls {class(time_from_injury_to_arrival_check)}}."
      )
    )
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

  # Check if all groups exist in the `df`
  if (!all(groups %in% names(df))) {
    invalid_vars <- groups[!groups %in% names(df)]
    cli::cli_abort(
      "The following group variable(s) are not valid columns in {.var df}: {paste(invalid_vars, collapse = ', ')}"
    )
  }

  # Validate `calculate_ci` argument: must be NULL or "wilson" or "clopper-pearson".
  if (!is.null(calculate_ci)) {
    attempt <- try(
      match.arg(calculate_ci, choices = c("wilson", "clopper-pearson")),
      silent = TRUE
    )
    if (inherits(attempt, "try-error")) {
      cli::cli_abort(
        c(
          "If {.var calculate_ci} is not {cli::col_blue('NULL')}, it must be {.val wilson} or {.val clopper-pearson}.",
          "i" = "{.var calculate_ci} was {.val {calculate_ci}}."
        )
      )
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

  # Filter for trauma levels I-IV, deduplicate by `unique_incident_id`, then summarize.
  seqic_6 <- df |>
    dplyr::filter({{ level }} %in% included_levels) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::summarize(
      numerator_6 = sum(
        {{ low_GCS_indicator }} == TRUE & # GCS < 9
          {{ time_from_injury_to_arrival }} > 180 & # Time to arrival > 180 min
          {{ receiving_indicator }} %in% c(TRUE, "Yes") & # Hospital received
          {{ transfer_out_indicator }} %in% c(FALSE, "No"), # No transfer out
        na.rm = TRUE
      ),
      denominator_6 = sum(
        {{ low_GCS_indicator }} == TRUE &
          {{ receiving_indicator }} %in% c(TRUE, "Yes") &
          {{ transfer_out_indicator }} %in% c(FALSE, "No"),
        na.rm = TRUE
      ),
      seqic_6 = dplyr::if_else(
        denominator_6 > 0,
        numerator_6 / denominator_6,
        NA_real_
      ),
      .by = {{ groups }} # Grouping if applicable
    )

  # Optional: Add binomial confidence intervals if requested.
  if (!is.null(calculate_ci)) {
    seqic_6 <- seqic_6 |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_6,
          x = numerator_6, # Number of positive outcomes.
          n = denominator_6, # Number of eligible patients.
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |> # Drop intermediate columns.
          dplyr::rename(lower_ci_6 = lower_ci, upper_ci_6 = upper_ci) # Rename CIs for clarity.
      )
  }

  # Add a `Data` label if not grouped, or arrange output by grouping variables.
  if (is.null(groups)) {
    seqic_6 <- seqic_6 |>
      tibble::add_column(Data = "Population/Sample", .before = "numerator_6")
  } else if (!is.null(groups)) {
    seqic_6 <- seqic_6 |>
      dplyr::arrange(!!!rlang::syms(groups)) # Order rows based on group vars.
  }

  # Return the final summarized dataset.
  return(seqic_6)
}
