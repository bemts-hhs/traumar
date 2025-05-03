#' @title SEQIC Indicator 7 - Delayed Arrival to Definitive Care
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Computes SEQIC Indicator 7, which measures the proportion of trauma patients
#' arriving at the definitive care facility trauma centers (level I–IV) more
#' than 180 minutes after injury. This indicator identifies delays in definitive
#' care. Confidence intervals may optionally be computed using the Wilson or
#' Clopper-Pearson method.
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
#'   denominator, and proportion. Confidence intervals are included if
#'   requested.
#'
#' @author Nicolas Foss Ed.D., MS
#'
#' @export
seqic_indicator_7 <- function(
  df,
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

  # Validate that `unique_incident_id` is character or factor.
  incident_id_check <- df |> dplyr::pull({{ unique_incident_id }})
  if (!is.character(incident_id_check) && !is.factor(incident_id_check)) {
    cli::cli_abort(
      c(
        "{.var unique_incident_id} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var unique_incident_id} was an object of class {.cls {class(incident_id_check)}}."
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

  # Validate `groups` argument: must be character vectors matching column names.
  if (!is.null(groups)) {
    if (!all(sapply(groups, is.character))) {
      cli::cli_abort(c(
        "All elements in {.var groups} must be strings.",
        "i" = "You passed a {.cls {class(groups)}} variable to {.var groups}."
      ))
    }
    if (!all(groups %in% names(df))) {
      invalid_vars <- groups[!groups %in% names(df)]
      cli::cli_abort(
        "The following group variable(s) are not valid columns in {.var df}: {paste(invalid_vars, collapse = ', ')}"
      )
    }
  }

  # Validate `calculate_ci` argument: must be NULL or "wilson" or
  # "clopper-pearson".
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

  ###___________________________________________________________________________
  ### Measure Calculation
  ###___________________________________________________________________________

  # Filter only trauma center levels I–IV
  seqic_7 <- df |>
    dplyr::filter({{ level }} %in% included_levels) |>

    # Deduplicate records by unique incident ID
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>

    # Create flag for arrivals >180 minutes after injury
    dplyr::mutate(
      arrive_greater_than_180 = {{ time_from_injury_to_arrival }} > 180
    ) |>

    # Summarize: count patients meeting the criteria (numerator) and total
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

  # Optionally compute confidence intervals
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

  # Add label if ungrouped
  if (is.null(groups)) {
    seqic_7 <- seqic_7 |>
      tibble::add_column(Data = "Population/Sample", .before = "numerator_7")
  } else {
    # Arrange by grouping variables
    seqic_7 <- seqic_7 |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  return(seqic_7)
}
