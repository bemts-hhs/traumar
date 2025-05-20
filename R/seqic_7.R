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

  # Validate that `data` is a data frame or tibble.
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort(
      c(
        "{.var data} must be of class {.cls data.frame} or {.cls tibble}.",
        "i" = "{.var data} was an object of class {.cls {class(data)}}."
      )
    )
  }

  # make the `level` column accessible for validation
  level_check <- tryCatch(
    {
      data |> dplyr::pull({{ level }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var level}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_7())
      )
    }
  )
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(
      c(
        "{.var level} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var level} was an object of class {.cls {class(level_check)}}."
      )
    )
  }

  # make the `unique_incident_id` column accessible for validation
  unique_incident_id_check <- tryCatch(
    {
      data |> dplyr::pull({{ unique_incident_id }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var unique_incident_id}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_7())
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
        call = rlang::expr(seqic_indicator_7())
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

  # Validate that `time_from_injury_to_arrival` is numeric.
  time_from_injury_to_arrival_check <- tryCatch(
    {
      data |> dplyr::pull({{ time_from_injury_to_arrival }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var time_from_injury_to_arrival}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_7())
      )
    }
  )
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

  # Check if all groups exist in the `data`
  if (!all(groups %in% names(data))) {
    invalid_vars <- groups[!groups %in% names(data)]
    cli::cli_abort(
      "The following group variable(s) are not valid columns in {.var data}: {paste(invalid_vars, collapse = ', ')}"
    )
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
  ### Measure Calculation
  ###___________________________________________________________________________

  # Filter only trauma center levels I–IV
  seqic_7 <- data |>
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
      tibble::add_column(data = "population/sample", .before = "numerator_7")
  } else {
    # Arrange by grouping variables
    seqic_7 <- seqic_7 |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  return(seqic_7)
}
