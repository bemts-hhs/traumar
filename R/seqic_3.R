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
#'   trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV", "I", "II"),
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
  ### Data validation
  ###___________________________________________________________________________

  # Validate if `data` is a data frame or tibble.
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    cli::cli_abort(
      c(
        "{.var data} must be of class {.cls data.frame} or {.cls tibble}.",
        "i" = "{.var data} was an object of class {.cls {class(data)}}."
      )
    )
  }

  # Make the `trauma_type` column accessible for validation.
  trauma_type_check <- tryCatch(
    {
      data |> dplyr::pull({{ trauma_type }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var trauma_type}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_3())
      )
    }
  )

  # Validate `trauma_type` to ensure it's either character or factor.
  if (
    !is.character(trauma_type_check) &&
      !is.factor(trauma_type_check)
  ) {
    cli::cli_abort(
      c(
        "{.var trauma_type} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var trauma_type} was an object of class {.cls {class(trauma_type_check)}}."
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
        call = rlang::expr(seqic_indicator_3())
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

  # Make the `probability_of_survival` column accessible for validation.
  probability_of_survival_check <- tryCatch(
    {
      data |> dplyr::pull({{ probability_of_survival }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var probability_of_survival}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_3())
      )
    }
  )

  # Validate `probability_of_survival` to ensure it's either character or factor.
  if (!is.numeric(probability_of_survival_check)) {
    cli::cli_abort(
      c(
        "{.var probability_of_survival} must be of class {.cls numeric}.",
        "i" = "{.var probability_of_survival} was an object of class {.cls {class(probability_of_survival_check)}}."
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
        call = rlang::expr(seqic_indicator_3())
      )
    }
  )

  # Validate `level` to ensure it's either character or factor.
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(
      c(
        "{.var level} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var level} was an object of class {.cls {class(level_check)}}."
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

  # Check if all `groups` exist in the `data`.
  if (!all(groups %in% names(data))) {
    invalid_vars <- groups[!groups %in% names(data)]
    cli::cli_abort(
      "The following group variable(s) are not valid columns in {.var data}: {paste(invalid_vars, collapse = ', ')}"
    )
  }

  # Validate the `calculate_ci` argument to ensure it's either "wilson" or "clopper-pearson".
  if (!is.null(calculate_ci)) {
    # Attempt to match the argument against allowed choices.
    attempt <- try(
      match.arg(calculate_ci, choices = c("wilson", "clopper-pearson")),
      silent = TRUE
    )

    # If `match.arg` fails, provide a user-friendly error message.
    if (inherits(attempt, "try-error")) {
      cli::cli_abort(
        c(
          "If {.var calculate_ci} is not {cli::col_blue('NULL')}, it must be {.val wilson} or {.val clopper-pearson}.",
          "i" = "{.var calculate_ci} was {.val {calculate_ci}}."
        )
      )
    }

    # If valid, overwrite `calculate_ci` with standardized value.
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

  # Filter the data for valid levels and exclude "Burn" trauma types.
  seqic_3 <- data |>
    dplyr::filter(
      {{ level }} %in% included_levels,
      {{ trauma_type }} != "Burn"
    ) |>
    dplyr::distinct(
      {{ unique_incident_id }},
      .keep_all = TRUE
    ) |>
    # Calculate the numerator and denominator for Indicator 3.
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

  # Optionally calculate confidence intervals for the proportions:
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

  # Add a label column to indicate whether the data represents population or sample-level results.
  if (is.null(groups)) {
    seqic_3 <- seqic_3 |>
      tibble::add_column(data = "population/sample", .before = "numerator_3") # Add the label column.
  } else if (!is.null(groups)) {
    seqic_3 <- seqic_3 |>
      dplyr::arrange(!!!rlang::syms(groups)) # Arrange the results by the specified grouping variables.
  }

  # Return the final summarized data for Indicator 3.
  return(seqic_3)
}
