#' @title SEQIC Indicator 4 - Autopsy and Long LOS Without Autopsy
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Computes SEQIC Indicator 4a and 4b for trauma center performance. Indicator
#' 4a captures the proportion of deceased trauma patients at trauma level I–IV
#' facilities who had an autopsy performed. Indicator 4b identifies deceased
#' trauma patients with a prolonged length of stay (LOS > 3 days) but without an
#' autopsy.
#'
#' @inheritParams seqic_indicator_1
#' @param ed_disposition Column representing the emergency department
#'   disposition. For a record to be picked up in this function, the ED
#'   dispostion must be documented as "Deceased/Expired".
#' @param ed_LOS Column for the calculated ED length of stay, measured in
#'   minutes.
#' @param hospital_disposition Column representing the hospital disposition. For
#'   a record to be picked up in this function, the hospital dispostion must be
#'   documented as "Deceased/Expired".
#' @param hospital_LOS Column for the calculated hospital length of stay,
#'   measured in minutes.
#' @param autopsy Unquoted column name indicating whether an autopsy was
#'   performed. Expected values: `"Yes"` or `NA`.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details This function:
#' \itemize{
#'   \item Filters trauma records to those with a trauma center level of I–IV.
#'   \item Identifies records where the patient died, based on ED or hospital
#'   disposition.
#'   \item Deduplicates by `unique_incident_id` to ensure one record per
#'   incident.
#'   \item For Indicator 4a, calculates the proportion of deceased patients who
#'   received an autopsy.
#'   \item For Indicator 4b, calculates the proportion of deceased patients with
#'   a hospital or ED length of stay greater than 72 hours (4320 minutes) and no
#'   autopsy performed.
#' }
#'
#' @note
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @return A tibble summarizing SEQIC Indicator 4a and 4b results. Includes
#'   numerator, denominator, and performance rate for the indicator. 95%
#'   confidence intervals are provided optionally.
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Create a synthetic test dataset
#' test_data <- tibble::tibble(
#'   id = as.character(1:8),
#'   trauma_level = c("I", "II", "III", "IV", "I", "II", "III", "IV"),
#'   ed_disp = c(
#'     "Operating Room",
#'     "Admitted",
#'     "Deceased/Expired",
#'     "Transferred",
#'     "Deceased/Expired",
#'     "Deceased/Expired",
#'     "Admitted",
#'     "Deceased/Expired"
#'   ),
#'   ed_los = c(120, 200, 5000, 180, 3000, 4321, 60, 4000),
#'   hosp_disp = c(
#'     "Deceased/Expired",
#'     "Deceased/Expired",
#'     "Deceased/Expired",
#'     "Discharged",
#'     "Deceased/Expired",
#'     "Deceased/Expired",
#'     "Discharged",
#'     "Deceased/Expired"
#'   ),
#'   hosp_los = c(3000, 4500, 1000, 200, 5000, 4400, 150, 3000),
#'   autopsy_done = c("Yes", "No", "No", NA, "Yes", "No", NA, "Yes")
#' )
#'
#' # Run the indicator function
#' traumar::seqic_indicator_4(
#'   data = test_data,
#'   level = trauma_level,
#'   ed_disposition = ed_disp,
#'   ed_LOS = ed_los,
#'   hospital_disposition = hosp_disp,
#'   hospital_LOS = hosp_los,
#'   unique_incident_id = id,
#'   autopsy = autopsy_done
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
seqic_indicator_4 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  ed_disposition,
  ed_LOS,
  hospital_disposition,
  hospital_LOS,
  unique_incident_id,
  autopsy,
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

  # make the `level` column accessible for validation
  level_check <- tryCatch(
    {
      data |> dplyr::pull({{ level }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var level}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_4())
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

  # Validate `ed_disposition`
  ed_disp_check <- tryCatch(
    {
      data |> dplyr::pull({{ ed_disposition }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var ed_disposition}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_4())
      )
    }
  )
  if (!is.character(ed_disp_check) && !is.factor(ed_disp_check)) {
    cli::cli_abort(
      c(
        "{.var ed_disposition} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var ed_disposition} was an object of class {.cls {class(ed_disp_check)}}."
      )
    )
  }

  # Validate `hospital_disposition`
  hospital_disp_check <- tryCatch(
    {
      data |> dplyr::pull({{ hospital_disposition }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var hospital_disposition}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_4())
      )
    }
  )
  if (!is.character(hospital_disp_check) && !is.factor(hospital_disp_check)) {
    cli::cli_abort(
      c(
        "{.var hospital_disposition} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var hospital_disposition} was an object of class {.cls {class(hospital_disp_check)}}."
      )
    )
  }

  # Validate `ed_LOS`
  ed_los_check <- tryCatch(
    {
      data |> dplyr::pull({{ ed_LOS }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var ed_LOS}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_4())
      )
    }
  )
  if (!is.numeric(ed_los_check)) {
    cli::cli_abort(
      c(
        "{.var ed_LOS} must be of class {.cls numeric}.",
        "i" = "{.var ed_LOS} was an object of class {.cls {class(ed_los_check)}}."
      )
    )
  }

  # Validate `hospital_LOS`
  hospital_los_check <- tryCatch(
    {
      data |> dplyr::pull({{ hospital_LOS }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var hospital_LOS}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_4())
      )
    }
  )
  if (!is.numeric(hospital_los_check)) {
    cli::cli_abort(
      c(
        "{.var hospital_LOS} must be of class {.cls numeric}.",
        "i" = "{.var hospital_LOS} was an object of class {.cls {class(hospital_los_check)}}."
      )
    )
  }

  # Validate `autopsy`
  autopsy_check <- tryCatch(
    {
      data |> dplyr::pull({{ autopsy }})
    },
    error = function(e) {
      cli::cli_abort(
        "It was not possible to validate {.var autopsy}, please check this column in the function call.",
        call = rlang::expr(seqic_indicator_4())
      )
    }
  )
  if (!is.character(autopsy_check) && !is.factor(autopsy_check)) {
    cli::cli_abort(
      c(
        "{.var autopsy} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var autopsy} was an object of class {.cls {class(autopsy_check)}}."
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
        call = rlang::expr(seqic_indicator_4())
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

  if (!is.null(groups)) {
    if (!is.character(groups)) {
      cli::cli_abort(c(
        "All elements in {.var groups} must be strings.",
        "i" = "You passed an object of class {.cls {class(groups)}} to {.var groups}."
      ))
    }

    # Check if all groups exist in the `data`
    if (!all(groups %in% names(data))) {
      invalid_vars <- groups[!groups %in% names(data)]
      cli::cli_abort(
        "The following group variable(s) are not valid columns in {.var data}: {paste(invalid_vars, collapse = ', ')}"
      )
    }
  }

  # Validate `calculate_ci`
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

  ###___________________________________________________________________________
  # Indicator 4A - Presence of Autopsy in Deceased Patients at Trauma Centers
  ###___________________________________________________________________________
  seqic_4a <- data |>
    dplyr::filter(
      # Limit to valid trauma levels
      {{ level }} %in% included_levels,
      # Include cases where ED or hospital disposition is "Deceased/Expired"
      dplyr::if_any(
        c({{ ed_disposition }}, {{ hospital_disposition }}),
        ~ . == "Deceased/Expired"
      )
    ) |>
    # Ensure unique incidents (in case of duplicated records)
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    # Compute numerator (autopsies = "Yes") and denominator
    dplyr::summarize(
      numerator_4a = sum({{ autopsy }} == "Yes", na.rm = TRUE),
      denominator_4a = dplyr::n(),
      seqic_4a = dplyr::if_else(
        denominator_4a > 0,
        numerator_4a / denominator_4a,
        NA_real_
      ),
      .by = {{ groups }}
    )

  # Optional confidence intervals for 4a
  if (!is.null(calculate_ci)) {
    seqic_4a <- seqic_4a |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_4a,
          x = numerator_4a,
          n = denominator_4a,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_4a = lower_ci, upper_ci_4a = upper_ci)
      ) |>
      dplyr::relocate(lower_ci_4a, .after = seqic_4a) |>
      dplyr::relocate(upper_ci_4a, .after = lower_ci_4a)
  }

  ###___________________________________________________________________________
  # Indicator 4B - No Autopsy + Long LOS in Deceased Patients
  ###___________________________________________________________________________
  seqic_4b <- data |>
    dplyr::filter(
      {{ level }} %in% included_levels,
      dplyr::if_any(
        c({{ ed_disposition }}, {{ hospital_disposition }}),
        ~ . == "Deceased/Expired"
      ),
      # Long LOS (> 72 hours = 4320 minutes)
      dplyr::if_any(c({{ ed_LOS }}, {{ hospital_LOS }}), ~ . > 4320)
    ) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    # Numerator = no autopsy or autopsy not "Yes"
    dplyr::summarize(
      numerator_4b = sum(is.na({{ autopsy }}) | {{ autopsy }} != "Yes"),
      denominator_4b = dplyr::n(),
      seqic_4b = dplyr::if_else(
        denominator_4b > 0,
        numerator_4b / denominator_4b,
        NA_real_
      ),
      .by = {{ groups }}
    )

  # Optional confidence intervals for 4b
  if (!is.null(calculate_ci)) {
    seqic_4b <- seqic_4b |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_4b,
          x = numerator_4b,
          n = denominator_4b,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_4b = lower_ci, upper_ci_4b = upper_ci)
      ) |>
      dplyr::relocate(lower_ci_4b, .after = seqic_4b) |>
      dplyr::relocate(upper_ci_4b, .after = lower_ci_4b)
  }

  # Combine 4a and 4b results; assign label for ungrouped reporting.
  if (is.null(groups)) {
    seqic_4 <- dplyr::bind_cols(
      seqic_4a,
      seqic_4b
    ) |>
      tibble::add_column(data = "population/sample", .before = "numerator_4a")
  } else {
    seqic_4 <- seqic_4a |>
      dplyr::full_join(seqic_4b, by = dplyr::join_by(!!!rlang::syms(groups))) |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  return(seqic_4)
}
