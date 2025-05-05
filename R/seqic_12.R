#' @title SEQIC Indicator 12 - Timeliness of Data Entry Post-Discharge
#'
#' @description
#' Calculates the proportion of trauma cases where data were entered into the
#' trauma registry within a defined number of days post-discharge. This measure
#' supports trauma system quality improvement by identifying facilities meeting
#' timely documentation expectations.
#'
#' @inheritParams seqic_indicator_1
#' @param facility_id Numeric, character, or factor. Column giving the unique
#'   facility identifiers in the trauma dataset.
#' @param exclude_facility_list Optional. Numeric, character, or factor. List of
#'   facilities to exclude from analysis due to known data quality issues or
#'   other justifiable reasons. Defaults to `NULL`.
#' @param data_entry_time Numeric. Column representing the time in days between
#'   patient discharge and trauma registry data entry.
#' @param data_entry_standard Numeric. The maximum allowable number of days
#'   between discharge and data entry. Records entered within this threshold are
#'   considered timely. Default is `60`.
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details
#'
#' This function:
#' \itemize{
#'   \item Filters to include only patients treated at Level I–IV trauma
#'   centers.
#'   \item Excludes records from facilities specified by the user, if
#'   applicable.
#'   \item Deduplicates by `unique_incident_id` to ensure each incident is
#'   counted once.
#'   \item Flags records where data entry occurred within `data_entry_standard`
#'   days of discharge.
#'   \item Optionally calculates confidence intervals using methods from
#'   `nemsqa_binomial_confint()`.
#'   \item Returns a tibble with numerator,
#'   denominator, and proportion of timely entries, with optional confidence
#'   intervals and population/sample labels.
#' }
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @returns
#' A tibble containing:
#' \itemize{
#'   \item `numerator_12`: Number of records meeting the data entry timeliness
#'   threshold.
#'   \item `denominator_12`: Total number of included trauma records.
#'   \item `seqic_12`: Proportion of timely data entries.
#'   \item `lower_ci_12`,
#'   \item `upper_ci_12`: (Optional) 95% confidence interval bounds.
#' }
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Simulated data for SEQIC Indicator 12
#' test_data <- tibble::tibble(
#'   id = as.character(1:10),
#'   trauma_level = c("I", "II", "III", "IV", "II", "I", "IV", "III", "II",
#'   "I"),
#'   facility = c("A", "B", "C", "D", "A", "C", "B", "A", "C", "D"),
#'   data_entry_delay = c(30, 65, 10, 70, 45, 20, 80, 15, 55, 90)
#' )
#'
#' # Run the function
#' traumar::seqic_indicator_12(
#'   df = test_data,
#'   level = trauma_level,
#'   included_levels = c("I", "II", "III", "IV"),
#'   facility_id = facility,
#'   unique_incident_id = id,
#'   exclude_facility_list = c("D"),
#'   data_entry_time = data_entry_delay,
#'   data_entry_standard = 60,
#'   calculate_ci = "wilson"
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_12 <-
  function(
    df,
    level,
    included_levels = c("I", "II", "III", "IV"),
    facility_id,
    exclude_facility_list = NULL,
    unique_incident_id,
    data_entry_time,
    data_entry_standard = 60,
    groups = NULL,
    calculate_ci = NULL,
    ...
  ) {
    ###___________________________________________________________________________
    ### Data validation
    ###___________________________________________________________________________

    # Ensure input is a data frame or tibble
    if (!is.data.frame(df) && !tibble::is_tibble(df)) {
      cli::cli_abort(c(
        "{.var df} must be a data frame or tibble.",
        "i" = "You provided an object of class {.cls {class(df)}}."
      ))
    }

    # Validate trauma level input column
    level_check <- df |> dplyr::pull({{ level }})
    if (!is.character(level_check) && !is.factor(level_check)) {
      cli::cli_abort(c(
        "{.var level} must be character or factor.",
        "i" = "Provided class: {.cls {class(level_check)}}."
      ))
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

    # Validate facility ID type
    facility_id_check <- df |> dplyr::pull({{ facility_id }})
    if (
      !is.numeric(facility_id_check) &&
        !is.character(facility_id_check) &&
        !is.factor(facility_id_check)
    ) {
      cli::cli_abort(
        c(
          "{.var facility_id} must be of class {.cls character}, {.cls factor}, or {.cls numeric}.",
          "i" = "{.var facility_id} was an object of class {.cls {class(facility_id_check)}}."
        )
      )
    }

    # Validate that data_entry_time is numeric
    data_entry_time_check <- df |> dplyr::pull({{ data_entry_time }})
    if (!is.numeric(data_entry_time_check)) {
      cli::cli_abort(
        c(
          "{.var data_entry_time} must be of class {.cls numeric}.",
          "i" = "{.var data_entry_time} was an object of class {.cls {class(data_entry_time_check)}}."
        )
      )
    }

    # Validate the cutoff standard for timeliness
    if (!is.numeric(data_entry_standard)) {
      cli::cli_abort(
        c(
          "{.var data_entry_standard} must be numeric.",
          "i" = "Provided object is of class {.cls {class(data_entry_standard)}}."
        )
      )
    }

    # Validate the `included_levels` argument
    if (
      !is.character({{ included_levels }}) &&
        !is.numeric({{ included_levels }}) &&
        !is.factor({{ included_levels }})
    ) {
      cli::cli_abort(
        c(
          "{.var included_levels} must be of class {.cls character}, {.cls factor}, or {.cls numeric}.",
          "i" = "{.var included_levels} was an object of class {.cls {class({{ included_levels }})}}."
        )
      )
    }

    # define excluded facilities
    exclude_facility_list <- exclude_facility_list

    # Validate optional excluded facility list
    if (
      !is.null(exclude_facility_list) &&
        !is.character(exclude_facility_list) &&
        !is.numeric(exclude_facility_list) &&
        !is.factor(exclude_facility_list)
    ) {
      cli::cli_abort(
        c(
          "{.var exclude_facility_list} must be character, numeric, or factor if provided.",
          "i" = "{.var exclude_facility_list} was of class {.cls {class(exclude_facility_list)}}."
        )
      )
    }

    ###_________________________________________________________________________
    ### Calculations
    ###_________________________________________________________________________

    seqic_12 <- df |>
      # Filter for designated trauma centers only (Levels defined by user)
      dplyr::filter({{ level }} %in% {{ included_levels }}) |>

      # Exclude any user-specified facilities from the analysis
      dplyr::filter(!{{ facility_id }} %in% exclude_facility_list) |>

      # Deduplicate based on incident ID
      dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>

      # Compute numerator, denominator, and proportion of timely records
      dplyr::summarize(
        numerator_12 = sum(
          {{ data_entry_time }} <= data_entry_standard,
          na.rm = TRUE
        ),
        denominator_12 = dplyr::n(),
        seqic_12 = dplyr::if_else(
          denominator_12 > 0,
          numerator_12 / denominator_12,
          NA_real_
        ),
        .by = {{ groups }}
      )

    # Optionally compute confidence intervals using chosen method
    if (!is.null(calculate_ci)) {
      seqic_12 <- seqic_12 |>
        dplyr::bind_cols(
          nemsqar::nemsqa_binomial_confint(
            data = seqic_12,
            x = numerator_12,
            n = denominator_12,
            method = calculate_ci,
            ...
          ) |>
            dplyr::select(lower_ci, upper_ci) |>
            dplyr::rename(lower_ci_12 = lower_ci, upper_ci_12 = upper_ci)
        )
    }

    # Add population label or group if applicable
    if (is.null(groups)) {
      seqic_12 <- seqic_12 |>
        tibble::add_column(Data = "Population/Sample", .before = "numerator_12")
    } else {
      seqic_12 <- seqic_12 |>
        dplyr::arrange(!!!rlang::syms(groups))
    }

    return(seqic_12)
  }
