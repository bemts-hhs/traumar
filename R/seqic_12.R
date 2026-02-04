#' @title SEQIC Indicator 12 - Timeliness of Data Entry Post-Discharge
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
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
#' @return A tibble summarizing SEQIC Indicator 12 results. Includes numerator,
#'   denominator, and performance rate. 95% confidence intervals are included if
#'   requested.
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
#'   data = test_data,
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
    data,
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
    ### Data validation ----
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

    # Ensure `facility_id` can be validated ----
    facility_id_check <- validate_data_pull(
      input = data,
      col = {{ facility_id }},
      type = "error",
      var_name = "facility_id"
    )

    # Validate facility ID type ----
    validate_class(
      input = facility_id_check,
      class_type = c("numeric", "character", "factor"),
      type = "error",
      var_name = "facility_id"
    )

    # Ensure `data_entry_time` can be validated ----
    data_entry_time_check <- validate_data_pull(
      input = data,
      col = {{ data_entry_time }},
      type = "error",
      var_name = "data_entry_time"
    )

    # Validate that data_entry_time is numeric ----
    validate_numeric(
      input = data_entry_time_check,
      type = "error",
      var_name = "data_entry_time"
    )

    # Validate the cutoff standard for timeliness ----
    validate_numeric(input = data_entry_standard, type = "error")

    # Check if all elements in groups are strings (i.e., character vectors) ----
    validate_character_factor(input = groups, type = "error", null_ok = TRUE)

    # Check if all `groups` exist in the `data` ----
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

    # Validate optional excluded facility list ----
    validate_class(
      input = exclude_facility_list,
      class_type = c("numeric", "character", "factor"),
      logic = "or",
      type = "error",
      null_ok = TRUE
    )

    ###_________________________________________________________________________
    ### Calculations ----
    ###_________________________________________________________________________

    seqic_12 <- data |>
      # Filter for designated trauma centers only (Levels defined by user)
      dplyr::filter({{ level }} %in% included_levels) |>

      # Exclude any user-specified facilities from the analysis
      dplyr::filter(!{{ facility_id }} %in% exclude_facility_list) |>

      # Deduplicate based on incident ID
      dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>

      # Compute numerator, denominator, and proportion of timely records ----
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

    # Optionally compute confidence intervals using chosen method ----
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

    # Add population label or group if applicable ----
    if (is.null(groups)) {
      seqic_12 <- seqic_12 |>
        tibble::add_column(data = "population/sample", .before = "numerator_12")
    } else {
      seqic_12 <- seqic_12 |>
        dplyr::arrange(!!!rlang::syms(groups))
    }

    return(seqic_12)
  }
