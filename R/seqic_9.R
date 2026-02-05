#' @title SEQIC Indicator 9 - Emergency Department Transfer Timeliness
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates the proportion of EMS-transferred trauma patients who experienced
#' delayed transfer from the emergency department (ED) based on disposition and
#' decision-to-transfer time frames. This includes both overall rates and
#' stratified results by trauma team activation status, with optional confidence
#' intervals.
#'
#' @inheritParams seqic_indicator_1
#' @inheritParams seqic_indicator_4
#' @inheritParams seqic_indicator_5
#' @inheritParams seqic_indicator_6
#' @inheritParams seqic_indicator_8
#'
#' @param transport_method Column identifying the EMS transport method (e.g.,
#'   ambulance, private vehicle). Used to exclude non-qualified modes of
#'   arrival.
#' @param trauma_team_activated Column indicating whether the trauma team was
#'   activated (character, factor, or logical).
#' @param ed_decision_LOS Numeric column representing minutes from ED arrival to
#'   decision to transfer.
#' @param ed_decision_discharge_LOS Numeric column representing minutes from ED
#'   decision to discharge to physical discharge.
#'
#' @inheritDotParams nemsqar::nemsqa_binomial_confint conf.level correct
#'
#' @details
#'
#'  This function:
#' \itemize{
#'   \item Filters the dataset to include only transfers out from trauma centers
#'   designated Level I through IV.
#'   \item Deduplicates records using `unique_incident_id`.
#'   \item Flags records where emergency department decision to discharge
#'   occurred more than 60 or 120 minutes after ED arrival.
#'   \item Flags records where physical departure from the ED occurred more than
#'   120 or 180 minutes after ED arrival.
#'   \item Flags records where physical discharge occurred more than 60 or 120
#'   minutes after ED decision to discharge.
#'   \item Stratifies results by trauma team activation status and
#'   one or more grouping variables.
#'   \item Stratifies results by risk groups and one or more grouping variables.
#'   \item Returns a summarized tibble with the number of delayed cases
#'   (numerator), eligible records (denominator), and the proportion for each
#'   delay threshold.
#'   \item Optionally includes 95% confidence intervals if `calculate_ci =
#'   TRUE`.
#' }
#'
#' @note
#'
#' This function calculates discharge timeliness outcomes for patients
#' transported to trauma centers, stratified by risk of mortality. Risk
#' groups—low, moderate, and high— are defined by the Iowa System Evaluation and
#' Quality Improvement Committee (SEQIC) as described below. Users may also
#' apply alternative risk stratification methods if preferred.
#'
#' \itemize{
#'   \item Abnormal Physiology Criteria: GCS 3–5; Respirations <5 or >30 per
#'   minute; Systolic BP <60 mm Hg
#'   \item High Risk: Probability of Survival < 0.2; ISS > 41; ISS > 24 with
#'   abnormal physiology
#'   \item Moderate Risk: Probability of Survival 0.2–0.5; ISS 16–41
#'   \item Low Risk: Probability of Survival > 0.5; ISS < 16; Normal physiology
#' }
#'
#' Users must ensure appropriate column names are passed and data is
#' pre-processed to include the necessary fields without missing critical
#' identifiers or timestamps.
#'
#' @returns
#' A list of four tibbles, with optional 95% confidence intervals:
#' \itemize{
#'   \item{`seqic_9_all`}: Proportion of transferred trauma patients with ED
#'   discharge or decision delays >2 or >3 hours, grouped by optional
#'   variables.
#'   \item{`seqic_9_activations`}: Same proportions as above, further stratified
#'   by trauma team activation status.
#'   \item{`seqic_9_risk`}: Same proportions as above, further stratified by
#'   risk groups.
#'   \item{`seqic_9_activations_risk`}: Same proportions as above, further
#'   stratified by risk groups and trauma team activation status.
#' }
#'
#' Each tibble includes numerators, denominators, proportions, and (optionally)
#' confidence intervals for:
#' \itemize{
#'   \item{9a}: Delayed discharge >2 hours
#'   \item{9b}: Delayed discharge >3 hours
#'   \item{9c}: Delayed decision >1 hours
#'   \item{9d}: Delayed decision >2 hours
#'   \item{9e}: Delayed decision to discharge >1 hour
#'   \item{9f}: Delayed decision to discharge >2 hours
#' }
#'
#' @examples
#' # Packages
#' library(dplyr)
#' library(traumar)
#'
#' # Simulated dataset for SEQIC Indicator 9
#' test_data <- tibble::tibble(
#'   id = as.character(1:10),
#'   trauma_level = c("I", "II", "III", "IV", "V", "II", "III", "IV", "I",
#'   "II"),
#'   transport = c("Ambulance", "Ambulance", "Private Vehicle", "Ambulance",
#'   "Helicopter", "Ambulance", "Ambulance", "Ambulance", "Ambulance",
#'   "Ambulance"),
#'   activated = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,
#'   FALSE),
#'   ed_LOS = c(120, 180, 90, 60, 200, 130, 110, 160, 95, 220),
#'   ed_decision = c(55, 125, 65, 30, 190, 80, 70, 45, 61, 130),
#'   ed_discharge = c(130, 185, 110, 65, 150, 160, 95, 180, 70, 210),
#'   transfer_out = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
#'   TRUE),
#'   risk = c("High", "High", "Moderate", "Low", "Moderate", "Low",
#'            "High", "Low", "Moderate", "High")
#' )
#'
#' # Run the function, and store as a list object
#' seqic_9_result <- traumar::seqic_indicator_9(
#'   data = test_data,
#'   level = trauma_level,
#'   included_levels = c("I", "II", "III", "IV"),
#'   unique_incident_id = id,
#'   transport_method = transport,
#'   transfer_out_indicator = transfer_out,
#'   ed_LOS = ed_LOS,
#'   ed_decision_LOS = ed_decision,
#'   ed_decision_discharge_LOS = ed_discharge,
#'   trauma_team_activated = activated,
#'   risk_group = risk
#' )
#'
#' # Take a look at the overall output of the function
#' seqic_9_result$overall |>
#' tidyr::pivot_longer(cols = -1,
#'                     names_to = "Indicator",
#'                     values_to = "Values"
#'                     )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seqic_indicator_9 <- function(
  data,
  level,
  included_levels = c("I", "II", "III", "IV"),
  transfer_out_indicator,
  transport_method,
  unique_incident_id,
  trauma_team_activated,
  risk_group,
  ed_LOS,
  ed_decision_LOS,
  ed_decision_discharge_LOS,
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

  # Ensure that `transport_method` can be validated ----
  transport_method_check <- validate_data_pull(
    input = data,
    col = {{ transport_method }},
    type = "error",
    var_name = "transport_method"
  )

  # Validate that `transport_method` is character or factor ----
  validate_character_factor(
    input = transport_method_check,
    type = "error",
    var_name = "transport_method"
  )

  # Ensure that `ed_LOS` can be validated ----
  ed_los_check <- validate_data_pull(
    input = data,
    col = {{ ed_LOS }},
    type = "error",
    var_name = "ed_LOS"
  )

  # Validate `ed_LOS` ----
  validate_numeric(input = ed_los_check, type = "error", var_name = "ed_LOS")

  # Ensure that `ed_decision_LOS` can be validated ----
  ed_decision_los_check <- validate_data_pull(
    input = data,
    col = {{ ed_decision_LOS }},
    type = "error",
    var_name = "ed_decision_LOS"
  )

  # Validate `ed_decision_LOS` ----
  validate_numeric(
    input = ed_decision_los_check,
    type = "error",
    var_name = "ed_decision_LOS"
  )

  # Ensure that `ed_decision_discharge_LOS` can be validated ----
  ed_decision_discharge_los_check <- validate_data_pull(
    input = data,
    col = {{ ed_decision_discharge_LOS }},
    type = "error",
    var_name = "ed_decision_discharge_LOS"
  )

  # Validate `ed_decision_discharge_LOS`
  validate_numeric(
    input = ed_decision_discharge_los_check,
    type = "error",
    var_name = "ed_decision_discharge_LOS"
  )

  #
  trauma_team_activated_check <- validate_data_pull(
    input = data,
    col = {{ trauma_team_activated }},
    type = "error",
    var_name = "trauma_team_activated"
  )

  # Validate that `trauma_team_activated` is character, factor, or logical ----
  validate_class(
    input = trauma_team_activated_check,
    class_type = c("character", "factor", "logical"),
    type = "error",
    var_name = "trauma_team_activated"
  )

  # Ensure that `risk_group` can be validated
  risk_group_check <- validate_data_pull(
    input = data,
    col = {{ risk_group }},
    type = "error",
    var_name = "risk_group"
  )

  # Validate the `risk_group` column
  validate_character_factor(
    input = risk_group_check,
    type = "error",
    var_name = "risk_group"
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

  # Get vector of transport methods as strings that will not be included ----
  excluded_transport_methods <- paste(
    "private vehicle",
    "public vehicle",
    "walk[\\s-]in",
    "not\\s(?:known|recorded|applicable)",
    "other",
    sep = "|"
  )

  # Create a regex from `excluded_transport_methods` ----
  excluded_transport_methods_regex <- paste0(
    "(?:",
    excluded_transport_methods,
    ")"
  )

  # Initiate the output list
  seqic_9 <- list()

  ###___________________________________________________________________________
  ### Calculations ----
  ###___________________________________________________________________________

  # Get `data` with manipulations ----
  data_prep <- data |>
    dplyr::filter(
      {{ level }} %in% included_levels,
      {{ transfer_out_indicator }} %in% c("Yes", TRUE),
      !grepl(
        pattern = excluded_transport_methods_regex,
        x = {{ transport_method }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::mutate(
      Delayed_DC_2hr = {{ ed_LOS }} > 120,
      Delayed_DC_3hr = {{ ed_LOS }} > 180,
      Delayed_Decision_1hr = {{ ed_decision_LOS }} > 60,
      Delayed_Decision_2hr = {{ ed_decision_LOS }} > 120,
      Delayed_Decision_Discharge_1hr = {{ ed_decision_discharge_LOS }} > 60,
      Delayed_Decision_Discharge_2hr = {{ ed_decision_discharge_LOS }} > 120
    )

  ###___________________________________________________________________________
  ### Overall ----
  ###___________________________________________________________________________

  # 9a-f overall ----
  seqic_9_all <- data_prep |>
    dplyr::summarize(
      numerator_9a_all = sum(Delayed_DC_2hr == TRUE, na.rm = TRUE),
      denominator_9a_all = dplyr::n(),
      seqic_9a_all = dplyr::if_else(
        denominator_9a_all > 0,
        numerator_9a_all / denominator_9a_all,
        NA_real_
      ),
      numerator_9b_all = sum(Delayed_DC_3hr == TRUE, na.rm = TRUE),
      denominator_9b_all = dplyr::n(),
      seqic_9b_all = dplyr::if_else(
        denominator_9b_all > 0,
        numerator_9b_all / denominator_9b_all,
        NA_real_
      ),
      numerator_9c_all = sum(Delayed_Decision_1hr == TRUE, na.rm = TRUE),
      denominator_9c_all = dplyr::n(),
      seqic_9c_all = dplyr::if_else(
        denominator_9c_all > 0,
        numerator_9c_all / denominator_9c_all,
        NA_real_
      ),
      numerator_9d_all = sum(Delayed_Decision_2hr == TRUE, na.rm = TRUE),
      denominator_9d_all = dplyr::n(),
      seqic_9d_all = dplyr::if_else(
        denominator_9d_all > 0,
        numerator_9d_all / denominator_9d_all,
        NA_real_
      ),
      numerator_9e_all = sum(
        Delayed_Decision_Discharge_1hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9e_all = dplyr::n(),
      seqic_9e_all = dplyr::if_else(
        denominator_9e_all > 0,
        numerator_9e_all / denominator_9e_all,
        NA_real_
      ),
      numerator_9f_all = sum(
        Delayed_Decision_Discharge_2hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9f_all = dplyr::n(),
      seqic_9f_all = dplyr::if_else(
        denominator_9f_all > 0,
        numerator_9f_all / denominator_9f_all,
        NA_real_
      ),
      .by = {{ groups }}
    )

  ###___________________________________________________________________________
  ### Activations ----
  ###___________________________________________________________________________

  # 9a-f for activations ----
  seqic_9_activations <- data_prep |>
    dplyr::summarize(
      numerator_9a_activations = sum(Delayed_DC_2hr == TRUE, na.rm = TRUE),
      denominator_9a_activations = dplyr::n(),
      seqic_9a_activations = dplyr::if_else(
        denominator_9a_activations > 0,
        numerator_9a_activations / denominator_9a_activations,
        NA_real_
      ),
      numerator_9b_activations = sum(Delayed_DC_3hr == TRUE, na.rm = TRUE),
      denominator_9b_activations = dplyr::n(),
      seqic_9b_activations = dplyr::if_else(
        denominator_9b_activations > 0,
        numerator_9b_activations / denominator_9b_activations,
        NA_real_
      ),
      numerator_9c_activations = sum(
        Delayed_Decision_1hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9c_activations = dplyr::n(),
      seqic_9c_activations = dplyr::if_else(
        denominator_9c_activations > 0,
        numerator_9c_activations / denominator_9c_activations,
        NA_real_
      ),
      numerator_9d_activations = sum(
        Delayed_Decision_2hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9d_activations = dplyr::n(),
      seqic_9d_activations = dplyr::if_else(
        denominator_9d_activations > 0,
        numerator_9d_activations / denominator_9d_activations,
        NA_real_
      ),
      numerator_9e_activations = sum(
        Delayed_Decision_Discharge_1hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9e_activations = dplyr::n(),
      seqic_9e_activations = dplyr::if_else(
        denominator_9e_activations > 0,
        numerator_9e_activations / denominator_9e_activations,
        NA_real_
      ),
      numerator_9f_activations = sum(
        Delayed_Decision_Discharge_2hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9f_activations = dplyr::n(),
      seqic_9f_activations = dplyr::if_else(
        denominator_9f_activations > 0,
        numerator_9f_activations / denominator_9f_activations,
        NA_real_
      ),
      .by = c({{ groups }}, {{ trauma_team_activated }})
    )

  ###___________________________________________________________________________
  ### Risk Groups ----
  ###___________________________________________________________________________

  # 9a-f for risk groups ----
  seqic_9_risk <- data_prep |>
    dplyr::summarize(
      numerator_9a_risk = sum(Delayed_DC_2hr == TRUE, na.rm = TRUE),
      denominator_9a_risk = dplyr::n(),
      seqic_9a_risk = dplyr::if_else(
        denominator_9a_risk > 0,
        numerator_9a_risk / denominator_9a_risk,
        NA_real_
      ),
      numerator_9b_risk = sum(Delayed_DC_3hr == TRUE, na.rm = TRUE),
      denominator_9b_risk = dplyr::n(),
      seqic_9b_risk = dplyr::if_else(
        denominator_9b_risk > 0,
        numerator_9b_risk / denominator_9b_risk,
        NA_real_
      ),
      numerator_9c_risk = sum(
        Delayed_Decision_1hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9c_risk = dplyr::n(),
      seqic_9c_risk = dplyr::if_else(
        denominator_9c_risk > 0,
        numerator_9c_risk / denominator_9c_risk,
        NA_real_
      ),
      numerator_9d_risk = sum(
        Delayed_Decision_2hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9d_risk = dplyr::n(),
      seqic_9d_risk = dplyr::if_else(
        denominator_9d_risk > 0,
        numerator_9d_risk / denominator_9d_risk,
        NA_real_
      ),
      numerator_9e_risk = sum(
        Delayed_Decision_Discharge_1hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9e_risk = dplyr::n(),
      seqic_9e_risk = dplyr::if_else(
        denominator_9e_risk > 0,
        numerator_9e_risk / denominator_9e_risk,
        NA_real_
      ),
      numerator_9f_risk = sum(
        Delayed_Decision_Discharge_2hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9f_risk = dplyr::n(),
      seqic_9f_risk = dplyr::if_else(
        denominator_9f_risk > 0,
        numerator_9f_risk / denominator_9f_risk,
        NA_real_
      ),
      .by = c({{ groups }}, {{ risk_group }})
    )

  ###___________________________________________________________________________
  ### Activations and Risk Groups ----
  ###___________________________________________________________________________

  # 9a-f for risk groups and trauma team activations ----
  seqic_9_activations_risk <- data_prep |>
    dplyr::summarize(
      numerator_9a_activations_risk = sum(Delayed_DC_2hr == TRUE, na.rm = TRUE),
      denominator_9a_activations_risk = dplyr::n(),
      seqic_9a_activations_risk = dplyr::if_else(
        denominator_9a_activations_risk > 0,
        numerator_9a_activations_risk / denominator_9a_activations_risk,
        NA_real_
      ),
      numerator_9b_activations_risk = sum(Delayed_DC_3hr == TRUE, na.rm = TRUE),
      denominator_9b_activations_risk = dplyr::n(),
      seqic_9b_activations_risk = dplyr::if_else(
        denominator_9b_activations_risk > 0,
        numerator_9b_activations_risk / denominator_9b_activations_risk,
        NA_real_
      ),
      numerator_9c_activations_risk = sum(
        Delayed_Decision_1hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9c_activations_risk = dplyr::n(),
      seqic_9c_activations_risk = dplyr::if_else(
        denominator_9c_activations_risk > 0,
        numerator_9c_activations_risk / denominator_9c_activations_risk,
        NA_real_
      ),
      numerator_9d_activations_risk = sum(
        Delayed_Decision_2hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9d_activations_risk = dplyr::n(),
      seqic_9d_activations_risk = dplyr::if_else(
        denominator_9d_activations_risk > 0,
        numerator_9d_activations_risk / denominator_9d_activations_risk,
        NA_real_
      ),
      numerator_9e_activations_risk = sum(
        Delayed_Decision_Discharge_1hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9e_activations_risk = dplyr::n(),
      seqic_9e_activations_risk = dplyr::if_else(
        denominator_9e_activations_risk > 0,
        numerator_9e_activations_risk / denominator_9e_activations_risk,
        NA_real_
      ),
      numerator_9f_activations_risk = sum(
        Delayed_Decision_Discharge_2hr == TRUE,
        na.rm = TRUE
      ),
      denominator_9f_activations_risk = dplyr::n(),
      seqic_9f_activations_risk = dplyr::if_else(
        denominator_9f_activations_risk > 0,
        numerator_9f_activations_risk / denominator_9f_activations_risk,
        NA_real_
      ),
      .by = c({{ groups }}, {{ trauma_team_activated }}, {{ risk_group }})
    )

  ###___________________________________________________________________________
  ### Optional 95% CIs ----
  ###___________________________________________________________________________

  # Compute confidence intervals if requested ----
  if (!is.null(calculate_ci)) {
    # Overall CIs
    seqic_9_all <- seqic_9_all |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_all,
          x = numerator_9a_all,
          n = denominator_9a_all,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(lower_ci_9a_all = lower_ci, upper_ci_9a_all = upper_ci),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_all,
          x = numerator_9b_all,
          n = denominator_9b_all,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9b_all = lower_ci,
            upper_ci_9b_all = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_all,
          x = numerator_9c_all,
          n = denominator_9c_all,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9c_all = lower_ci,
            upper_ci_9c_all = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_all,
          x = numerator_9d_all,
          n = denominator_9d_all,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9d_all = lower_ci,
            upper_ci_9d_all = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_all,
          x = numerator_9e_all,
          n = denominator_9e_all,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9e_all = lower_ci,
            upper_ci_9e_all = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_all,
          x = numerator_9f_all,
          n = denominator_9f_all,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9f_all = lower_ci,
            upper_ci_9f_all = upper_ci
          )
      ) |>
      dplyr::relocate(lower_ci_9a_all, .after = seqic_9a_all) |>
      dplyr::relocate(upper_ci_9a_all, .after = lower_ci_9a_all) |>
      dplyr::relocate(lower_ci_9b_all, .after = seqic_9b_all) |>
      dplyr::relocate(upper_ci_9b_all, .after = lower_ci_9b_all) |>
      dplyr::relocate(lower_ci_9c_all, .after = seqic_9c_all) |>
      dplyr::relocate(upper_ci_9c_all, .after = lower_ci_9c_all) |>
      dplyr::relocate(lower_ci_9d_all, .after = seqic_9d_all) |>
      dplyr::relocate(upper_ci_9d_all, .after = lower_ci_9d_all) |>
      dplyr::relocate(lower_ci_9e_all, .after = seqic_9e_all) |>
      dplyr::relocate(upper_ci_9e_all, .after = lower_ci_9e_all) |>
      dplyr::relocate(lower_ci_9f_all, .after = seqic_9f_all) |>
      dplyr::relocate(upper_ci_9f_all, .after = lower_ci_9f_all)

    # Activations CIs ----
    seqic_9_activations <- seqic_9_activations |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations,
          x = numerator_9a_activations,
          n = denominator_9a_activations,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9a_activations = lower_ci,
            upper_ci_9a_activations = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations,
          x = numerator_9b_activations,
          n = denominator_9b_activations,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9b_activations = lower_ci,
            upper_ci_9b_activations = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations,
          x = numerator_9c_activations,
          n = denominator_9c_activations,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9c_activations = lower_ci,
            upper_ci_9c_activations = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations,
          x = numerator_9d_activations,
          n = denominator_9d_activations,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9d_activations = lower_ci,
            upper_ci_9d_activations = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations,
          x = numerator_9e_activations,
          n = denominator_9e_activations,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9e_activations = lower_ci,
            upper_ci_9e_activations = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations,
          x = numerator_9f_activations,
          n = denominator_9f_activations,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9f_activations = lower_ci,
            upper_ci_9f_activations = upper_ci
          )
      ) |>
      dplyr::relocate(
        lower_ci_9a_activations,
        .after = seqic_9a_activations
      ) |>
      dplyr::relocate(
        upper_ci_9a_activations,
        .after = lower_ci_9a_activations
      ) |>
      dplyr::relocate(
        lower_ci_9b_activations,
        .after = seqic_9b_activations
      ) |>
      dplyr::relocate(
        upper_ci_9b_activations,
        .after = lower_ci_9b_activations
      ) |>
      dplyr::relocate(
        lower_ci_9c_activations,
        .after = seqic_9c_activations
      ) |>
      dplyr::relocate(
        upper_ci_9c_activations,
        .after = lower_ci_9c_activations
      ) |>
      dplyr::relocate(
        lower_ci_9d_activations,
        .after = seqic_9d_activations
      ) |>
      dplyr::relocate(
        upper_ci_9d_activations,
        .after = lower_ci_9d_activations
      ) |>
      dplyr::relocate(
        lower_ci_9e_activations,
        .after = seqic_9e_activations
      ) |>
      dplyr::relocate(
        upper_ci_9e_activations,
        .after = lower_ci_9e_activations
      ) |>
      dplyr::relocate(
        lower_ci_9f_activations,
        .after = seqic_9f_activations
      ) |>
      dplyr::relocate(
        upper_ci_9f_activations,
        .after = lower_ci_9f_activations
      )

    # Risk Groups CIs ----
    seqic_9_risk <- seqic_9_risk |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_risk,
          x = numerator_9a_risk,
          n = denominator_9a_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9a_risk = lower_ci,
            upper_ci_9a_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_risk,
          x = numerator_9b_risk,
          n = denominator_9b_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9b_risk = lower_ci,
            upper_ci_9b_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_risk,
          x = numerator_9c_risk,
          n = denominator_9c_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9c_risk = lower_ci,
            upper_ci_9c_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_risk,
          x = numerator_9d_risk,
          n = denominator_9d_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9d_risk = lower_ci,
            upper_ci_9d_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_risk,
          x = numerator_9e_risk,
          n = denominator_9e_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9e_risk = lower_ci,
            upper_ci_9e_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_risk,
          x = numerator_9f_risk,
          n = denominator_9f_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9f_risk = lower_ci,
            upper_ci_9f_risk = upper_ci
          )
      ) |>
      dplyr::relocate(
        lower_ci_9a_risk,
        .after = seqic_9a_risk
      ) |>
      dplyr::relocate(
        upper_ci_9a_risk,
        .after = lower_ci_9a_risk
      ) |>
      dplyr::relocate(
        lower_ci_9b_risk,
        .after = seqic_9b_risk
      ) |>
      dplyr::relocate(
        upper_ci_9b_risk,
        .after = lower_ci_9b_risk
      ) |>
      dplyr::relocate(
        lower_ci_9c_risk,
        .after = seqic_9c_risk
      ) |>
      dplyr::relocate(
        upper_ci_9c_risk,
        .after = lower_ci_9c_risk
      ) |>
      dplyr::relocate(
        lower_ci_9d_risk,
        .after = seqic_9d_risk
      ) |>
      dplyr::relocate(
        upper_ci_9d_risk,
        .after = lower_ci_9d_risk
      ) |>
      dplyr::relocate(
        lower_ci_9e_risk,
        .after = seqic_9e_risk
      ) |>
      dplyr::relocate(
        upper_ci_9e_risk,
        .after = lower_ci_9e_risk
      ) |>
      dplyr::relocate(
        lower_ci_9f_risk,
        .after = seqic_9f_risk
      ) |>
      dplyr::relocate(
        upper_ci_9f_risk,
        .after = lower_ci_9f_risk
      )

    # Activations and Risk Groups CIs ----
    seqic_9_activations_risk <- seqic_9_activations_risk |>
      dplyr::bind_cols(
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations_risk,
          x = numerator_9a_activations_risk,
          n = denominator_9a_activations_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9a_activations_risk = lower_ci,
            upper_ci_9a_activations_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations_risk,
          x = numerator_9b_activations_risk,
          n = denominator_9b_activations_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9b_activations_risk = lower_ci,
            upper_ci_9b_activations_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations_risk,
          x = numerator_9c_activations_risk,
          n = denominator_9c_activations_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9c_activations_risk = lower_ci,
            upper_ci_9c_activations_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations_risk,
          x = numerator_9d_activations_risk,
          n = denominator_9d_activations_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9d_activations_risk = lower_ci,
            upper_ci_9d_activations_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations_risk,
          x = numerator_9e_activations_risk,
          n = denominator_9e_activations_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9e_activations_risk = lower_ci,
            upper_ci_9e_activations_risk = upper_ci
          ),
        nemsqar::nemsqa_binomial_confint(
          data = seqic_9_activations_risk,
          x = numerator_9f_activations_risk,
          n = denominator_9f_activations_risk,
          method = calculate_ci,
          ...
        ) |>
          dplyr::select(lower_ci, upper_ci) |>
          dplyr::rename(
            lower_ci_9f_activations_risk = lower_ci,
            upper_ci_9f_activations_risk = upper_ci
          )
      ) |>
      dplyr::relocate(
        lower_ci_9a_activations_risk,
        .after = seqic_9a_activations_risk
      ) |>
      dplyr::relocate(
        upper_ci_9a_activations_risk,
        .after = lower_ci_9a_activations_risk
      ) |>
      dplyr::relocate(
        lower_ci_9b_activations_risk,
        .after = seqic_9b_activations_risk
      ) |>
      dplyr::relocate(
        upper_ci_9b_activations_risk,
        .after = lower_ci_9b_activations_risk
      ) |>
      dplyr::relocate(
        lower_ci_9c_activations_risk,
        .after = seqic_9c_activations_risk
      ) |>
      dplyr::relocate(
        upper_ci_9c_activations_risk,
        .after = lower_ci_9c_activations_risk
      ) |>
      dplyr::relocate(
        lower_ci_9d_activations_risk,
        .after = seqic_9d_activations_risk
      ) |>
      dplyr::relocate(
        upper_ci_9d_activations_risk,
        .after = lower_ci_9d_activations_risk
      ) |>
      dplyr::relocate(
        lower_ci_9e_activations_risk,
        .after = seqic_9e_activations_risk
      ) |>
      dplyr::relocate(
        upper_ci_9e_activations_risk,
        .after = lower_ci_9e_activations_risk
      ) |>
      dplyr::relocate(
        lower_ci_9f_activations_risk,
        .after = seqic_9f_activations_risk
      ) |>
      dplyr::relocate(
        upper_ci_9f_activations_risk,
        .after = lower_ci_9f_activations_risk
      )
  }

  # Label output or arrange by grouping vars <-
  if (is.null(groups)) {
    seqic_9$overall <- seqic_9_all |>
      tibble::add_column(data = "population/sample", .before = 1)
    seqic_9$activations <- seqic_9_activations |>
      tibble::add_column(data = "population/sample TTA groups", .before = 1) |>
      dplyr::arrange({{ trauma_team_activated }})
    seqic_9$risk_groups <- seqic_9_risk |>
      tibble::add_column(data = "population/sample risk groups", .before = 1) |>
      dplyr::arrange({{ risk_group }})
    seqic_9$activations_risk_groups <- seqic_9_activations_risk |>
      tibble::add_column(
        data = "population/sample TTA and risk groups",
        .before = 1
      ) |>
      dplyr::arrange(
        {{ trauma_team_activated }},
        {{ risk_group }}
      )
  } else {
    seqic_9$overall <- seqic_9_all |>
      dplyr::arrange(!!!rlang::syms(groups))
    seqic_9$activations <- seqic_9_activations |>
      dplyr::arrange(!!!rlang::syms(groups), {{ trauma_team_activated }})
    seqic_9$risk_groups <- seqic_9_risk |>
      dplyr::arrange(!!!rlang::syms(groups), {{ risk_group }})
    seqic_9$activations_risk_groups <- seqic_9_activations_risk |>
      dplyr::arrange(
        !!!rlang::syms(groups),
        {{ trauma_team_activated }},
        {{ risk_group }}
      )
  }

  # Return the output as a list ----
  return(seqic_9)
}
