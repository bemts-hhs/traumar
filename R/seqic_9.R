#' @title SEQIC Indicator 9 - Emergency Department Transfer Timeliness
#'
#' @description
#' Calculates the proportion of EMS-transferred trauma patients who experienced
#' delayed transfer from the emergency department (ED) based on disposition and
#' decision-to-transfer timeframes. This includes both overall rates and
#' stratified results by trauma team activation status, with optional confidence
#' intervals.
#'
#' @inheritParams seqic_indicator_1
#' @inheritParams seqic_indicator_5
#' @inheritParams seqic_indicator_6
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
#' @returns
#' A list of two tibbles:
#' \itemize{
#'   \item{`seqic_9_all`}: Proportion of transferred trauma patients with ED
#'   discharge or decision delays >2 or >3 hours, grouped by optional
#'   variables.
#'   \item{`seqic_9_activations`}: Same proportions as above, further stratified
#'   by trauma team activation status.
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
#' @details This function:
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
#'   \item Returns a summarized tibble with the number of delayed cases
#'   (numerator), eligible records (denominator), and the proportion for each
#'   delay threshold.
#'   \item Optionally includes 95% confidence intervals if `calculate_ci = TRUE`.
#' }
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export

seqic_indicator_9 <- function(
  df,
  level,
  transfer_out_indicator,
  transport_method,
  unique_incident_id,
  trauma_team_activated,
  ed_LOS,
  ed_decision_LOS,
  ed_decision_discharge_LOS,
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

  # Validate the `level` column
  level_check <- df |> dplyr::pull({{ level }})
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(c(
      "{.var level} must be character or factor.",
      "i" = "Provided class: {.cls {class(level_check)}}."
    ))
  }

  # Validate the `unique_incident_id` column
  incident_id_check <- df |> dplyr::pull({{ unique_incident_id }})
  if (!is.character(incident_id_check) && !is.factor(incident_id_check)) {
    cli::cli_abort(c(
      "{.var unique_incident_id} must be character or factor.",
      "i" = "Provided class: {.cls {class(incident_id_check)}}."
    ))
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

  # Validate `ed_LOS`
  ed_los_check <- df |> dplyr::pull({{ ed_LOS }})
  if (!is.numeric(ed_los_check)) {
    cli::cli_abort(
      c(
        "{.var ed_LOS} must be of class {.cls numeric}.",
        "i" = "{.var ed_LOS} was an object of class {.cls {class(ed_los_check)}}."
      )
    )
  }

  # Validate `ed_decision_LOS`
  ed_decision_los_check <- df |> dplyr::pull({{ ed_decision_LOS }})
  if (!is.numeric(ed_decision_los_check)) {
    cli::cli_abort(
      c(
        "{.var ed_decision_LOS} must be of class {.cls numeric}.",
        "i" = "{.var ed_decision_LOS} was an object of class {.cls {class(ed_decision_los_check)}}."
      )
    )
  }

  # Validate `ed_decision_discharge_LOS`
  ed_decision_discharge_los_check <- df |>
    dplyr::pull({{ ed_decision_discharge_LOS }})
  if (!is.numeric(ed_decision_discharge_los_check)) {
    cli::cli_abort(
      c(
        "{.var ed_decision_discharge_LOS} must be of class {.cls numeric}.",
        "i" = "{.var ed_decision_discharge_LOS} was an object of class {.cls {class(ed_decision_discharge_los_check)}}."
      )
    )
  }

  # Validate that `trauma_team_activated` is character, factor, or logical.
  trauma_team_activated_check <- df |>
    dplyr::pull({{ trauma_team_activated }})
  if (
    !is.character(trauma_team_activated_check) &&
      !is.factor(trauma_team_activated_check) &&
      !is.logical(trauma_team_activated_check)
  ) {
    cli::cli_abort(
      c(
        "{.var trauma_team_activated} must be of class {.cls character}, {.cls factor}, or {.cls logical}.",
        "i" = "{.var trauma_team_activated} was an object of class {.cls {class(trauma_team_activated_check)}}."
      )
    )
  }

  # Validate `groups` argument
  if (!is.null(groups)) {
    if (!all(sapply(groups, is.character))) {
      cli::cli_abort(c(
        "All elements in {.var groups} must be strings.",
        "i" = "Provided class: {.cls {class(groups)}}."
      ))
    }
    if (!all(groups %in% names(df))) {
      invalid_vars <- groups[!groups %in% names(df)]
      cli::cli_abort(
        "Invalid grouping variable(s): {paste(invalid_vars, collapse = ', ')}"
      )
    }
  }

  # Validate confidence interval method
  if (!is.null(calculate_ci)) {
    attempt <- try(
      match.arg(calculate_ci, choices = c("wilson", "clopper-pearson")),
      silent = TRUE
    )
    if (inherits(attempt, "try-error")) {
      cli::cli_abort(c(
        "If {.var calculate_ci} is not NULL, it must be {.val wilson} or {.val clopper-pearson}.",
        "i" = "Provided value: {.val {calculate_ci}}"
      ))
    }
    calculate_ci <- attempt
  }

  # Get vector of transport methods as strings that will not be included
  excluded_transport_methods <- paste(
    "private vehicle",
    "public vehicle",
    "walk[\\s-]in",
    "not\\s(?:known|recorded|applicable)",
    "other",
    sep = "|"
  )

  # Create a regex from `excluded_transport_methods`
  excluded_transport_methods_regex <- paste0(
    "(?:",
    excluded_transport_methods,
    ")"
  )

  # Initiate the output list
  seqic_9 <- list()

  # Get `df` with manipulations
  df_prep <- df |>
    dplyr::filter(
      {{ level }} %in% c("I", "II", "III", "IV"),
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

  # 9a-b overall
  seqic_9_all <- df_prep |>
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

  # 9a-b for activations
  seqic_9_activations <- df_prep |>
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

  # Compute confidence intervals if requested
  if (!is.null(calculate_ci)) {
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
  }

  # Label output or arrange by grouping vars
  if (is.null(groups)) {
    seqic_9$overall <- seqic_9_all |>
      tibble::add_column(Data = "Population/Sample", .before = 1)
    seqic_9$activations <- seqic_9_activations |>
      tibble::add_column(Data = "Population/Sample TTA Groups", .before = 1)
  } else {
    seqic_9$overall <- seqic_9_all |>
      dplyr::arrange(!!!rlang::syms(groups))
    seqic_9$activations <- seqic_9_activations |>
      dplyr::arrange(!!!rlang::syms(groups))
  }

  return(seqic_9)
}
