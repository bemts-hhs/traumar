#' @title Calculate Trauma Hospital Performance Based on Robust and Validated
#'   Measures
#'
#' @description
#'
#' This function calculates trauma hospital performance based on the M, W, and Z
#' scores, which are derived from survival probability and mortality data, using
#' established methods. It computes the W-score, M-score, and Z-score based on
#' the provided dataset and calculates performance metrics for trauma programs.
#' For more information on the methods used in this function, please see
#' Champion et al. (1990) on the W score, and Flora (1978) and Boyd et al.
#' (1987) on the M and Z scores.
#'
#' @param df A data frame containing patient data.
#' @param Ps_col The name of the column containing the probability of survival
#'   (Ps). The values should be numeric and between 0 and 1. Values greater than
#'   1 will be automatically converted to decimal format by dividing by 100.
#' @param outcome_col The name of the column containing the binary outcome data.
#'
#'   The column should contain binary values indicating the patient outcome.
#'   Valid values include 1 (dead) and 0 (alive), or TRUE (dead) and FALSE
#'   (alive). The function will check values in this column and expects them to
#'   represent the outcome in a binary form.
#'
#' @param z_method A character vector indicating which method to use for
#'   calculating the Z-score. Must be one of "survival" or "mortality". The
#'   default is "survival".
#'
#' @return A tibble containing the following calculations:
#' \itemize{
#' \item `N_Patients`: The total number of patients included in the analysis.
#' \item `N_Survivors`: The total number of patients who survived, based on the
#' provided outcome data.
#' \item `N_Deaths`: The total number of patients who died, based on the
#' provided outcome data.
#' \item `Predicted_Survivors`: The total predicted number of survivors based on
#' the survival probability (`Ps`) for all patients.
#' \item `Predicted_Deaths`: The total predicted number of deaths, calculated as
#' `1 - Ps` for all patients.
#' \item `Patient_Estimate`: The estimated number of patients who survived that
#' were predicted to die, calculated based on the W-score. This value reflects
#' the difference between the actual and predicted number of deceased patients.
#' \item `W_Score`: The W-score, representing the difference between the
#' observed and expected number of survivors per 100 patients. A positive
#' W-score indicates that more patients survived than expected, while a negative
#' score indicates that fewer patients survived than expected.
#' \item `M_Score`: The M-score, which compares the observed patient case mix to
#' the Major Trauma Outcomes Study (MTOS) case mix. A higher score indicates
#' that the patient mix is more similar to MTOS, while a lower score indicates a
#' dissimilar mix. Based on the MTOS literature, an M_Score >= 0.88 indicates
#' that the Z_Score comes from distribution similar enough to the MTOS Ps
#' distribution.
#' \item `Z_Score`: The Z-score, which quantifies the difference between the
#' actual and predicted mortality (if `z_method = "mortality"`) or survival (if
#' `z_method = "survival"`). A Z-score > 1.96 is considered to point to the
#' statistical significance of the W-Score at alpha = 0.05 level for survival.
#' The positive Z_Score indicates that more patients survived than predicted,
#' while a negative Z-score indicates fewer survivors than predicted.
#'}
#'
#' @details
#'
#' The function checks whether the `outcome_col` contains values representing a
#' binary outcome. It also ensures that `Ps_col` contains numeric values within
#' the range 0 to 1. If any values exceed 1, a warning is issued. The patients
#' are then grouped into predefined Ps ranges, and the function compares the
#' fraction of patients in each range with the MTOS case mix distribution.
#'
#' Like other statistical computing functions, `trauma_performance()` is
#' happiest without missing data.  It is best to pass complete probability of
#' survival and outcome data to the function for optimal performance. With
#' smaller datasets, this is especially helpful.  However,
#' `trauma_performance()` will throw a warning about missing values, if any
#' exist in `Ps_col` and/or `outcome_col`.
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#'
#' # Parameters
#' # Total number of patients
#' n_patients <- 5000
#'
#' # Arbitrary group labels
#' groups <- sample(x = LETTERS[1:2], size = n_patients, replace = TRUE)
#'
#' # Trauma types
#' trauma_type_values <- sample(
#'   x = c("Blunt", "Penetrating"),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # RTS values
#' rts_values <- sample(
#'   x = seq(from = 0, to = 7.8408, by = 0.005),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # patient ages
#' ages <- sample(
#'   x = seq(from = 0, to = 100, by = 1),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # ISS scores
#' iss_scores <- sample(
#'   x = seq(from = 0, to = 75, by = 1),
#'   size = n_patients,
#'   replace = TRUE
#' )
#'
#' # Generate survival probabilities (Ps)
#' Ps <- traumar::probability_of_survival(
#'   trauma_type = trauma_type_values,
#'   age = ages,
#'   rts = rts_values,
#'   iss = iss_scores
#' )
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes, groups = groups) |>
#'   dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Calculate trauma performance (W, M, Z scores)
#' trauma_performance(data, Ps_col = Ps, outcome_col = death)
#'
#' @export
#'
#' @author Nicolas Foss, Ed.D., MS
#'
trauma_performance <- function(
  df,
  Ps_col,
  outcome_col,
  z_method = c("survival", "mortality")
) {
  if (length(z_method) > 1) {
    z_method <- "survival"
  }

  # Evaluate column names passed in
  Ps_col <- rlang::enquo(Ps_col)
  outcome_col <- rlang::enquo(outcome_col)

  # Check if the dataframe is valid
  if (!is.data.frame(df)) {
    cli::cli_abort("The first argument must be a dataframe.")
  }

  # Pull and check the outcome column
  binary_data <- df |> dplyr::pull(!!outcome_col)

  # Ensure the column is either logical or numeric
  if (!is.logical(binary_data) && !is.numeric(binary_data)) {
    cli::cli_abort(
      "The {.var outcome_col} must be of type logical (TRUE/FALSE) or numeric (1/0)."
    )
  }

  # Get unique non-missing values
  non_missing <- stats::na.omit(binary_data)

  # Validate type and values
  if (is.logical(binary_data)) {
    # Logical vector: ensure only TRUE/FALSE (no coercion needed)
    invalid_vals <- setdiff(unique(non_missing), c(TRUE, FALSE))
    if (length(invalid_vals) > 0) {
      cli::cli_abort(
        "The {.var outcome_col} contains invalid logical values: {.val {invalid_vals}}."
      )
    }
  } else if (is.numeric(binary_data)) {
    # Numeric vector: ensure strictly 0 or 1
    invalid_vals <- setdiff(unique(non_missing), c(0, 1))
    if (length(invalid_vals) > 0) {
      cli::cli_abort(
        "The {.var outcome_col} contains numeric values other than 0 and 1: {.val {invalid_vals}}."
      )
    }
  } else {
    # Not logical or numeric
    cli::cli_abort(
      "The {.var outcome_col} must be either logical (TRUE/FALSE) or numeric (1/0)."
    )
  }

  # Warn if missing
  if (any(is.na(binary_data))) {
    cli::cli_warn(
      "Missing values detected in {.var outcome_col}; please apply an appropriate treatment to the missings and rerun {.fn trauma_performance}."
    )
  }
  # Check if Ps column is numeric

  # dplyr::pull the Ps data
  Ps_data <- df |> dplyr::pull(!!Ps_col)

  # check to ensure Ps_data is numeric
  if (!is.numeric(Ps_data)) {
    cli::cli_abort("The probability of survival (Ps) column must be numeric.")
  }

  # Check if Ps column is continuous (values between 0 and 1 or 0 and 100)
  if (any(Ps_data < 0 | Ps_data > 1, na.rm = T)) {
    cli::cli_abort(
      "The probability of survival (Ps) values must be between 0 and 1."
    )
  }

  # Check if Ps column is missing
  if (any(is.na(Ps_data))) {
    cli::cli_warn(
      "Missing values detected in {.var Ps_col}; please apply an appropriate treatment to the missings and rerun {.fn trauma_performance}."
    )
  }

  ### Initiate calculation of the W-Score

  # Total number of patients
  total_patients <- df |>
    nrow()

  # get n survivors
  total_survivors <- df |>
    dplyr::summarize(survivors = sum(!!outcome_col == 0)) |>
    dplyr::pull(survivors)

  # Number of patients who died
  total_deaths <- df |>
    dplyr::summarize(
      deaths = sum(!!outcome_col == 1, na.rm = TRUE)
    ) |>
    dplyr::pull(deaths)

  # Sum of Ps values for the patients
  sum_Ps <- df |>
    dplyr::summarize(sum_Ps = sum(!!Ps_col, na.rm = TRUE)) |>
    dplyr::pull(sum_Ps)

  # Calculate W-score
  W_score <- (total_patients - total_deaths - sum_Ps) / (total_patients / 100)

  ### Initiate process to calculate M-score

  Ps_range_order <- c(
    "0.96 - 1.00",
    "0.91 - 0.95",
    "0.76 - 0.90",
    "0.51 - 0.75",
    "0.26 - 0.50",
    "0.00 - 0.25"
  )

  # Define the MTOS Ps distribution
  MTOS_distribution <- tibble::tibble(
    Ps_range = factor(
      c(
        "0.96 - 1.00",
        "0.91 - 0.95",
        "0.76 - 0.90",
        "0.51 - 0.75",
        "0.26 - 0.50",
        "0.00 - 0.25"
      ),
      levels = Ps_range_order
    ),
    MTOS_distribution = c(0.842, 0.053, 0.052, 0, 0.043, 0.01)
  )

  # Bin patients into Ps ranges and calculate current fractions
  fractions_set <- df |>
    dplyr::mutate(
      Ps_range = dplyr::case_when(
        !!Ps_col >= 0.96 ~ "0.96 - 1.00",
        !!Ps_col >= 0.91 ~ "0.91 - 0.95",
        !!Ps_col >= 0.76 ~ "0.76 - 0.90",
        !!Ps_col >= 0.51 ~ "0.51 - 0.75",
        !!Ps_col >= 0.26 ~ "0.26 - 0.50",
        TRUE ~ "0.00 - 0.25"
      )
    ) |>
    dplyr::summarize(
      current_fraction = dplyr::n() / nrow(df),
      .by = Ps_range
    ) |>
    dplyr::left_join(MTOS_distribution, by = "Ps_range") |>
    dplyr::arrange(Ps_range)

  # Take the M-Score
  M_score <- fractions_set |>
    dplyr::mutate(smallest_val = pmin(current_fraction, MTOS_distribution)) |>
    dplyr::summarize(M_score = sum(smallest_val)) |>
    dplyr::pull(M_score)

  ### Initiate process to calculate the Z-Score
  ### from Boyd et al. (1987)

  # get key statistics
  z_data <- df |>
    dplyr::mutate(
      prob_death = 1 - !!Ps_col,
      predicted_prob_death = !!Ps_col * prob_death
    )

  # extract probability of death
  probability_death <- z_data$prob_death

  # extract predicted probability of death
  predicted_probability_death <- z_data$predicted_prob_death

  if (z_method == "mortality") {
    # get Z-Score, studying mortality (negative Z-Score is desired)
    Z_score <- z_data |>
      dplyr::summarize(
        z_score = (total_deaths - sum(prob_death)) /
          sqrt(sum(predicted_prob_death)) # standard error of
      ) |>
      dplyr::pull(z_score)
  } else if (z_method == "survival") {
    # get Z-Score, studying mortality (positive Z-Score is desired)
    Z_score <- z_data |>
      dplyr::summarize(
        z_score = (total_survivors - sum(!!Ps_col)) /
          sqrt(sum(predicted_prob_death))
      ) |>
      dplyr::pull(z_score)
  }

  # Return the scores as a dplyr::tibble
  result <- dplyr::tibble(
    N_Patients = total_patients,
    N_Survivors = total_survivors,
    N_Deaths = total_deaths,
    Predicted_Survivors = sum_Ps,
    Predicted_Deaths = sum(probability_death),
    Patient_Estimate = W_score * (total_patients / 100),
    W_Score = W_score,
    M_Score = M_score,
    Z_Score = Z_score
  ) |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "Calculation_Name",
      values_to = "Value"
    )

  # Return the result
  return(result)
}
