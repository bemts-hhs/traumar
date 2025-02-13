#' Calculate Trauma Hospital Performance Based on Robust and Validated Measures
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
#'   (alive), or other similar binary representations (e.g., "Yes" for dead and
#'   "No" for alive). The function will check for two unique values in this
#'   column and expects them to represent the outcome in a binary form.
#'
#' @param outcome The value representing mortality (default is 1). Can also be set
#'   to 0 or TRUE/FALSE, depending on how the outcome is encoded in
#'   `outcome_col`.
#' @param z_method A character vector indicating which method to use for
#'   calculating the Z-score. Must be one of "survival" or "mortality". The
#'   default is "survival".
#' @param diagnostics A logical flag (default is FALSE). If TRUE, diagnostic
#'   information about the W, M, and Z scores will be printed to the console.
#'
#' @return A tibble containing the following calculations:
#'
#' - `N_Patients`: The total number of patients included in the analysis.
#' - `N_Survivors`: The total number of patients who survived, based on the provided outcome data.
#' - `N_Deaths`: The total number of patients who died, based on the provided outcome data.
#' - `Predicted_Survivors`: The total predicted number of survivors based on the
#' survival probability (`Ps`) for all patients.
#' - `Predicted_Deaths`: The total predicted number of deaths, calculated as `1 - Ps` for all patients.
#' - `Patient_Estimate`: The estimated number of patients who survived, calculated based
#' on the W-score. This value reflects the difference between the actual and
#' predicted number of survivors.
#' - `W_Score`: The W-score, representing the difference between the observed and expected
#' number of survivors per 100 patients. A positive W-score indicates that more
#' patients survived than expected, while a negative score indicates that fewer
#' patients survived than expected.
#' - `M_Score`: The M-score, which compares the observed patient case mix to the Major Trauma
#' Outcomes Study (MTOS) case mix. A higher score indicates that the patient mix
#' is more similar to MTOS, while a lower score indicates a dissimilar mix. Based on the MTOS
#' literature, an M_Score >= 0.88 indicates that the Z_Score comes from distribution similar
#' enough to the MTOS Ps distribution.
#' - `Z_Score`: The Z-score, which quantifies the difference between the actual and predicted
#' mortality (if `z_method = "mortality"`) or survival (if `z_method =
#' "survival"`). A Z-score > 1.96 is considered to point to the statistical
#' significance of the W-Score at alpha = 0.05 level for survival. The positive
#' Z_Score indicates that more patients survived than predicted, while a
#' negative Z-score indicates fewer survivors than predicted.
#'
#' @examples
#' # Generate example data with high negative skewness
#' set.seed(123)
#'
#' # Parameters
#' n_patients <- 10000  # Total number of patients
#'
#' # Generate survival probabilities (Ps) using a logistic distribution
#' set.seed(123)  # For reproducibility
#' Ps <- plogis(rnorm(n_patients, mean = 2, sd = 1.5))  # Skewed towards higher values
#'
#' # Simulate survival outcomes based on Ps
#' survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)
#'
#' # Create data frame
#' data <- data.frame(Ps = Ps, survival = survival_outcomes) |>
#' dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
#'
#' # Calculate trauma performance (W, M, Z scores)
#' trauma_performance(data, Ps_col = Ps, outcome_col = death)
#'
#' @export
#'
#' @author Nicolas Foss, Ed.D., MS
#'
trauma_performance <- function(df, Ps_col, outcome_col, outcome = 1, z_method = c("survival", "mortality"), diagnostics = FALSE) {

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

  # Check if the outcome_col is binary
  binary_data <- df |>
    dplyr::select({{ outcome_col }}) |>
    dplyr::pull()

  # Validate binary data
  unique_values <- unique(stats::na.omit(binary_data))

  if (!all(unique_values %in% c(0, 1, TRUE, FALSE), na.rm = T) || length(unique_values) > 2) {
    cli::cli_abort("The {.var outcome_col} must be binary, such as 1/0, TRUE/FALSE, or a combination of these. Ensure the column has a binary structure.")
  }

  # Check if Ps column is numeric

  # dplyr::pull the Ps data
  Ps_data <- df |> dplyr::pull(!!Ps_col)

  # check to ensure Ps_data is numeric
  if (!is.numeric(Ps_data)) {
    cli::cli_abort("The probability of survival (Ps) column must be numeric.")
  }

  # Check if Ps column is continuous (values between 0 and 1 or 0 and 100)
  if (any(Ps_data < 0 | Ps_data > 100, na.rm = T)) {
    cli::cli_abort("The probability of survival (Ps) values must be between 0 and 100.")
  }

  # Notify the user if any conversions were made and manipulate the data if necessary
  if (any(Ps_data > 1, na.rm = T)) {
    cli::cli_alert_info("Some Probability of survival (Ps) values will be divided by 100 to convert to decimal format.")

    # Convert ##.## format to decimal if needed (rowwise operation but vectorized)
    Ps_data <- dplyr::if_else(Ps_data > 1, Ps_data / 100, Ps_data)

    # convert ##.## percentages to 0.### percentages
    df <- df |>
      dplyr::mutate(!!Ps_col := Ps_data)
  }

  ### Initiate calculation of the W-Score

  # Total number of patients
  total_patients <- df |>
    dplyr::filter(!is.na(!!Ps_col) & !is.na(!!outcome_col)) |>
    nrow()

  # get n survivors
  total_survivors <- df |>
    dplyr::filter(!is.na(!!Ps_col) & !is.na(!!outcome_col)) |>
    dplyr::summarize(survivors = sum(!!outcome_col != outcome)) |>
    dplyr::pull(survivors)

  # Number of patients who died
  total_deaths <- df |>
    dplyr::filter(!is.na(!!Ps_col) & !is.na(!!outcome_col) & (!!outcome_col == outcome)) |>
    nrow()

  # Sum of Ps values for the patients
  sum_Ps <- df |>
    dplyr::filter(!is.na(!!Ps_col)) |>
    dplyr::summarize(sum_Ps = sum(!!Ps_col, na.rm = TRUE)) |>
    dplyr::pull(sum_Ps)

  # Calculate W-score
  W_score <- (total_patients - total_deaths - sum_Ps) / (total_patients / 100)

  ### Initiate process to calculate M-score

  Ps_range_order <- c("0.96 - 1.00", "0.91 - 0.95", "0.76 - 0.90", "0.51 - 0.75", "0.26 - 0.50", "0.00 - 0.25")

  # Define the MTOS Ps distribution
  MTOS_distribution <- tibble::tibble(
    Ps_range = factor(c(
      "0.96 - 1.00",
      "0.91 - 0.95",
      "0.76 - 0.90",
      "0.51 - 0.75",
      "0.26 - 0.50",
      "0.00 - 0.25"
    ), levels = Ps_range_order),
    MTOS_distribution = c(0.842, 0.053, 0.052, 0, 0.043, 0.01)
  )

  # Bin patients into Ps ranges and calculate current fractions
  fractions_set <- df |>
    dplyr::filter(!is.na(!!Ps_col) & !is.na(!!outcome_col)) |>
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
    dplyr::filter(!is.na(!!Ps_col) & !is.na(!!outcome_col)) |>
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
        z_score = (total_deaths - sum(prob_death)) / sqrt(sum(predicted_prob_death)) # standard error of
      ) |>
      dplyr::pull(z_score)
  } else if (z_method == "survival") {
    # get Z-Score, studying mortality (positive Z-Score is desired)
    Z_score <- z_data |>
      dplyr::summarize(
        z_score =
          (total_survivors - sum(!!Ps_col)) / sqrt(sum(predicted_prob_death))
      ) |>
      dplyr::pull(z_score)
  }

  # Optionally print diagnostic information related to the W-Score test

  if (diagnostics) {
    # print diagnostic information to the console
    cli::cli_h1("Trauma Program Performance Summary")

    # print critical information about the calculations
    cli::cli_alert_info("The numbers involved in the overall calculations below are:")
    cli::cli_text("{symbol$arrow_right} Total patients = {total_patients}")
    cli::cli_text("{symbol$arrow_right} Total survivors = {total_survivors}")
    cli::cli_text("{symbol$arrow_right} Total deaths = {total_deaths}")
    cli::cli_text("{symbol$arrow_right} Predicted survivors = {round(sum_Ps, digits = 2)}")
    cli::cli_text("{symbol$arrow_right} Predicted deaths = {round(sum(probability_death), digits = 2)}")

    cli::cli_h2("W-Score Information: Trauma Program Performance:")

    # get dynamic diagnostic text W-Score
    if (W_score > 0) {
      cli::cli_alert_success("W-Score was estimated as {.val {round(W_score, digits = 2)}}.")
      cli::cli_alert_info(c("i" = "Relative mortality analysis indicates that for every 100 patients, {.val {abs(round(W_score, digits = 2))}} ", cli::col_blue("more"), " patients survived than were expected."))
      cli::cli_alert_info(c("v" = "The estimated number of patients saved that were ", cli::col_blue("statistically expected to die"), " was {.val {abs(round(W_score * (total_patients / 100), digits = 1))}}."))
    } else if (W_score < 0) {
      cli::cli_alert_warning("W-Score was estimated as {.val {round(W_score, digits = 2)}}.")
      cli::cli_alert_info(c("i" = "Relative mortality analysis indicates that for every 100 patients, {.val {abs(round(W_score, digits = 2))}} ", cli::col_blue("fewer"), " patients survived than were expected."))
      cli::cli_alert_info(c("v" = "The estimated number of patients lost that were ", cli::col_blue("statistically expected to live"), " was {.val {abs(round(W_score * (total_patients / 100), digits = 1))}}."))
    } else {
      cli::cli_alert_danger("W-Score was estimated as {.val {round(W_score, digits = 2)}}.  This result could indicate a problem with the data passed to {.fn trauma_performance}. Please check the data and ensure proper filters are applied and appropriate data types used.")

      cli::cli_alert_info(c("i" = "Relative mortality analysis indicates that for every 100 patients, {.val {abs(round(W_score, digits = 2))}} ", cli::col_blue("more"), " patients survived than were expected."))
    }

    cli::cli_h2("M-Score Information: Current Trauma Population Similarity to the Major Trauma Study Population:")

    # get dynamic diagnostic text M-Score
    if (M_score >= 0.88) {
      cli::cli_alert_success("M-Score was estimated as {.val {round(M_score, digits = 2)}}.")
      cli::cli_alert_info(c("i" = "The patient case mix is considered ", cli::col_green("SIMILAR"), " to the Major Trauma Outcomes Study (MTOS) case mix."))
    } else {
      cli::cli_alert_warning("M-Score was estimated as {.val {round(M_score, digits = 2)}}.")
      cli::cli_alert_info(c("i" = "The patient case mix is considered ", cli::col_red("DISSIMILAR"), " to the MTOS case mix."))
    }

    cli::cli_h2("Z-Score Information: Difference Between Actual and Predicted {str_to_title(z_method)}:")

    # Inference here based on Peitzman et al. (1990)
    if (z_method == "mortality") {
      if (Z_score < 0) {
        cli::cli_alert_success("Z-Score was estimated as {.val {round(Z_score, digits = 2)}}.")
        cli::cli_alert_info(c("i" = "Significantly ", cli::col_green("fewer"), " deaths occurred compared to predicted deaths."))
      } else if (Z_score > 0) {
        cli::cli_alert_warning("Z-Score was estimated as {.val {round(Z_score, digits = 2)}}.")
        cli::cli_alert_info(c("i" = "Significantly ", cli::col_red("more"), " deaths occurred compared to predicted deaths."))
      }
    } else if (z_method == "survival") {
      if (Z_score > 0) {
        cli::cli_alert_success("Z-Score was estimated as {.val {round(Z_score, digits = 2)}}.")
        cli::cli_alert_info(c("i" = "Significantly ", cli::col_green("more"), " patients survived compared to predicted survivors."))
      } else if (Z_score < 0) {
        cli::cli_alert_warning("Z-Score was estimated as {.val {round(Z_score, digits = 2)}}.")
        cli::cli_alert_info(c("i" = "Significantly ", cli::col_red("fewer"), " patients survived compared to predicted survivors."))
      }
    }
  } else {
    # Return the scores as a dplyr::tibble
    dplyr::tibble(
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
  }
}
