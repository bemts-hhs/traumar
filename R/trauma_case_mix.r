#' @title View the Current Patient Population Case Mix Compared to the Major
#'   Trauma Study Case Mix
#'
#' @description
#'
#' This function compares the current patient population's case mix (based on
#' probability of survival, Ps) to the MTOS case mix by binning patients into
#' specific Ps ranges. It returns the fraction of patients in each range and
#' compares it to the MTOS distribution. For more information on the methods
#' used in these calculations, please see Flora (1978) and Boyd et al. (1987).
#'
#' @param df A data frame containing patient data.
#' @param Ps_col The name of the column containing the probability of survival
#'   (Ps) values.
#' @param outcome_col The name of the column containing the binary outcome data
#'   (valid values are 1 or TRUE for alive, 0 or FALSE for dead).
#'
#' @details
#'
#' The function checks whether the `outcome_col` contains values representing a
#' binary outcome. It also ensures that `Ps_col` contains numeric values within
#' the range 0 to 1. If any values exceed 1, a warning is issued. The patients
#' are then grouped into predefined Ps ranges, and the function compares the
#' fraction of patients in each range with the MTOS case mix distribution.
#'
#' Like other statistical computing functions, `trauma_case_mix()` is happiest
#' without missing data.  It is best to pass complete probability of survival
#' and outcome data to the function for optimal performance. With smaller
#' datasets, this is especially helpful.  However, `trauma_case_mix()` will
#' throw a warning about missing values, if any exist in `Ps_col` and/or
#' `outcome_col`.
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item \code{Ps_range}: The probability of survival range category.
#'     \item \code{current_fraction}: The fraction of patients in the current
#'     dataset within each Ps range.
#'     \item \code{MTOS_distribution}: The reference distribution of patients in
#'     each Ps range based on the MTOS study.
#'     \item \code{survivals}: The number of observed survivors (outcome = 1) in
#'     each Ps range.
#'     \item \code{predicted_survivals}: The sum of predicted survivals (sum of
#'     Ps values) in each Ps range.
#'     \item \code{deaths}: The number of observed deaths (outcome = 0) in each
#'     Ps range.
#'     \item \code{predicted_deaths}: The sum of predicted deaths (sum of 1 - Ps
#'     values) in each Ps range.
#'     \item \code{count}: The total number of patients in each Ps range.
#'   }
#'
#' @note
#'
#' This function will produce the most reliable and interpretable results when
#' using a dataset that has one row per patient, with each column being a
#' feature.
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
#' # Compare the current case mix with the MTOS case mix
#' trauma_case_mix(data, Ps_col = Ps, outcome_col = death)
#'
#' @export
#'
#' @author Nicolas Foss, Ed.D., MS
#'
trauma_case_mix <- function(df, Ps_col, outcome_col) {
  # Check if the dataframe is valid
  validate_data_structure(
    input = df,
    structure_type = c("data.frame", "tbl", "tbl_df"),
    logic = "or",
    type = "error",
    null_ok = FALSE
  )

  # Ensure Ps_col and outcome_col arguments are provided  ----
  # with tailored error messages
  if (missing(Ps_col) && missing(outcome_col)) {
    cli::cli_abort(
      "Both {.var Ps_col} and {.var outcome_col} arguments must be provided."
    )
  } else if (missing(Ps_col)) {
    cli::cli_abort("The {.var Ps_col} argument must be provided.")
  } else if (missing(outcome_col)) {
    cli::cli_abort("The {.var outcome_col} argument must be provided.")
  }

  # Pull and check the outcome column ----
  binary_data <- validate_data_pull(
    input = df,
    col = {{ outcome_col }},
    var_name = "outcome_col"
  )

  # Ensure the column is either logical or numeric ----
  validate_class(
    input = binary_data,
    class_type = c("numeric", "logical", "integer"),
    logic = "or",
    type = "error",
    var_name = "outcome_col"
  )

  # Get unique non-missing values to use in subsequent data validation ----
  non_missing <- stats::na.omit(binary_data)

  # Validate type and values ----
  if (is.logical(non_missing)) {
    # Logical vector: ensure only TRUE/FALSE (no coercion needed)
    validate_set(
      input = non_missing,
      valid_set = c(TRUE, FALSE),
      type = "error",
      var_name = "outcome_col"
    )
  } else if (is.numeric(non_missing)) {
    # Numeric vector: ensure strictly 0 or 1 ----
    validate_set(
      input = non_missing,
      valid_set = c(0, 1),
      type = "error",
      var_name = "outcome_col"
    )
  } else if (is.integer(non_missing)) {
    # Integer vector: ensure strictly 0 or 1 ----
    validate_set(
      input = non_missing,
      valid_set = c(0L, 1L),
      type = "error",
      var_name = "outcome_col"
    )
  } else {
    # Not logical or numeric ----
    cli::cli_abort(
      "The {.var outcome_col} must be either logical (TRUE/FALSE) or numeric/integer (1/0)."
    )
  }

  # Warn if missing ----
  validate_complete(
    input = binary_data,
    type = "warning",
    var_name = "outcome_col"
  )

  # Check if Ps column is numeric ----
  # dplyr::pull the Ps data
  Ps_check <- validate_data_pull(
    input = df,
    col = {{ Ps_col }},
    var_name = "Ps_col",
    calls = 5
  )

  # check the Ps_check remains continuous ----
  # Check if Ps column is continuous (values between 0 and 1)
  validate_numeric(
    input = Ps_check,
    min = 0,
    max = 1,
    type = "error",
    var_name = "Ps_col"
  )

  # Warn if any missings in Ps_col ----
  validate_complete(input = Ps_check, type = "warning", var_name = "Ps_col")

  # Set the Ps range order for the function
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
    # Mutate the dataframe to add new columns
    dplyr::mutate(
      # Create a new column Ps_range based on the value of Ps_col
      Ps_range = dplyr::case_when(
        {{ Ps_col }} >= 0.96 ~ "0.96 - 1.00", # Ps_col >= 0.96
        {{ Ps_col }} >= 0.91 ~ "0.91 - 0.95", # Ps_col >= 0.91
        {{ Ps_col }} >= 0.76 ~ "0.76 - 0.90", # Ps_col >= 0.76
        {{ Ps_col }} >= 0.51 ~ "0.51 - 0.75", # Ps_col >= 0.51
        {{ Ps_col }} >= 0.26 ~ "0.26 - 0.50", # Ps_col >= 0.26
        TRUE ~ "0.00 - 0.25" # Default case for Ps_col < 0.26
      )
    ) |>
    # Summarize the dataframe to calculate various statistics
    dplyr::summarize(
      # Calculate the number of survivals
      survivals = sum({{ outcome_col }} == 1, na.rm = TRUE),
      # Calculate the predicted number of survivals
      predicted_survivals = sum({{ Ps_col }}, na.rm = TRUE),
      # Calculate the number of deaths
      deaths = sum({{ outcome_col }} == 0, na.rm = TRUE),
      # Calculate the predicted number of deaths
      predicted_deaths = sum(1 - {{ Ps_col }}, na.rm = TRUE),
      # Count the number of rows in each Ps_range
      count = dplyr::n(),
      # Calculate the current fraction of each Ps_range
      current_fraction = dplyr::n() / nrow(df),
      # Group by Ps_range
      .by = Ps_range
    ) |>
    # Join the summarized dataframe with MTOS_distribution by Ps_range
    dplyr::left_join(MTOS_distribution, by = dplyr::join_by(Ps_range)) |>
    # Arrange the dataframe by Ps_range
    dplyr::arrange(Ps_range) |>
    # Relocate the current_fraction column to be after Ps_range
    dplyr::relocate(current_fraction, .after = Ps_range) |>
    # Relocate the MTOS_distribution column to be after current_fraction
    dplyr::relocate(MTOS_distribution, .after = current_fraction) |>
    tibble::as_tibble()

  # Return the result as a tibble
  return(fractions_set)
}
