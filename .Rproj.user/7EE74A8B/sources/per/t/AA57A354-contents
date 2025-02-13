#' Calculate Probability of Survival Using TRISS Method
#'
#' This function calculates the probability of survival (Ps) for trauma patients
#' based on the Trauma and Injury Severity Score (TRISS) methodology. TRISS
#' combines physiological and anatomical data to predict survival likelihood
#' using a logistic regression model. The function incorporates trauma type,
#' patient age, Revised Trauma Score (RTS), and Injury Severity Score (ISS) into
#' the calculation. Probability of survival is expressed as a percentage.
#'
#' @param trauma_type Character vector indicating the type of trauma ("Blunt" or
#'   "Penetrating"). Different methods exist for calculating probability of survival
#'   for burn patients, and so these records are excluded here.
#' @param age Numeric vector indicating the patient's age in years.
#' @param rts Numeric vector indicating the patient's Revised Trauma Score
#'   (RTS).
#' @param iss Numeric vector indicating the patient's Injury Severity Score
#'   (ISS).
#'
#' @returns Numeric vector of probabilities of survival (Ps) expressed as
#'   percentages on a scale from 0 to 1.
#'
#' @export
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @examples
#' # Example usage:
#' trauma_data <- data.frame(
#'   Trauma_Type = c("Blunt", "Penetrating"),
#'   Patient_Age_Years = c(30, 60),
#'   RTS = c(7.84, 6.90),
#'   ISS = c(10, 25)
#' )
#'
#' result <- trauma_data |>
#'   dplyr::mutate(Ps = probability_of_survival(
#'     trauma_type = Trauma_Type,
#'     age = Patient_Age_Years,
#'     rts = RTS,
#'     iss = ISS
#'   ))
#'
probability_of_survival <- function(trauma_type, age, rts, iss) {

  # Check trauma_type
  if (!is.character(trauma_type) && !is.factor(trauma_type)) {
    cli::cli_abort("The {.var trauma_type} column must be of type {.cls character} or {.cls factor}.")
  }

  # Check for valid values in trauma_type, ignoring NA
  valid_trauma_types <- c("Blunt", "Penetrating", "Burn")
  if (!all(unique(trauma_type[!is.na(trauma_type)]) %in% c(valid_trauma_types))) {
    cli::cli_warn("The {.var trauma_type} column contains values other than 'Blunt', 'Penetrating', or 'Burn'.")
  }

  # Warn about 'Burn' and missing values
  if (any(trauma_type %in% "Burn", na.rm = TRUE) | any(is.na(trauma_type))) {
    cli::cli_warn("The {.var trauma_type} column contains missing and/or 'Burn' values. These records will not receive a probability of survival calculation.")
  }

  # Check age
  if (any(age < 0, na.rm = TRUE)) {
    cli::cli_warn(c("Negative values detected in the {.var age} column.",
                    "i" = "{.var age} must be a non-negative {.cls numeric} value."
                    ))
  }

  # Check rts
  if (any(rts < 0 | rts > 7.84, na.rm = TRUE)) {
    cli::cli_warn(c("Negative values detected in the {.var rts} column.",
                    "i" = "{.var rts} must be a {.cls numeric} value between 0 and 7.84."
                    ))
  }

  # Check iss
  if (any(iss < 0 | iss > 75, na.rm = TRUE)) {
    cli::cli_warn(c("{.var iss} values less than 0 or greater than 75 were detected.",
                     "i" = "{.var iss} must be a {.cls numeric} value between 0 and 75."
                     ))
  }

  # perform calculation
  b <- ifelse(
    trauma_type == "Blunt" &
      age < 55,
    -0.4499 + 0.8085 * rts - 0.0835 * iss - (1.7430 * 0),
    ifelse(
      trauma_type == "Blunt" &
        age >= 55,
      -0.4499 + 0.8085 * rts - 0.0835 * iss - (1.7430 * 1),
      ifelse(
        trauma_type == "Penetrating" &
          age < 55,
        -2.5355 + 0.9934 * rts - 0.0651 * iss - (1.1360 * 0),
        ifelse(
          trauma_type == "Penetrating" &
            age >= 55,
          -2.5355 + 0.9934 * rts - 0.0651 * iss - (1.1360 * 1),
          NA_real_
        )
      )
    )
  )

  survival_calc <- round(1 / (1 + exp(-b)), digits = 3)

  return(survival_calc)

}
