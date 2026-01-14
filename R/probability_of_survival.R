#' @title Calculate Probability of Survival Using TRISS Method
#'
#' @description
#'
#' This function calculates the probability of survival (Ps) for trauma patients
#' based on the Trauma and Injury Severity Score (TRISS) methodology. TRISS
#' combines physiological and anatomical data to predict survival likelihood
#' using a logistic regression model. The function incorporates trauma type,
#' patient age, Revised Trauma Score (RTS), and Injury Severity Score (ISS) into
#' the calculation. Probability of survival is expressed as a percentage.
#'
#' @param trauma_type Character vector indicating the type of trauma ("Blunt" or
#'   "Penetrating"). Different methods exist for calculating probability of
#'   survival for burn patients, and so these records are excluded here.
#' @param age Numeric vector indicating the patient's age in years.
#' @param rts Numeric vector indicating the patient's Revised Trauma Score
#'   (RTS).
#' @param iss Numeric vector indicating the patient's Injury Severity Score
#'   (ISS).
#'
#' @returns Numeric vector of predicted probabilities of survival on a scale
#'   from 0 to 1.
#'
#' @details The methodology used in the calculation of survival probabilities
#' aligns with the coefficients published in Norouzi et al. (2013) and Merchant
#' et al. (2023). Consistent with Boyd et al. (1987),
#' `probability_of_survival()` does not treat patients under 15 years of age
#' differently and accounts for penetrating injuries similarly to other age
#' groups. Norouzi et al. (2013) and Merchant et al. (2023) use the updated
#' TRISS coefficients to calculate survival probabilities for penetrating
#' traumas with the same coefficients as for blunt traumas. If this approach is
#' preferred, please take note and adjust accordingly.
#'
#' @export
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @references
#'
#' Boyd CR, Tolson MA, Copes WS. (1987). Evaluating trauma care: the TRISS
#' method. Trauma Score and the Injury Severity Score. J Trauma. 1987
#' Apr;27(4):370-8. PMID: 3106646.
#'
#' Merchant AAH, Shaukat N, Ashraf N, Hassan S, Jarrar Z, Abbasi A, et al.
#' (2023). Which curve is better? A comparative analysis of trauma scoring
#' systems in a South Asian country. Trauma Surgery & Acute Care Open.
#' 2023;8:e001171. <doi:10.1136/tsaco-2023-001171>
#'
#' Norouzi V, Feizi I, Vatankhah S, Pourshaikhian M. (2013). Calculation of the
#' probability of survival for trauma patients based on trauma score and the
#' injury severity score model in fatemi hospital in ardabil. Arch Trauma Res.
#' 2013 Spring;2(1):30-5. <doi:10.5812/atr.9411>. Epub 2013 Jun 1. PMID:
#' 24396787; PMCID: PMC3876517.
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
#' # Run the function on example data
#' result <- trauma_data |>
#'   dplyr::mutate(Ps = probability_of_survival(
#'     trauma_type = Trauma_Type,
#'     age = Patient_Age_Years,
#'     rts = RTS,
#'     iss = ISS
#'   ))
#'
#' # Print the result
#' result
#'
probability_of_survival <- function(trauma_type, age, rts, iss) {
  # Ensure trauma_type is character or factor

  # Set up trauma_type validity checks
  validate_character_factor(column = trauma_type, type = "e")

  # Vector of valid trauma_type(s)
  valid_trauma_types <- c("Blunt", "Penetrating")

  # Check for valid values in trauma_type, ignoring NA
  validate_set(
    column = trauma_type,
    valid_set = valid_trauma_types,
    type = "w"
  )

  # Check age
  validate_numeric(
    column = age,
    min = 0,
    type = "warning",
    na_ok = TRUE,
    null_ok = TRUE
  )

  # Check rts
  validate_numeric(
    column = rts,
    min = 0,
    max = 7.84,
    type = "warning",
    na_ok = TRUE,
    null_ok = TRUE
  )

  # Check iss
  validate_numeric(
    column = iss,
    min = 0,
    max = 75,
    type = "warning",
    na_ok = TRUE,
    null_ok = TRUE
  )

  # Assign age category
  # Age points are assigned as 0 if age is less than 55, otherwise 1
  age_points <- ifelse(age < 55, 0, 1)

  # Assign coefficients for b0
  # Coefficient b0 is assigned based on trauma type
  b0 <- ifelse(
    trauma_type == "Blunt",
    -0.4499,
    ifelse(
      trauma_type == "Penetrating",
      -2.5355,
      NA_real_
    )
  )

  # Assign coefficients for b1
  # Coefficient b1 is assigned based on trauma type
  b1 <- ifelse(
    trauma_type == "Blunt",
    0.8085,
    ifelse(
      trauma_type == "Penetrating",
      0.9934,
      NA_real_
    )
  )

  # Assign coefficients for b2
  # Coefficient b2 is assigned based on trauma type
  b2 <- ifelse(
    trauma_type == "Blunt",
    -0.0835,
    ifelse(
      trauma_type == "Penetrating",
      -0.0651,
      NA_real_
    )
  )

  # Assign coefficients for b3
  # Coefficient b3 is assigned based on trauma type
  b3 <- ifelse(
    trauma_type == "Blunt",
    -1.7430,
    ifelse(
      trauma_type == "Penetrating",
      -1.1360,
      NA_real_
    )
  )

  # Perform calculation
  # Calculate the logit value using the assigned coefficients and input
  # variables
  b <- b0 + (b1 * rts) + (b2 * iss) + (b3 * age_points)

  # Predicted probability of survival
  # Calculate the probability of survival using the logistic function
  survival_calc <- 1 / (1 + exp(-b))

  # End function
  # Return the calculated probability of survival
  return(survival_calc)
}
