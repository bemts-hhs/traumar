# Calculate Probability of Survival Using TRISS Method

This function calculates the probability of survival (Ps) for trauma
patients based on the Trauma and Injury Severity Score (TRISS)
methodology. TRISS combines physiological and anatomical data to predict
survival likelihood using a logistic regression model. The function
incorporates trauma type, patient age, Revised Trauma Score (RTS), and
Injury Severity Score (ISS) into the calculation. Probability of
survival is expressed as a percentage.

## Usage

``` r
probability_of_survival(trauma_type, age, rts, iss)
```

## Arguments

- trauma_type:

  Character vector indicating the type of trauma ("Blunt" or
  "Penetrating"). Different methods exist for calculating probability of
  survival for burn patients, and so these records are excluded here.

- age:

  Numeric vector indicating the patient's age in years.

- rts:

  Numeric vector indicating the patient's Revised Trauma Score (RTS).

- iss:

  Numeric vector indicating the patient's Injury Severity Score (ISS).

## Value

Numeric vector of predicted probabilities of survival on a scale from 0
to 1.

## Details

The methodology used in the calculation of survival probabilities aligns
with the coefficients published in Norouzi et al. (2013) and Merchant et
al. (2023). Consistent with Boyd et al. (1987),
`probability_of_survival()` does not treat patients under 15 years of
age differently and accounts for penetrating injuries similarly to other
age groups. Norouzi et al. (2013) and Merchant et al. (2023) use the
updated TRISS coefficients to calculate survival probabilities for
penetrating traumas with the same coefficients as for blunt traumas. If
this approach is preferred, please take note and adjust accordingly.

## References

Boyd CR, Tolson MA, Copes WS. (1987). Evaluating trauma care: the TRISS
method. Trauma Score and the Injury Severity Score. J Trauma. 1987
Apr;27(4):370-8. PMID: 3106646.

Merchant AAH, Shaukat N, Ashraf N, Hassan S, Jarrar Z, Abbasi A, et al.
(2023). Which curve is better? A comparative analysis of trauma scoring
systems in a South Asian country. Trauma Surgery & Acute Care Open.
2023;8:e001171. <doi:10.1136/tsaco-2023-001171>

Norouzi V, Feizi I, Vatankhah S, Pourshaikhian M. (2013). Calculation of
the probability of survival for trauma patients based on trauma score
and the injury severity score model in fatemi hospital in ardabil. Arch
Trauma Res. 2013 Spring;2(1):30-5. <doi:10.5812/atr.9411>. Epub 2013
Jun 1. PMID: 24396787; PMCID: PMC3876517.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Example usage:
trauma_data <- data.frame(
  Trauma_Type = c("Blunt", "Penetrating"),
  Patient_Age_Years = c(30, 60),
  RTS = c(7.84, 6.90),
  ISS = c(10, 25)
)

# Run the function on example data
result <- trauma_data |>
  dplyr::mutate(Ps = probability_of_survival(
    trauma_type = Trauma_Type,
    age = Patient_Age_Years,
    rts = RTS,
    iss = ISS
  ))

# Print the result
result
#>   Trauma_Type Patient_Age_Years  RTS ISS        Ps
#> 1       Blunt                30 7.84  10 0.9936551
#> 2 Penetrating                60 6.90  25 0.8257009
```
