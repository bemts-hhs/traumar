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
