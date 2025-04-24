###_____________________________________________________________________________
# Indicator 5a-d
###_____________________________________________________________________________

# create indicator 5a-d for the state

seqic_indicator_5_state <- function(
  df,
  level,
  unique_incident_id,
  blood_alcohol_content,
  drug_screen,
  groups = NULL,
  calculate_ci = NULL,
  ...
) {
  # Options are consistent with the National Trauma Data Bank Data Dictionary responses as of the 2025 release

  drug_pattern <-
    "(?:alcohol|bzo|benzodiazepine|amp|amphetamine|coc|cocaine|thc|cannabinoid|opi|opioid|pcp|phencyclidine|bar|barbiturate|mamp|methamphetamine|mdma|ectasy|mtd|methadone|tca|tricyclic antidepressant|oxy|oxycodone|none|other)"

  positive_drug_pattern <-
    "(?:alcohol|bzo|benzodiazepine|amp|amphetamine|coc|cocaine|thc|cannabinoid|opi|opioid|pcp|phencyclidine|bar|barbiturate|mamp|methamphetamine|mdma|ectasy|mtd|methadone|tca|tricyclic antidepressant|oxy|oxycodone|other)"

  no_drug_pattern <- "(?:not\\s(?:tested|available|applicable|known|recorded)|na|select)"

  positive_alcohol_pattern <- "yes"

  seqic_state_5 <- df |>
    dplyr::filter({{ level }} %in% c("I", "II", "III", "IV")) |>
    dplyr::distinct({{ unique_incident_id }}, .keep_all = TRUE) |>
    dplyr::summarize(
      numerator_5a = sum(!is.na({{ blood_alcohol_content }})),
      denominator_5a = dplyr::n(),
      seqic_5a = round(numerator_5a / denominator_5a, digits = 3),
      numerator_5b = sum({{ blood_alcohol_content }} > 0, na.rm = TRUE),
      denominator_5b = sum(!is.na({{ blood_alcohol_content }})),
      seqic_5b = round(numerator_5b / denominator_5b, digits = 3),
      numerator_5c = sum(
        grepl(
          pattern = drug_pattern,
          x = {{ drug_screen }},
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      denominator_5c = dplyr::n(),
      seqic_5c = round(numerator_5c / denominator_5c, digits = 3),
      numerator_5d = sum(
        grepl(
          pattern = positive_drug_pattern,
          x = {{ drug_screen }},
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      denominator_5d = sum(
        grepl(
          pattern = drug_pattern,
          x = {{ drug_screen }},
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      seqic_5d = round(numerator_5d / denominator_5d, digits = 3),
      .by = {{ groups }}
    )

  return(seqic_state_5)
}
