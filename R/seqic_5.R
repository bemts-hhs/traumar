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
  ###___________________________________________________________________________
  ### Data validation
  ###___________________________________________________________________________

  # Validate if `df` is a data frame or tibble.
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      c(
        "{.var df} must be of class {.cls data.frame} or {.cls tibble}.",
        "i" = "{.var df} was an object of class {.cls {class(df)}}."
      )
    )
  }

  # Validate `level`
  level_check <- df |> dplyr::pull({{ level }})
  if (!is.character(level_check) && !is.factor(level_check)) {
    cli::cli_abort(
      c(
        "{.var level} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var level} was an object of class {.cls {class(level_check)}}."
      )
    )
  }

  # Validate `unique_incident_id`
  incident_id_check <- df |> dplyr::pull({{ unique_incident_id }})
  if (!is.character(incident_id_check) && !is.factor(incident_id_check)) {
    cli::cli_abort(
      c(
        "{.var unique_incident_id} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var unique_incident_id} was an object of class {.cls {class(incident_id_check)}}."
      )
    )
  }

  # Validate `ed_disposition`
  blood_alcohol_content_check <- df |> dplyr::pull({{ blood_alcohol_content }})
  if (!is.numeric(blood_alcohol_content_check)) {
    cli::cli_abort(
      c(
        "{.var blood_alcohol_content} must be of class {.cls numeric}.",
        "i" = "{.var blood_alcohol_content} was an object of class {.cls {class(blood_alcohol_content_check)}}."
      )
    )
  }

  # Validate `hospital_disposition`
  drug_screen_check <- df |> dplyr::pull({{ drug_screen }})
  if (!is.character(drug_screen_check) && !is.factor(drug_screen_check)) {
    cli::cli_abort(
      c(
        "{.var drug_screen} must be of class {.cls character} or {.cls factor}.",
        "i" = "{.var drug_screen} was an object of class {.cls {class(drug_screen_check)}}."
      )
    )
  }

  # Validate `groups` argument
  if (!is.null(groups)) {
    if (!all(sapply(groups, is.character))) {
      cli::cli_abort(c(
        "All elements in {.var groups} must be strings.",
        "i" = "You passed a {.cls {class(groups)}} variable to {.var groups}."
      ))
    }

    if (!all(groups %in% names(df))) {
      invalid_vars <- groups[!groups %in% names(df)]
      cli::cli_abort(
        "The following group variable(s) are not valid columns in {.var df}: {paste(invalid_vars, collapse = ', ')}"
      )
    }
  }

  # Validate `calculate_ci`
  if (!is.null(calculate_ci)) {
    attempt <- try(
      match.arg(calculate_ci, choices = c("wilson", "clopper-pearson")),
      silent = TRUE
    )

    if (inherits(attempt, "try-error")) {
      cli::cli_abort(
        c(
          "If {.var calculate_ci} is not {cli::col_blue('NULL')}, it must be {.val wilson} or {.val clopper-pearson}.",
          "i" = "{.var calculate_ci} was {.val {calculate_ci}}."
        )
      )
    }

    calculate_ci <- attempt
  }

  ###___________________________________________________________________________
  ### Set up regular expressions
  ###___________________________________________________________________________

  # Options are consistent with the National Trauma Data Bank Data Dictionary
  # responses as of the 2025 release
  # Define keyword vectors
  drug_keywords <- c(
    "alcohol",
    "bzo",
    "benzodiazepine",
    "amp",
    "amphetamine",
    "coc",
    "cocaine",
    "thc",
    "cannabinoid",
    "opi",
    "opioid",
    "pcp",
    "phencyclidine",
    "bar",
    "barbiturate",
    "mamp",
    "methamphetamine",
    "mdma",
    "ectasy",
    "mtd",
    "methadone",
    "tca",
    "tricyclic antidepressant",
    "oxy",
    "oxycodone",
    "none",
    "other"
  )

  # keywords for a positive test
  positive_drug_keywords <- setdiff(drug_keywords, "none")

  # Collapse into regular expression strings
  drug_pattern_terms <- stringr::str_c(drug_keywords, collapse = "|")
  positive_drug_pattern_terms <- stringr::str_c(
    positive_drug_keywords,
    collapse = "|"
  )

  # Final patterns (case-insensitive, non-capturing)
  drug_pattern <- sprintf("(?:%s)", drug_pattern_terms)
  positive_drug_pattern <- sprintf("(?:%s)", positive_drug_pattern_terms)

  ###___________________________________________________________________________
  ### Initiate calculations
  ###___________________________________________________________________________
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
