#' @title Exploratory Data Analysis, Normality Testing, and
#'   Visualization
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `is_it_normal()` calculates descriptive statistics and conducts univariate
#' normality testing on one or more numeric variables in a dataset using a
#' selected statistical test. Optional plots are included for one variable at a
#' time, only. Results are returned as a named list containing summaries and,
#' optionally, normality tests and/or diagnostic plots.
#'
#' @param df A `data.frame` or `tibble` containing the variables to assess.
#' @param ... One or more unquoted column names from `df` to be analyzed.
#' @param group_vars Optional. A character vector of column names in `df` to
#'   group results by (e.g., `c("year", "hospital_level")`). If `NULL`, no
#'   grouping is applied. Grouped summaries and normality tests are computed
#'   within each unique combination of values across these variables.
#' @param seed A numeric value passed to `set.seed()` to ensure reproducibility.
#'   Default is `10232015`.
#' @param normality_test A character string specifying the statistical test to
#'   use. Must be one of: `"shapiro-wilk" or "shapiro" or "sw"`,
#'   `"kolmogorov-smirnov" or "ks"`, `"anderson-darling" or "ad"`, `"lilliefors"
#'   or "lilli"`, `"cramer-von-mises" or "cvm"`, `"pearson" or "p"`, or
#'   `"shapiro-francia" or "sf"`. If `NULL`, no normality test is performed,
#'   which is the default.
#' @param include_plots Logical. If `TRUE`, plots are generated for a single
#'   variable. Plotting is disabled if multiple variables are passed.
#' @param plot_theme A `ggplot2::theme` function to apply to all plots. Default
#'   is `traumar::theme_cleaner`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{descriptive_statistics}{A `tibble` of summary statistics for each
#'   variable.}
#'   \item{normality_test}{A `tibble` of test statistics and p-values
#'   (if `normality_test == TRUE`).}
#'   \item{plots}{A patchwork object containing four plots (if `include_plots =
#'   TRUE` and one variable supplied).}
#' }
#'
#' @details
#' \itemize{
#' \item If the data do not meet the test requirements for a chosen test of
#' normality, `is_it_normal()` will not run the tests.
#' \item Normality tests may yield differing results. Each test has distinct
#' assumptions and sensitivity. Users should verify assumptions and consult
#' test-specific guidance to ensure appropriate use.
#' \item The function will abort with helpful CLI messages if input types or
#' structures are incorrect.
#' \item If plotting is enabled, and `nrow(df) > 10000`, a warning is issued
#' as plotting may become computationally expensive.
#'}
#'
#' @note Supported normality tests are below. Please check the specifications of
#'   these tests in the corresponding documentation.
#' \itemize{
#' \item Shapiro-Wilk (`stats::shapiro.test()`)
#' \item Kolmogorov-Smirnov (`stats::ks.test()`)
#' \item Anderson-Darling (`nortest::ad.test()`)
#' \item Lilliefors (`nortest::lillie.test()`)
#' \item Cramer-von Mises (`nortest::cvm.test()`)
#' \item Pearson (`norest::pearson.test()`)
#' \item Shapiro-Francia (`nortest::sf.test()`)
#'}
#'
#' Please note that if grouped plotting is enabled, each group will generate its
#' own set of plots. This may flood your IDE or console. Plan your use of this
#' functionality with care to avoid lags or unwanted outputs.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
is_it_normal <- function(
  df,
  ...,
  group_vars = NULL,
  seed = 10232015,
  normality_test = NULL,
  include_plots = FALSE,
  plot_theme = traumar::theme_cleaner
) {
  ###___________________________________________________________________________
  ### Data validation
  ###___________________________________________________________________________

  # Validate the `df` argument
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(c(
      "Input to argument {.strong {.var df}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
      "x" = "You supplied an object of class {.cls {class(df)}}.",
      "i" = "Please use an object of class {.cls data.frame} or {.cls tibble}."
    ))
  }

  # Validate group_vars
  if (!is.null(group_vars)) {
    if (!is.character(group_vars)) {
      cli::cli_abort(c(
        "Argument {.var group_vars} must be {.cls character} or {.val NULL}.",
        "x" = "You supplied an object of class {.cls {class(group_vars)}}."
      ))
    }

    missing_vars <- setdiff(group_vars, names(df))
    if (length(missing_vars) > 0) {
      cli::cli_abort(c(
        "Some values in {.var group_vars} are not columns in {.var df}.",
        "x" = "Missing column(s): {.val {missing_vars}}.",
        "i" = "Check that all values in {.var group_vars} match column names in {.var df}."
      ))
    }
  }

  # validate the `normality_test`
  if (!is.null(normality_test)) {
    # Normalize aliases
    normality_test <- tolower(normality_test)
    normality_test <- switch(
      normality_test,
      "shapiro" = "shapiro-wilk",
      "sw" = "shapiro-wilk",
      "sf" = "shapiro-francia",
      "ks" = "kolmogorov-smirnov",
      "ad" = "anderson-darling",
      "lilli" = "lilliefors",
      "cvm" = "cramer-von mises",
      "p" = "pearson",
      normality_test # if not an alias, keep original
    )

    attempt <- try(
      match.arg(
        normality_test,
        choices = c(
          "shapiro-wilk",
          "kolmogorov-smirnov",
          "anderson-darling",
          "lilliefors",
          "cramer-von mises",
          "pearson",
          "shapiro-francia"
        )
      ),
      silent = TRUE
    )

    if (inherits(attempt, "try-error")) {
      cli::cli_abort(
        c(
          "If {.var normality_test} is not {cli::col_blue('NULL')}, it must be one of {.val shapiro-wilk}, {.val kolmogorov-smirnov}, {.val anderson-darling}, {.val lilliefors}, {.val cramer-von mises}, {.val pearson}, or {.val shapiro-francia}.",
          "i" = "{.var normality_test} was {.val {normality_test}}."
        )
      )
    }

    normality_test <- attempt
  }

  # Check if plot_theme is a function
  if (include_plots && !is.function(plot_theme)) {
    cli::cli_abort(c(
      "Input to argument {.strong {.var plot_theme}} was not of the correct class.",
      "x" = "You supplied an object of class {.cls {class(plot_theme)}} to {.var plot_theme}.",
      "i" = "Please use an object of class {.cls function}, usually a {.fn ggplot2::theme} object."
    ))
  } else {
    chosen_theme <- as.function(plot_theme)
  }

  if (!is.numeric(seed)) {
    cli::cli_abort(c(
      "In order to set the random seed, {.var seed} must have class {.cls numeric}.",
      "i" = "{.var seed} had class {.cls {class(seed)}}."
    ))
  }

  # Set up variables to use in purrr::map()
  vars <- rlang::enquos(...)

  # Data name
  data_name <- deparse(substitute(df))

  # Dynamically generate the variable names
  var_names <- purrr::map_chr(vars, ~ rlang::as_label(.))

  # Check if multiple columns were provided
  multiple_columns <- if (length(vars) > 1) TRUE else FALSE

  # Callout for the variables passed
  cli::cli_h1("Output Summary")
  cli::cli_h3("Descriptive Statistics")
  cli::cli_inform(c(
    "*" = paste0(
      "Exploratory data analysis on the variable(s): ",
      cli::col_green(cli::style_bold(paste(var_names, collapse = ", "))),
      " from the '",
      cli::col_magenta(cli::style_bold(data_name)),
      "' dataset."
    )
  ))

  # Messaging related to normality testing
  cli::cli_h3("Normality Tests")

  # Issue a warning if data violates any normality test choice assumptions
  if (
    !is.null(normality_test) &&
      normality_test %in%
        c(
          "shapiro-wilk",
          "shapiro-francia",
          "anderson-darling",
          "cramer-von mises",
          "lilliefors"
        )
  ) {
    # Default values for tests
    test_info <- list(
      "shapiro-wilk" = list(
        min_n = 3,
        max_n = 5000,
        fn = "stats::shapiro.test"
      ),
      "shapiro-francia" = list(
        min_n = 5,
        max_n = 5000,
        fn = "nortest::sf.test"
      ),
      "anderson-darling" = list(
        min_n = 8,
        max_n = Inf,
        fn = "nortest::ad.test"
      ),
      "cramer-von mises" = list(
        min_n = 8,
        max_n = Inf,
        fn = "nortest::cvm.test"
      ),
      "lilliefors" = list(
        min_n = 5,
        max_n = Inf,
        fn = "nortest::lillie.test"
      )
    )[[normality_test]]

    # Perform the test within each group
    group_status <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::group_modify(
        ~ {
          group_data <- .x
          group_keys <- .y

          var_status <- purrr::map_chr(vars, function(var) {
            var_name <- rlang::as_label(var)
            vec <- dplyr::pull(group_data, !!var)
            n_complete <- sum(!is.na(vec))

            # control structure for no group
            if (!is.null(group_vars)) {
              group_label <- paste(
                purrr::map2_chr(
                  names(group_keys),
                  group_keys,
                  ~ glue::glue("{.x} = {.y}")
                ),
                collapse = ", "
              )
            } else {
              group_label <- "full dataset"
            }

            if (n_complete < test_info$min_n) {
              glue::glue(
                "{var_name} ({group_label}): n_complete = {n_complete}, problem: n_complete < {test_info$min_n}"
              )
            } else if (n_complete > test_info$max_n) {
              glue::glue(
                "{var_name} ({group_label}): n_complete = {n_complete}, problem: n_complete > {test_info$max_n}"
              )
            } else {
              NA_character_
            }
          })

          tibble::tibble(var_status = var_status)
        }
      )

    # Explicit check for any actual messages
    violations_exist <- any(!is.na(group_status$var_status))

    if (violations_exist) {
      cli::cli_alert_warning(c(
        "One or more variables do not meet the sample size requirements for {.fn {test_info$fn}}.\n",
        "The test will not run for overall data or any groups that fail to meet the requirements.\n",
        "The test will be performed for groups that meet the sample size requirements.\n",
        "Please select another test if desired."
      ))
      cli::cli_alert_info("Problematic variables:")
      purrr::walk(stats::na.omit(group_status$var_status), cli::cli_li)
    } else {
      # When all is well, let the user know.
      cli::cli_alert_info(
        "No issues reported with the univariate tests of normality. Proceeding."
      )
    }
  } else if (is.null(normality_test)) {
    # When no normality test procedure is requested, return a message
    cli::cli_alert_info("No normality test requested.")
  } else {
    # Data limits do not apply to other tests in these ways.
    cli::cli_alert_info(
      "No issues reported with the univariate tests of normality. Proceeding."
    )
  }

  # Summary on grouping behavior of the function
  cli::cli_h3("Strata")
  # Callout for the grouping applied (or no grouping if NULL)
  if (is.null(group_vars) || length(group_vars) == 0) {
    cli::cli_inform(
      c(
        "*" = "No grouping applied. Descriptive statistics and normality tests (as applicable) are computed on the entire dataset."
      )
    )
  } else {
    cli::cli_inform(
      c(
        "*" = paste0(
          "Grouping by: ",
          cli::col_blue(cli::style_bold(paste(group_vars, collapse = ", ")))
        )
      )
    )
  }

  ###___________________________________________________________________________
  ### Create some convention functions for plotting
  ###___________________________________________________________________________

  # A utility function to check the length of a vector
  is_valid_for_plotting <- function(vec, min_n = 2) {
    vec <- vec[!is.na(vec)]
    length(vec) >= min_n && all(is.finite(vec))
  }

  # Utility function for a density plot
  make_density_plot <- function(data, vec, var, var_name, group_label = NULL) {
    if (!is_valid_for_plotting(vec)) {
      cli::cli_alert_danger(
        "Skipping density plot for {.var {var_name}}{if (!is.null(group_label)) paste0(' (', group_label, ')')}: insufficient valid data."
      )
      return(NULL)
    }

    ggplot2::ggplot(data, ggplot2::aes(x = !!var)) +
      ggplot2::geom_density(
        fill = "#F27026",
        color = "black",
        alpha = 0.5,
        na.rm = TRUE
      ) +
      ggplot2::geom_segment(
        x = median(vec, na.rm = TRUE),
        xend = median(vec, na.rm = TRUE),
        y = 0,
        yend = Inf,
        linetype = "dashed",
        color = "darkslategray",
        alpha = 0.25,
        linewidth = 1.5
      ) +
      ggplot2::ggtitle(
        if (is.null(group_label)) {
          paste0(
            "Kernel Density Plot of ",
            var_name,
            " with horizontal line at the median"
          )
        } else {
          paste0("Density Plot with Median (", group_label, ")")
        }
      ) +
      ggplot2::labs(
        x = var_name,
        caption = paste0("n = ", sum(!is.na(vec)), " non-missings")
      ) +
      chosen_theme()
  }

  # Utility function for a histogram plot
  make_hist_plot <- function(data, vec, var, var_name, group_label = NULL) {
    if (!is_valid_for_plotting(vec)) {
      cli::cli_alert_danger(
        "Skipping histogram for {.var {var_name}}{if (!is.null(group_label)) paste0(' (', group_label, ')')}: insufficient valid data."
      )
      return(NULL)
    }

    bin_width <- (max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE)) / 15

    ggplot2::ggplot(data, ggplot2::aes(x = !!var)) +
      ggplot2::geom_histogram(
        binwidth = bin_width,
        fill = "#C6D667",
        color = "black",
        na.rm = TRUE
      ) +
      ggplot2::ggtitle(
        if (is.null(group_label)) paste0("Histogram of ", var_name) else
          paste0("Histogram (", group_label, ")")
      ) +
      ggplot2::labs(x = var_name) +
      chosen_theme()
  }

  # Utility function for a boxplot
  make_boxplot <- function(data, vec, var, var_name, group_label = NULL) {
    if (!is_valid_for_plotting(vec)) {
      cli::cli_alert_danger(
        "Skipping boxplot for {.var {var_name}}{if (!is.null(group_label)) paste0(' (', group_label, ')')}: insufficient valid data."
      )
      return(NULL)
    }

    ggplot2::ggplot(data, ggplot2::aes(x = !!var, y = "")) +
      ggplot2::geom_jitter(
        color = "#03617A",
        alpha = 0.25,
        na.rm = TRUE,
        height = 0.35
      ) +
      ggplot2::geom_boxplot(
        fill = "#B9E1DA",
        color = "black",
        alpha = 0.2,
        na.rm = TRUE,
        orientation = "y"
      ) +
      ggplot2::stat_boxplot(geom = "errorbar", width = 0.5) +
      ggplot2::ggtitle(
        if (is.null(group_label))
          paste0("Boxplot with scatterplot of ", var_name) else
          paste0("Boxplot (", group_label, ")")
      ) +
      ggplot2::labs(x = var_name, y = "") +
      chosen_theme()
  }

  # Utility function for a Q-Q plot
  make_qq_plot <- function(data, vec, var, var_name, group_label = NULL) {
    if (!is_valid_for_plotting(vec)) {
      cli::cli_alert_danger(
        "Skipping Q-Q plot for {.var {var_name}}{if (!is.null(group_label)) paste0(' (', group_label, ')')}: insufficient valid data."
      )
      return(NULL)
    }

    ggplot2::ggplot(data, ggplot2::aes(sample = !!var)) +
      ggplot2::geom_qq_line(
        linewidth = 1.25,
        color = "#70C8B8",
        alpha = 0.7,
        na.rm = TRUE
      ) +
      ggplot2::stat_qq(
        color = "#19405B",
        alpha = 0.4,
        size = 2,
        na.rm = TRUE
      ) +
      ggplot2::ggtitle(
        if (is.null(group_label)) paste0("Normal Q-Q Plot of ", var_name) else
          paste0("Normal Q-Q Plot (", group_label, ")")
      ) +
      chosen_theme()
  }

  ###___________________________________________________________________________
  ### Initialize the output list
  ###___________________________________________________________________________

  output_list <- list()

  ###___________________________________________________________________________
  ### Generate descriptive statistics
  ###___________________________________________________________________________

  # Iterate over the provided columns
  results <- purrr::map(vars, function(var) {
    var_name <- rlang::as_label(var)

    df_summarized <- df |>
      dplyr::select(dplyr::all_of(c(group_vars, var_name))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarize(
        variable = var_name,
        mean = round(mean(.data[[var_name]], na.rm = TRUE), 3),
        std_dev = round(sd(.data[[var_name]], na.rm = TRUE), 3),
        min = min(.data[[var_name]], na.rm = TRUE),
        quantile_25 = quantile(.data[[var_name]], 0.25, na.rm = TRUE),
        median = quantile(.data[[var_name]], 0.5, na.rm = TRUE),
        quantile_75 = quantile(.data[[var_name]], 0.75, na.rm = TRUE),
        max = max(.data[[var_name]], na.rm = TRUE),
        non_missings = sum(!is.na(.data[[var_name]])),
        missings = sum(is.na(.data[[var_name]])),
        p_complete = round(mean(!is.na(.data[[var_name]])), 3),
        p_missing = round(mean(is.na(.data[[var_name]])), 3),
        n_obs = dplyr::n(),
        .groups = "drop"
      )

    df_summarized
  })

  # set names
  names(results) <- purrr::map_chr(vars, rlang::as_label)

  # Pivot results wider
  results <- results |>
    purrr::list_rbind()

  # Assign results to the output_list
  output_list$descriptive_statistics <- results

  ###___________________________________________________________________________
  ### Optional: Perform the test of normality on variables
  ###___________________________________________________________________________

  if (!is.null(normality_test)) {
    # Only set the random seed if normality test is done
    set.seed(seed)

    # Grouping (if any) applied before normality test
    if (!is.null(group_vars) && length(group_vars) > 0) {
      # Group data by the specified group_vars
      df <- df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))
    }

    # Iterate over the variables of interest to perform the requested normality
    # test within each group (or overall if no grouping)
    test_results <- purrr::map(vars, function(var) {
      var_name <- rlang::as_label(var)

      # Perform normality test within each group
      test_result <- df |>
        dplyr::group_modify(function(sub_df, group) {
          vec <- dplyr::pull(sub_df, !!var)

          # get complete observations
          n_complete <- sum(!is.na(vec))

          # Try to perform the normality test and handle any errors
          test_result <- tryCatch(
            {
              # Shapiro-Wilk
              if (normality_test == "shapiro-wilk") {
                test_result <- shapiro.test(vec) |>
                  broom::tidy() |>
                  dplyr::mutate(
                    result = dplyr::if_else(
                      p.value < 0.05,
                      "Non-normal distribution",
                      "Normal distribution"
                    )
                  )

                # Kolmogorov-Smirnov
              } else if (normality_test == "kolmogorov-smirnov") {
                test_result <- ks.test(vec, y = "pnorm") |>
                  broom::tidy() |>
                  dplyr::mutate(
                    result = dplyr::if_else(
                      p.value < 0.05,
                      "Non-normal distribution",
                      "Normal distribution"
                    )
                  )

                # Anderson-Darling
              } else if (normality_test == "anderson-darling") {
                test_result <- nortest::ad.test(vec) |>
                  broom::tidy() |>
                  dplyr::mutate(
                    result = dplyr::if_else(
                      p.value < 0.05,
                      "Non-normal distribution",
                      "Normal distribution"
                    )
                  )

                # Lilliefors
              } else if (normality_test == "lilliefors") {
                test_result <- nortest::lillie.test(vec) |>
                  broom::tidy() |>
                  dplyr::mutate(
                    result = dplyr::if_else(
                      p.value < 0.05,
                      "Non-normal distribution",
                      "Normal distribution"
                    )
                  )

                # Cramer-von Mises
              } else if (normality_test == "cramer-von mises") {
                test_result <- nortest::cvm.test(vec) |>
                  broom::tidy() |>
                  dplyr::mutate(
                    result = dplyr::if_else(
                      p.value < 0.05,
                      "Non-normal distribution",
                      "Normal distribution"
                    )
                  )
                # Pearson
              } else if (normality_test == "pearson") {
                test_result <- nortest::pearson.test(vec) |>
                  broom::tidy() |>
                  dplyr::mutate(
                    result = dplyr::if_else(
                      p.value < 0.05,
                      "Non-normal distribution",
                      "Normal distribution"
                    )
                  )
                # Shapiro-Francia
              } else if (normality_test == "shapiro-francia") {
                test_result <- nortest::sf.test(vec) |>
                  broom::tidy() |>
                  dplyr::mutate(
                    result = dplyr::if_else(
                      p.value < 0.05,
                      "Non-normal distribution",
                      "Normal distribution"
                    )
                  )
              }
            },
            error = function(e) {
              # Return a tibble indicating the test was skipped
              tibble::tibble(
                variable = var_name,
                test = "Test Skipped",
                statistic = NA_real_,
                p_value = NA_real_,
                result = "Test Skipped"
              )
            }
          )

          # Return the results in a tibble, including the group
          suppressWarnings(
            tibble::tibble(
              variable = var_name,
              test = test_result$method,
              statistic = test_result$statistic,
              p_value = test_result$p.value,
              result = test_result$result
            )
          )
        })

      # Return the test results for the variable
      test_result
    })

    # Combine the individual variable results into a single data frame
    test_results <- purrr::list_rbind(test_results)

    # Store the final results in the output list
    output_list$normality_test <- test_results
  } else {
    output_list$normality_test <- NULL
  }

  ###___________________________________________________________________________
  ### Optional plots - only available if one variable is passed to `variables`
  ###___________________________________________________________________________

  if (include_plots && !multiple_columns) {
    # Extract data programmatically
    var <- vars[[1]]
    var_name <- rlang::as_label(var)

    # Header for the plotting section
    cli::cli_h3("Visualizations")

    if (nrow(df) > 5000) {
      # Issue a warning
      cli::cli_alert_warning(
        c(
          "Large Data Warning! The number of observations within {.var df} is large, plotting may become computationally expensive."
        )
      )

      # Issue an alternative
      cli::cli_alert_success(
        "Consider downsampling your data using {.fn base::sample}, {.fn dplyr::slice_sample}, or a function with a similar purpose."
      )
    }

    # Grouped plotting can be computationally expensive
    # Ensure the user is aware and provide them a way out
    if (!is.null(group_vars)) {
      cli::cli_alert_warning(
        c(
          "Grouped plotting is enabled. Each group will generate its own set of plots. This may flood your IDE or console."
        )
      )
      cli::cli_alert_info(
        "To avoid this, assign the function result to an object and inspect plots selectively from the list output."
      )

      ###_______________________________________________________________________
      ### Plotting functionality
      ###_______________________________________________________________________

      # Group the data
      grouped_data <- df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
        dplyr::group_split()

      # Get labels for outside the loop
      plot_list_labels <- grouped_data |>
        purrr::map_chr(
          ~ {
            keys <- dplyr::distinct(
              .x,
              dplyr::across(dplyr::all_of(group_vars))
            )
            paste(
              purrr::map2_chr(names(keys), keys, \(n, v) glue::glue("{n}={v}")),
              collapse = ", "
            )
          }
        )

      # Create the plots with dynamic names
      output_list$plots <- grouped_data |>
        purrr::map(function(group_data) {
          group_keys <- dplyr::distinct(
            group_data,
            dplyr::across(dplyr::all_of(group_vars))
          )
          group_label <- paste(
            purrr::map2_chr(
              names(group_keys),
              group_keys,
              ~ glue::glue("{.x} = {.y}")
            ),
            collapse = ", "
          )

          # Get the data as a vector
          vec <- dplyr::pull(group_data, !!var)

          # QQ Plot
          qqplot <- make_qq_plot(group_data, vec, var, var_name, group_label)

          # Histogram
          hist_plot <- make_hist_plot(
            group_data,
            vec,
            var,
            var_name,
            group_label
          )

          # Kernel Density Plot with line at the median
          density_plot <- make_density_plot(
            group_data,
            vec,
            var,
            var_name,
            group_label
          )

          # Boxplot with scatterplot
          boxplot_plot <- make_boxplot(
            group_data,
            vec,
            var,
            var_name,
            group_label
          )

          # Set up list of plots, remove NULL objects
          valid_plots <- purrr::compact(
            list(
              qqplot,
              hist_plot,
              density_plot,
              boxplot_plot
            )
          )

          # Set up the patchwork
          patchwork::wrap_plots(valid_plots) +
            patchwork::plot_annotation(
              title = paste0(
                "Normality Test of the '",
                data_name,
                "' dataset, regarding variable '",
                var_name,
                "'"
              ),
              theme = chosen_theme()
            )
        }) |>
        purrr::set_names(plot_list_labels) |>
        purrr::compact()

      # Plot a single variable without grouped output
    } else {
      # Get the data of interest as a vector
      vec <- df |> dplyr::pull(!!var)

      # QQ Plot
      qqplot <- make_qq_plot(df, vec, var, var_name)

      # Histogram
      hist_plot <- make_hist_plot(df, vec, var, var_name)

      # Kernel Density Plot
      density_plot <- make_density_plot(df, vec, var, var_name)

      # Boxplot with scatterplot
      boxplot_plot <- make_boxplot(df, vec, var, var_name)

      # Combine, remove NULL objects
      plot_list <- purrr::compact(list(
        qqplot,
        hist_plot,
        density_plot,
        boxplot_plot
      ))

      # Patchwork
      combined_plots <- patchwork::wrap_plots(plot_list) +
        patchwork::plot_annotation(
          title = paste0(
            "Normality Test of the '",
            data_name,
            "' dataset, regarding variable '",
            var_name,
            "'"
          ),
          theme = chosen_theme()
        )

      # Add to the list
      output_list$plots <- combined_plots
    }
  } else if (include_plots && multiple_columns) {
    # Header for the plotting section
    cli::cli_h3("Visualizations")

    # If multiple_columns, then do not include plots
    include_plots <- FALSE

    cli::cli_alert_info(
      "More than one column was passed to {.fn is_it_normal}, and plotting is only available for one column at a time. {.var include_plots} is now set to {.val FALSE}."
    )
  }

  # Line to separate output for cleaner presentation
  cli::cli_rule()

  # Return outputs as a list
  return(output_list)
}
