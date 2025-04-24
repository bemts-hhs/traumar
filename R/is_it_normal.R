#' @title Assess Normality of a Continuous Variable
#'
#' @description
#'
#' Evaluates whether a numeric variable in a data frame follows a normal
#' distribution. Provides descriptive statistics, optional visualizations, and
#' formal normality tests.
#'
#' @param df A `data.frame` or `tibble`. The dataset containing the variable.
#' @param x A bare (unquoted) column name in `df` specifying the continuous
#'   variable to assess.
#' @param seed A numeric value to set the random seed. Defaults to `10232015`.
#' @param normality_test A string indicating which test to use. Must be one of:
#' `"shapiro-wilk"`, `"kolmogorov-smirnov"`, `"anderson-darling"`, `"lilliefors"`,
#' or `"cramer-von mises"`. Set to `NULL` to skip the test.
#' @param include_plots Logical. If `TRUE`, includes a set of diagnostic plots.
#' @param plot_theme A function for setting the `ggplot2` theme. Defaults to
#'   `traumar::theme_cleaner`.
#' @param ... Additional arguments passed to the `plot_theme`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{descriptive_statistics}{A tibble summarizing the variable's
#'   distribution.}
#'   \item{normality_test}{(Optional) A tidy result of the specified normality
#'   test.}
#'   \item{plots}{(Optional) A `patchwork` object containing four
#'   visualizations: Q-Q plot, histogram, density plot, and boxplot with
#'   scatter.}
#' }
#'
#' @author Nicolas Foss Ed.D., MS
#'
#' @export
#'
is_it_normal <- function(
  df,
  ...,
  seed = 10232015,
  normality_test = c(
    "shapiro-wilk",
    "kolmogorov-smirnov",
    "anderson-darling",
    "lilliefors",
    "cramer-von mises"
  ),
  include_plots = FALSE,
  plot_theme = traumar::theme_cleaner
) {
  # Validate the `df` argument
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(c(
      "Input to argument {.strong {.var df}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
      "x" = "You supplied an object of class {.cls {class(df)}}.",
      "i" = "Please use an object of class {.cls data.frame} or {.cls tibble}."
    ))
  }

  # validate the `normality_test`
  if (!is.null(normality_test)) {
    attempt <- try(
      match.arg(
        normality_test,
        choices = c(
          "shapiro-wilk",
          "kolmogorov-smirnov",
          "anderson-darling",
          "lilliefors",
          "cramer-von mises"
        )
      ),
      silent = TRUE
    )

    if (inherits(attempt, "try-error")) {
      cli::cli_abort(
        c(
          "If {.var normality_test} is not {cli::col_blue('NULL')}, it must be {.val wilson} or {.val clopper-pearson}.",
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

  # Callout for the variables passed
  cli::cli_h1("Output Summary")
  cli::cli_inform(c(
    "*" = paste0(
      "Exploratory data analysis on the variables: ",
      cli::col_green(cli::style_bold(paste(var_names, collapse = ", "))),
      " from the '",
      cli::col_magenta(cli::style_bold(data_name)),
      "' dataset."
    )
  ))

  # Issue a warning for the user if it is not advised to use the shapiro.test
  if (normality_test == "shapiro-wilk") {
    # Check if the data meet the requirements for shapiro.test()
    # If no, then default to Lilliefors
    if (nrow(df) < 3 || nrow(df) > 5000) {
      length_check <- dplyr::if_else(
        nrow(df) < 3,
        "less than 3",
        dplyr::if_else(
          nrow(df) > 5000,
          "greater than 5000",
          NA_character_
        )
      )

      cli::cli_alert_warning(
        "Total observations were {length_check}, and so {.fn stats::shapiro.test} cannot be used. Defaulting to the Lilliefors test via {.fn nortest::lillie.test}."
      )
    }
  }

  # Check if multiple columns were provided
  multiple_columns <- if (length(vars) > 1) TRUE else FALSE

  # If multiple_columns, then do not include plots
  if (multiple_columns && include_plots == TRUE) {
    include_plots <- FALSE

    cli::cli_alert_info(
      "More than one column was passed to {.fn is_it_normal}, and plotting is only available for one column at a time."
    )
  }

  # Line separator
  cli::cli_rule()

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
    vec <- df |> dplyr::pull(!!var)

    if (!is.numeric(vec)) {
      cli::cli_abort(c(
        "Variable {.var {var_name}} must be numeric.",
        "i" = "{.var {var_name} had class {.cls {class(vec)}."
      ))
    }

    tibble::tibble(
      variable = var_name,
      mean = round(mean(vec, na.rm = TRUE), 3),
      std_dev = round(sd(vec, na.rm = TRUE), 3),
      min = min(vec, na.rm = TRUE),
      quantile_25 = quantile(vec, 0.25, na.rm = TRUE),
      median = quantile(vec, 0.5, na.rm = TRUE),
      quantile_75 = quantile(vec, 0.75, na.rm = TRUE),
      max = max(vec, na.rm = TRUE),
      non_missings = sum(!is.na(vec)),
      missings = sum(is.na(vec)),
      p_complete = round(mean(!is.na(vec)), 3),
      p_missing = round(mean(is.na(vec)), 3),
      n_obs = length(vec)
    )
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

    # Iterate over the variables of interest to perform the requested normality
    # test
    test_results <- purrr::map(vars, function(var) {
      var_name <- rlang::as_label(var)
      vec <- dplyr::pull(df, !!var)

      # Shapiro-Wilk
      if (normality_test == "shapiro-wilk") {
        # Check if the data meet the requirements for shapiro.test()
        # If no, then default to Lilliefors
        if (length(vec) < 3 | length(vec) > 5000) {
          length_check <- dplyr::if_else(
            length(vec) < 3,
            "less than 3",
            dplyr::if_else(
              length(vec) > 5000,
              "greater than 5000",
              NA_character_
            )
          )

          # Default to the Lilliefors test
          test_result <- nortest::lillie.test(vec) |>
            broom::tidy() |>
            dplyr::mutate(
              result = dplyr::if_else(
                p.value < 0.05,
                "Non-normal distribution",
                "Normal distribution"
              )
            )
        } else {
          test_result <- shapiro.test(vec) |>
            broom::tidy() |>
            dplyr::mutate(
              result = dplyr::if_else(
                p.value < 0.05,
                "Non-normal distribution",
                "Normal distribution"
              )
            )
        }

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
      } else if (normality_test == "cramer-von-mises") {
        test_result <- nortest::cvm.test(vec) |>
          broom::tidy() |>
          dplyr::mutate(
            result = dplyr::if_else(
              p.value < 0.05,
              "Non-normal distribution",
              "Normal distribution"
            )
          )
      }

      # Dynamically assign the data to a tibble
      tibble::tibble(
        variable = var_name,
        test = test_result$method,
        statstic = test_result$statistic,
        p_value = test_result$p.value,
        result = test_result$result
      )
    })

    # Pivot the test_results wider
    test_results <- test_results |>
      purrr::list_rbind()

    # Use list_rbind to combine the results (instead of purrr::map_dfr)
    output_list$normality_test <- test_results
  }

  ###___________________________________________________________________________
  ### Optional plots - only available if one variable is passed to `variables`
  ###___________________________________________________________________________

  if (include_plots && !multiple_columns) {
    var <- vars[[1]]
    var_name <- rlang::as_label(var)
    vec <- df |> dplyr::pull(!!var)

    # QQ plot
    qqplot <- ggplot2::ggplot(df, ggplot2::aes(sample = !!var)) +
      ggplot2::geom_qq_line(
        linewidth = 1.25,
        color = "#70C8B8",
        alpha = 0.7,
        na.rm = TRUE
      ) +
      ggplot2::stat_qq(color = "#19405B", alpha = 0.2, size = 2, na.rm = TRUE) +
      ggplot2::ggtitle(paste0("Normal Q-Q Plot of ", var_name)) +
      chosen_theme()

    # Histogram
    hist_plot <- ggplot2::ggplot(df, ggplot2::aes(x = !!var)) +
      ggplot2::geom_histogram(
        binwidth = (max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE)) / 15,
        fill = "#C6D667",
        color = "black",
        na.rm = TRUE
      ) +
      ggplot2::ggtitle(paste0("Histogram of ", var_name)) +
      ggplot2::labs(x = var_name) +
      chosen_theme()

    # Density plot
    density_plot <- ggplot2::ggplot(df, ggplot2::aes(x = !!var)) +
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
      ggplot2::ggtitle(paste0(
        "Kernel Density Plot of ",
        var_name,
        "\nwith horizontal line at the median"
      )) +
      ggplot2::labs(
        x = var_name,
        caption = paste0("n = ", sum(!is.na(vec)), " non-missings")
      ) +
      chosen_theme()

    # Boxplot
    boxplot_plot <- ggplot2::ggplot(df, ggplot2::aes(x = !!var, y = "")) +
      ggplot2::geom_jitter(
        color = "#03617A",
        alpha = 0.1,
        na.rm = TRUE,
        height = 0.35
      ) +
      ggplot2::geom_boxplot(
        fill = "#FDD304",
        color = "black",
        alpha = 0.5,
        na.rm = TRUE,
        orientation = "y"
      ) +
      ggplot2::stat_boxplot(geom = "errorbar", width = 0.5) +
      ggplot2::ggtitle(paste0("Boxplot with scatterplot of ", var_name)) +
      ggplot2::labs(x = var_name, y = "") +
      chosen_theme()

    # Combine
    plot_list <- list(qqplot, hist_plot, density_plot, boxplot_plot)
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

    output_list$plots <- combined_plots
  }

  # Return outputs as a list
  return(output_list)
}
