test_that("theme_cleaner applies the correct default theme settings", {
  # Create a basic ggplot
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    theme_cleaner()

  # Check default values in the theme
  expect_s3_class(p$theme, "theme")
  expect_equal(p$theme$axis.text$colour, "#70C8B8")  # Default base colour
  expect_equal(p$theme$plot.title$size, 14)  # Default title text size
  expect_equal(p$theme$legend.position, "top")  # Default legend position
})

test_that("theme_cleaner customizes font family and sizes", {
  # Create a ggplot with custom base font size and family
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    theme_cleaner(base_size = 14, base_family = "Arial")

  # Check if custom base font size and family are applied
  expect_equal(p$theme$text$family, "Arial")
  expect_equal(p$theme$axis.text$size, 14)
})

test_that("theme_cleaner adjusts title and subtitle sizes", {
  # Create a ggplot with custom title and subtitle sizes
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    theme_cleaner(title_text_size = 16, subtitle_text_size = 14)

  # Check if custom title and subtitle sizes are applied
  expect_equal(p$theme$plot.title$size, 16)
  expect_equal(p$theme$plot.subtitle$size, 14)
})

test_that("theme_cleaner works with facet plots", {
  # Create a ggplot with facet wrapping
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~cyl) +
    theme_cleaner(facets = TRUE, facet_text_size = 12, draw_panel_border = TRUE)

  # Check if facet text size and panel border are applied
  expect_equal(p$theme$strip.text$size, 12)
  expect_equal(p$theme$panel.border$colour, "#70C8B8")  # Default base colour for panel border
})

test_that("theme_cleaner handles axis line visibility", {
  # Create a ggplot with axis lines visible
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    theme_cleaner(axis_lines = TRUE)

  # Check if axis lines are visible
  expect_equal(p$theme$axis.line$colour, "#70C8B8")  # Default base colour
})

test_that("theme_cleaner handles empty facet panels", {
  # Create a ggplot with facet_wrap but no data for faceting
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~gear) +
    theme_cleaner(facets = TRUE)

  # Check that no errors occur, and the facets are applied
  expect_s3_class(p$theme, "theme")
})

test_that("theme_cleaner applies custom caption colour", {
  # Create a ggplot with custom caption colour
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    theme_cleaner(caption_color = "#FF5733")

  # Check if custom caption colour is applied
  expect_equal(p$theme$plot.caption$colour, "#FF5733")
})

test_that("theme_cleaner handles additional ggplot theme arguments", {
  # Create a ggplot with additional theme arguments
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    theme_cleaner(base_size = 14, plot.margin = c(1, 1, 1, 1))

  # Check if the custom margin is applied
  expect_equal(p$theme$plot.margin, c(1, 1, 1, 1))
})

test_that("theme_cleaner works with different legend positions", {
  # Create ggplots with different legend positions
  p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    theme_cleaner(legend_position = "bottom")
  p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_point() +
    theme_cleaner(legend_position = "right")

  # Check if the legend positions are applied correctly
  expect_equal(p1$theme$legend.position, "bottom")
  expect_equal(p2$theme$legend.position, "right")
})
