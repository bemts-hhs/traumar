#' @title Customizable Minimalistic ggplot2 Theme
#'
#' @description
#'
#' A flexible and customizable theme function for creating polished and
#' minimalistic plots using `ggplot2`. The `theme_cleaner` function provides
#' various options to control the appearance of plot elements, including font
#' styles, sizes, colors, axis lines, grid lines, legend, title, subtitle,
#' captions, and facet appearance. The theme is highly customizable, allowing
#' for the creation of visually appealing and clean plots.
#'
#' @param base_size Numeric. Default font size for plot elements. Defaults to
#'   `12`.
#' @param base_family Character. Font family used for text in the plot. Defaults
#'   to `"Work Sans"`.
#' @param base_color Character. Hex color code for primary plot elements (e.g.,
#'   axis text, legend text). Defaults to `"#70C8B8"`.
#' @param base_color_title Character. Hex color code for plot title and legend
#'   title text. Defaults to `"#03617A"`.
#' @param title_text_size Numeric. Font size for plot title text. Defaults to
#'   `base_size * 1.1`.
#' @param subtitle_text_size Numeric. Font size for plot subtitle text. Defaults
#'   to `base_size * 1.05`.
#' @param caption_color Character. Hex color code for plot caption text.
#'   Defaults to `"#19405B"`.
#' @param legend_position Character. Legend position on the plot. Accepts "top",
#'   "bottom", "left", or "right". Defaults to `"top"`.
#' @param vjust_title Numeric. Vertical justification of the plot title.
#'   Defaults to `0`.
#' @param vjust_subtitle Numeric. Vertical justification of the plot subtitle.
#'   Defaults to `0`.
#' @param hjust_title Numeric. Horizontal justification of the plot title.
#'   Defaults to `0`.
#' @param hjust_subtitle Numeric. Horizontal justification of the plot subtitle.
#'   Defaults to `0`.
#' @param axis_lines Logical. If `TRUE`, axis lines are drawn in `base_color`;
#'   otherwise, they are hidden. Defaults to `FALSE`.
#' @param facets Logical. If `TRUE`, additional formatting for facet plots is
#'   applied. Defaults to `FALSE`.
#' @param facet_text_size Numeric. If `facets = TRUE`, size formatting for facet
#'   text (`strip.text`) is applied. Defaults to `base_size`.
#' @param draw_panel_border Logical. If `TRUE`, a border is drawn around panels
#'   in facet plots. Defaults to `FALSE`.
#' @param ... Additional arguments passed to `ggplot2::theme` for further
#'   customization.
#'
#' @returns A `ggplot2` theme object that can be applied to plots.
#'
#' @details The function customizes common plot elements like axis text, titles,
#' subtitles, captions, legend, and facet labels. It is designed to work with
#' `ggplot2` plots, providing a clean and professional look with minimal
#' styling. You can adjust various aesthetic features such as font size, color,
#' and legend position for maximum control over the appearance.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
#' @examples
#' # Create a ggplot2 plot with the theme_cleaner theme
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   theme_cleaner(
#'     base_size = 14,
#'     title_text_size = 16,
#'     legend_position = "bottom"
#'   )
#'
#' # Customize facet plots with theme_cleaner
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   facet_wrap(~cyl) +
#'   theme_cleaner(facets = TRUE,
#'   facet_text_size = 12,
#'   draw_panel_border = TRUE
#'   )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
theme_cleaner <-
  function(
    base_size = 12,
    base_family = "sans",
    base_color = "#70C8B8",
    base_color_title = "#03617A",
    title_text_size = ceiling(base_size * 1.1),
    subtitle_text_size = ceiling(base_size * 1.05),
    caption_color = "#19405B",
    legend_position = "top",
    vjust_title = 0,
    vjust_subtitle = 0,
    hjust_title = 0,
    hjust_subtitle = 0,
    axis_lines = FALSE,
    facets = FALSE,
    facet_text_size = base_size,
    draw_panel_border = FALSE,
    ...
  ) {
    # If facets are not enabled, apply the following theme settings
    if (!facets) {
      ggplot2::theme(
        # Remove axis ticks
        axis.ticks = ggplot2::element_blank(),
        # Remove background for the legend
        legend.background = ggplot2::element_blank(),
        # Remove background for the legend keys
        legend.key = ggplot2::element_blank(),
        # Remove background for the panel
        panel.background = ggplot2::element_blank(),
        # Remove border for the panel
        panel.border = ggplot2::element_blank(),
        # Remove background for the strip (facet labels)
        strip.background = ggplot2::element_blank(),
        # Remove background for the plot
        plot.background = ggplot2::element_blank(),
        # Indicate that this theme is complete and should override other themes
        complete = TRUE,
        # Set text properties for the plot
        text = ggplot2::element_text(
          family = base_family, # Font family
          face = "bold", # Font face (bold)
          color = base_color # Font color
        ),

        # Adjust font color for various plot elements

        # Set font color and size for axis text
        axis.text = ggplot2::element_text(
          color = base_color, # Color for axis text
          size = base_size # Font size for axis text
        ),

        # Set font color and size for axis titles
        axis.title = ggplot2::element_text(
          color = base_color_title, # Color for axis titles
          size = base_size # Font size for axis titles
        ),

        # Set font color and size for legend titles
        legend.title = ggplot2::element_text(
          color = base_color_title, # Color for legend titles
          size = base_size # Font size for legend titles
        ),

        # Set font color and size for legend text
        legend.text = ggplot2::element_text(
          color = base_color, # Color for legend text
          size = base_size # Font size for legend text
        ),

        # Set font color, size, and alignment for plot title
        plot.title = ggplot2::element_text(
          color = base_color_title, # Color for plot title
          size = title_text_size, # Font size for plot title
          face = "bold", # Font face (bold) for plot title
          hjust = hjust_title, # Horizontal justification for plot title
          vjust = vjust_title # Vertical justification for plot title
        ),

        # Set font color, size, and alignment for plot subtitle
        plot.subtitle = ggplot2::element_text(
          color = base_color, # Color for plot subtitle
          size = subtitle_text_size, # Font size for plot subtitle
          hjust = hjust_subtitle, # Horizontal justification for plot subtitle
          vjust = vjust_subtitle # Vertical justification for plot subtitle
        ),

        # Set font color, size, and alignment for plot caption
        plot.caption = ggplot2::element_text(
          size = base_size, # Font size for plot caption
          color = caption_color, # Color for plot caption
          hjust = 0 # Horizontal justification for plot caption
        ),
        # Set the position of the legend
        legend.position = legend_position,

        # Center the legend within its position
        legend.justification = "center",

        # Remove all grid lines from the panel
        panel.grid = ggplot2::element_blank(),

        # Remove major grid lines from the panel
        panel.grid.major = ggplot2::element_blank(),

        # Remove minor grid lines from the panel
        panel.grid.minor = ggplot2::element_blank(),

        # Set the color of the axis lines
        axis.line = ggplot2::element_line(
          color = dplyr::if_else(
            axis_lines == TRUE, # Check if axis lines should be drawn
            base_color, # Use base_color if axis lines are enabled
            "transparent" # Otherwise, make axis lines transparent
          )
        ),

        # Additional arguments passed to ggplot2::theme for customization
        ...
      )

      # If facets are enabled, apply the following theme settings
    } else if (facets) {
      ggplot2::theme(
        # Remove axis ticks
        axis.ticks = ggplot2::element_blank(),
        # Remove background for the legend
        legend.background = ggplot2::element_blank(),
        # Remove background for the legend keys
        legend.key = ggplot2::element_blank(),
        # Remove background for the panel
        panel.background = ggplot2::element_blank(),
        # Set border for the panel based on draw_panel_border parameter
        panel.border = ggplot2::element_rect(
          color = dplyr::if_else(
            draw_panel_border == FALSE, # Check if panel border should be drawn
            "transparent", # Make border transparent if not drawn
            base_color # Use base_color if border is drawn
          ),
          fill = NA # No fill for the panel border
        ),
        # Remove background for the plot
        plot.background = ggplot2::element_blank(),
        # Indicate that this theme is complete and should override other themes
        complete = TRUE,
        # Set text properties for the plot
        text = ggplot2::element_text(
          family = base_family, # Font family
          face = "bold", # Font face (bold)
          color = base_color # Font color
        ),
        # Adjust font color for various plot elements

        # Set font color and size for axis text
        axis.text = ggplot2::element_text(
          color = base_color, # Color for axis text
          size = base_size # Font size for axis text
        ),

        # Set font color and size for axis titles
        axis.title = ggplot2::element_text(
          color = base_color_title, # Color for axis titles
          size = base_size # Font size for axis titles
        ),

        # Set font color and size for legend titles
        legend.title = ggplot2::element_text(
          color = base_color_title, # Color for legend titles
          size = base_size # Font size for legend titles
        ),

        # Set font color and size for legend text
        legend.text = ggplot2::element_text(
          color = base_color, # Color for legend text
          size = base_size # Font size for legend text
        ),

        # Set font color, size, and alignment for plot title
        plot.title = ggplot2::element_text(
          color = base_color_title, # Color for plot title
          size = title_text_size, # Font size for plot title
          face = "bold", # Font face (bold) for plot title
          hjust = hjust_title, # Horizontal justification for plot title
          vjust = vjust_title # Vertical justification for plot title
        ),

        # Set font color, size, and alignment for plot subtitle
        plot.subtitle = ggplot2::element_text(
          color = base_color, # Color for plot subtitle
          size = subtitle_text_size, # Font size for plot subtitle
          hjust = hjust_subtitle, # Horizontal justification for plot subtitle
          vjust = vjust_subtitle # Vertical justification for plot subtitle
        ),

        # Set font color, size, and alignment for plot caption
        plot.caption = ggplot2::element_text(
          size = base_size, # Font size for plot caption
          color = caption_color, # Color for plot caption
          hjust = 0 # Horizontal justification for plot caption
        ),
        # Set the position of the legend
        legend.position = legend_position,

        # Center the legend within its position
        legend.justification = "center",

        # Remove all grid lines from the panel
        panel.grid = ggplot2::element_blank(),

        # Remove major grid lines from the panel
        panel.grid.major = ggplot2::element_blank(),

        # Remove minor grid lines from the panel
        panel.grid.minor = ggplot2::element_blank(),

        # Set the color of the axis lines
        axis.line = ggplot2::element_line(
          color = dplyr::if_else(
            axis_lines == TRUE, # Check if axis lines should be drawn
            base_color, # Use base_color if axis lines are enabled
            "transparent" # Otherwise, make axis lines transparent
          )
        ),

        # Set the background color for facet strips
        strip.background = ggplot2::element_rect(
          fill = "#2A6357", # Background color for facet strips
          color = "#B9E1DA" # Border color for facet strips
        ),

        # Set the text properties for facet strips
        strip.text = ggplot2::element_text(
          color = "ghostwhite", # Text color for facet strips
          size = facet_text_size # Font size for facet strips
        ),

        # Additional arguments passed to ggplot2::theme for customization
        ...
      )
    }
  }
