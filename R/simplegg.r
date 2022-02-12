#' Template for {ggplot2}
#'
#' @param title_size numeric. Specify font size of plot title.
#' @param axis_title_size numeric. Specify font size of axis title.
#' @param axis_text_size numeric. Specify font size of axis texts.
#' @param legend_title_size numeric. Specify font size of legend title.
#' @param legend_text_size numeric. Specify font size of legend texts.
#' @param caption_size numeric. Specify font size of caption.
#' @param legend_key_size numeric. Specify key size of legends
#' @param font_family character. Specify font family. Default is NULL.
#' @param flip logical. Specify whether to swap the x-axis and y-axis.
#'
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom grid unit
#' @importFrom ggplot2 element_line
#'
#' @export
#'
simplegg <- function(
  title_size = 13,
  axis_title_size = 13,
  axis_text_size = 12,
  legend_title_size = 12,
  legend_text_size = 12,
  caption_size = 11,
  legend_key_size = 1,
  font_family = NULL,
  flip = FALSE) {
  my_theme <- ggplot2::theme_minimal(base_family = font_family) +
    ggplot2::theme(
      # panel grid
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      # axis
      axis.text = ggplot2::element_text(
        size = axis_text_size, family = font_family
      ),
      axis.title = ggplot2::element_text(
        size = axis_title_size, family = font_family
      ),
      axis.ticks.length = grid::unit(0.25, "cm"),
      axis.ticks.x = ggplot2::element_line(),
      axis.ticks.y = ggplot2::element_line(),
      axis.line = ggplot2::element_line(),
      # legend
      legend.text = ggplot2::element_text(
        size = legend_text_size, family = font_family
      ),
      legend.key.size = grid::unit(legend_key_size, "cm"),
      legend.title = ggplot2::element_text(
        size = legend_title_size, family = font_family
      ),
      legend.position = "bottom",
      # facet_wrap
      strip.text = ggplot2::element_text(
        size = axis_title_size, family = font_family
      ),
      # caption
      plot.caption = ggplot2::element_text(
        size = caption_size, family = font_family
      )
    )

  if (flip) {
    my_theme <- my_theme +
      theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line()
      )
  }

  return(my_theme)
}
