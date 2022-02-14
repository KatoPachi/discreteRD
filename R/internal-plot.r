#' Basic RD plot
#'
#' @param aggregate observed data aggregated by mass points
#' @param predict1 predicted data of treated
#' @param predict0 predicted data of control
#' @param cutoff cutoff value
#' @param ate_label In-plot text about local ATE estimates
#' @param ate_label_size In-plot text size
#' @param outcome_label Outcome label in plot title
#' @param ylim numeric vector of limits of y-axis
#' @param vjust numeric. Adjust in-plot text vertically
#' @param hjust numeric. Adjust in-plot text horizontally
#' @param xlab label of x-axis
#' @param ylab label of y-axis
#' @param \dots arguments of [simplegg()]
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ylim
#'
#' 
gplot_internal_cutoff <- function(aggregate,
                                  predict1,
                                  predict0,
                                  cutoff,
                                  ate_label,
                                  ate_label_size = 5,
                                  outcome_label,
                                  ylim,
                                  vjust = 0,
                                  hjust = 0,
                                  xlab = "Running variable",
                                  ylab = "Average",
                                  ...) {
  g <- ggplot2::ggplot(aggregate, ggplot2::aes(x = x, y = outcome)) +
    ggplot2::geom_point(ggplot2::aes(shape = d), size = 2) +
    ggplot2::geom_line(ggplot2::aes(x = x + cutoff, y = yhat1), predict1) +
    ggplot2::geom_line(ggplot2::aes(x = x + cutoff, y = yhat0), predict0) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = cutoff), linetype = 3) +
    ggplot2::annotate(
      "text", x = -Inf, y = Inf, label = ate_label,
      vjust = 1 + vjust, hjust = 0 + hjust, size = ate_label_size,
      family = getOption("discRD.plot_family")
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = outcome_label,
      shape = NULL
    )

  if (missing(ylim)) {
    g + simplegg(font_family = getOption("discRD.plot_family"), ...)
  } else {
    g + ggplot2::ylim(ylim) +
      simplegg(font_family = getOption("discRD.plot_family"), ...)
  }

}