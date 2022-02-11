#' Useful Wrapper Functions for Discrete Regression Discontinuity Design
#'
#' @section Package Options:
#'
#' discreteRD provides the following [options()]
#' to facilitate consistent analysis:
#' \itemize{
#'   \item `discRD.outcome`: a string vector with outcome variables.
#'   \item `discRD.running`: a string of running variable.
#'   \item `discRD.cutoff`: a numeric value of cutoff point.
#'   \item `discRD.assign`: a string of assignment rule.
#'     If "greater", treatment is determined by 1*(running >= cutoff).
#'     If "smaller", treatment is determined by 1*(running <= cutoff).
#'   \item `discRD.xmod`: A list of one-sided formulas with covariates
#'     on the right-hand side of the equation.
#'     See `rct_lm()` in detail.
#'   \item `discRD.xlist`: String vector with covariates.
#'   \item `discRD.plot_family`: The name of the font family
#'     used to output the figure with {ggplot2}.
#'   \item `discRD.table_output`: String of output format with {modelsummary}.
#'     Default is "kableExtra".
#'   \item `discRD.table_fontsize`: The font size in the table
#'     output by {kableExtra} and {flextable} used through {modelsummary}.
#' }
#'
#' @docType package
#' @name discreteRD
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_pkg <- list(
    discRD.outcome = "",
    discRD.running = "",
    discRD.cutoff = 0,
    discRD.assign = "greater",
    discRD.xmod = "",
    discRD.xlist = "",
    discRD.plot_family = "",
    discRD.table_output = "kableExtra",
    discRD.table_fontsize = 15
  )
  toset <- !(names(opt_pkg) %in% names(opt))
  if (any(toset)) options(opt_pkg[toset])

  invisible()
}
