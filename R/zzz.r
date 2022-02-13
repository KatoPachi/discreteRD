#' Useful Wrapper Functions for Discrete Regression Discontinuity Design
#'
#' @section Package Options:
#'
#' discreteRD provides the following [options()]
#' to facilitate consistent analysis:
#' \itemize{
#'   \item `discRD.basemod`: a list of formula with `outcome ~ running`. 
#'   \item `discRD.cutoff`: a numeric value of cutoff point.
#'   \item `discRD.assign`: a string of assignment rule.
#'     If "greater", treatment is determined by 1*(running >= cutoff).
#'     If "smaller", treatment is determined by 1*(running <= cutoff).
#'   \item `discRD.covmod`: A list of one-sided formulas with covariates
#'     on the right-hand side of the equation
#'   \item `discRD.unique.covariate`: String vector with unique covariates.
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
    discRD.basemod = list(),
    discRD.cutoff = 0,
    discRD.assign = "greater",
    discRD.covmod = list(),
    discRD.unique.covariate = "",
    discRD.plot_family = "",
    discRD.table_output = "kableExtra",
    discRD.table_fontsize = 15
  )
  toset <- !(names(opt_pkg) %in% names(opt))
  if (any(toset)) options(opt_pkg[toset])

  invisible()
}
