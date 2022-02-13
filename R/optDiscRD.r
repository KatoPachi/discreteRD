#' Manipulation Global Options for {discRD}
#'
#' @param basemod two-sided formula with
#'   outcome variables on lhs and running variable on rhs
#' @param covmod (list of) one-sided formulas with
#'   covariates used in regression analysis on rhs.
#' @param \dots Other global options as follows: \itemize{
#'   \item `discRD.cutoff`: a numeric value of cutoff point.
#'   \item `discRD.assign`: a string of assignment rule.
#'     If "greater", treatment is determined by 1*(running >= cutoff).
#'     If "smaller", treatment is determined by 1*(running <= cutoff).
#'   \item `discRD.plot_family`: The name of the font family
#'     used to output the figure with {ggplot2}.
#'   \item `discRD.table_output`: String of output format with {modelsummary}.
#'     Default is "kableExtra".
#'   \item `discRD.table_fontsize`: The font size in the table
#'     output by {kableExtra} and {flextable} used through {modelsummary}.
#' }
#'
#' @details If `set_optDiscRD` passes empty arguments, return current options.
#'   `clear_optDiscRD` reset the global option value to the initial one.
#'
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom stats formula
#' @export
#' @examples
#' \dontrun{
#' set_optDiscRD(y1 + y2 ~ run, discRD.cutoff = 45)
#' clear_optDiscRD()
#' set_optDiscRD()
#' }
#'
set_optDiscRD <- function(basemod, covmod, ...) {
  # collect arguments
  args <- list(...)

  # parse basicmod and add arguments
  if (!missing(basemod)) {
    lhs <- all.vars(rlang::f_lhs(basemod))
    rhs <- all.vars(rlang::f_rhs(basemod))
    if (length(rhs) > 1) stop("Only one running variable.")
    args$discRD.basemod <- lapply(lhs, function(x) {
      formula(paste0(x, "~", rhs))
    })
  }

  if (!missing(covmod)) {
    # parse xmod and add arguments
    if (!is.list(covmod)) covmod <- list(covmod)
    unique_x <- unique(unlist(lapply(covmod, all.vars)))
    args$discRD.covmod <- covmod
    args$discRD.unique.covariate <- unique_x
  }

  # collect current option name
  opt <- names(options())

  # check whether options specified in arguments register
  ok <- names(args) %in% opt

  # register options if ok
  options(args[ok])

  # show registered options
  options()[grep("discRD.", names(options()))]
}

#'
#' @export
#' @name set_optDiscRD
clear_optDiscRD <- function() {
  # default option list
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
  # collect current option name
  opt <- names(options())
  # check whether options specified in arguments register
  ok <- names(opt_pkg) %in% opt
  # register options if ok
  options(opt_pkg[ok])
}