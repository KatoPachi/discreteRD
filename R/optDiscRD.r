#' Manipulation Global Options for {discRD}
#'
#' @param basicmod two-sided formula with
#' outcome variables on lhs and running variable on rhs
#' @param xmod (list of) one-sided formulas with
#' covariates used in regression analysis on rhs.
#' @param \dots Specify `option_name = option`
#'
#' @details If empty arguments, return current options.
#'
#' @export
#' @examples
#' \dontrun{
#' set_optDiscRD(y1 + y2 ~ run, discRD.cutoff = 45)
#' clear_optDiscRD()
#' set_optDiscRD()
#' }
#'
set_optDiscRD <- function(basicmod, xmod, ...) {
  # collect arguments
  args <- list(...)

  # parse basicmod and add arguments
  if (!missing(basicmod)) {
    # parse basicmod and add arguments
    parse_basicmod <- parse_model(basicmod)
    args$discRD.outcome <- parse_basicmod$lhs
    if (length(parse_basicmod$rhs) != 1) stop("Only one running variable.")
    args$discRD.running <- parse_basicmod$rhs
  }

  if (!missing(xmod)) {
    # parse xmod and add arguments
    xmod <- if (!is.list(xmod)) list(xmod) else xmod
    xlist <- unique(unlist(lapply(xmod, all.vars)))
    args$discRD.xmod <- xmod
    args$discRD.xlist <- xlist
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

#' Clear Global Options for RCTtoolbox
#'
#' @export
#' @examples
#' \dontrun{
#' set_optDiscRD(y1 + y2 ~ run, discRD.cutoff = 45)
#' clear_optDiscRD()
#' set_optDiscRD()
#' }
#'
clear_optDiscRD <- function() {
  # default option list
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
  # collect current option name
  opt <- names(options())
  # check whether options specified in arguments register
  ok <- names(opt_pkg) %in% opt
  # register options if ok
  options(opt_pkg[ok])
}