#' Parse Model
#'
#' @param mod formula
#'
#' @importFrom stats terms
#'
#'
parse_model <- function(mod) {
  full_var <- all.vars(mod)
  rhs <- attr(terms(mod), "term.labels")
  lhs <- full_var[!(full_var %in% rhs)]
  list(lhs = lhs, rhs = rhs)
}
