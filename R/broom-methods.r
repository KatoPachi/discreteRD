#' Methods of tidy and glance
#'
#' @param x object with local_ate_global_lm class
#' @param \dots other arguments
#'
#' @importFrom generics tidy
#' @method tidy local_ate_global_lm
#' @export
#'
tidy.local_ate_global_lm <- function(x, ...) {
  res <- x$local.ate
  data.frame(
    term = rownames(res),
    estimate = res[1, 1],
    std.error = res[1, 2],
    statistic = res[1, 3],
    p.value = res[1, 4],
    outcome = x$outcome
  )
}

#'
#' @importFrom generics glance
#' @method glance local_ate_global_lm
#' @export
glance.local_ate_global_lm <- function(x, ...) {
  data.frame(
    nobs = x$treat$N + x$control$N,
    se = x$treat$se.type
  )
}