#' Methods of tidy and glance
#'
#' @param x object
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
#' @method tidy local_ate_local_lm
#' @export
#' @name tidy.local_ate_global_lm
tidy.local_ate_local_lm <- function(x, ...) {
  tidy.local_ate_global_lm(x, ...)
}

#'
#' @importFrom generics glance
#' @method glance local_ate_global_lm
#' @export
#' @name tidy.local_ate_global_lm
glance.local_ate_global_lm <- function(x, ...) {
  data.frame(
    nobs = x$treat$N + x$control$N,
    se = x$treat$se.type
  )
}

#'
#' @method glance local_ate_local_lm
#' @export
#' @name tidy.local_ate_global_lm
glance.local_ate_local_lm <- function(x, ...) {
  data.frame(
    nobs = x$treat$N + x$control$N,
    "Effective Num.Obs." = x$method$effective.nobs,
    se = x$treat$se.type,
    kernel = x$method$kernel,
    bandwidth = x$method$bandwidth
  )
}