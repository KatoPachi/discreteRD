#' Methods of tidy and glance
#'
#' @param x object
#' @param \dots other arguments
#'
#' @importFrom generics tidy
#' @export
#'
tidy.global_lm <- function(x, ...) {
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
#' @export
#' @name tidy.global_lm
tidy.local_lm <- function(x, ...) {
  tidy.global_lm(x, ...)
}

#'
#' @importFrom generics glance
#' @export
#' @name tidy.global_lm
glance.global_lm <- function(x, ...) {
  data.frame(
    nobs = x$treat$N + x$control$N,
    se = x$treat$vcov$type
  )
}

#'
#' @importFrom tibble tibble
#' @export
#' @name tidy.global_lm
glance.local_lm <- function(x, ...) {
  n <- x$treat$N + x$control$N
  efn <- x$treat$input$local.wls$effective.nobs +
    x$control$input$local.wls$effective.nobs
  tibble::tibble(
    nobs = n,
    "Effective Num.Obs." = sprintf("%1d", efn),
    se = x$treat$vcov$type,
    kernel = x$treat$input$local.wls$kernel,
    bandwidth = x$treat$input$local.wls$bandwidth
  )
}