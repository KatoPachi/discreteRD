#' Data Shape
#'
#' @param basemod baseline formula. `outcome ~ running variable`.
#' @param covmod one-sided formula with covariates on rhs.
#' @param data data.frame
#' @param subset subset condition.
#' @param weights weight variable.
#' @param cluster cluster variable.
#' @param order order of polynomial.
#' @param cutoff numeric of cutoff point.
#'   If missing, search `option("discRD.cutoff")`
#' @param assign assignment rule of treatment.
#'   If "greater",
#'   treated whose running variable is greater than or equal to cutoff.
#'   If "smaller",
#'   treated whose running variable is less than or equal to cutoff.
#'   If missing, search `option("discRD.assign")`
#' @param bw numeric vector of bandwidth.
#'   If specified, use data
#'   whose running variables are within this range will be used.
#'   If missing, use data from treatment and control groups
#'   where the running variable is closest to the cutoff
#' @param global logical (default is FALSE).
#'   Whether to use all observations.
#'
#' @importFrom stats update
#' @importFrom stats na.omit
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#' @importFrom stats model.frame
#'
#' @examples 
#' \dontrun{
#' running <- sample(1:100, size = 1000, replace = TRUE)
#' cov1 <- rnorm(1000, sd = 2); cov2 <- rnorm(1000, mean = -1)
#' y0 <- running + cov1 + cov2 + rnorm(1000, sd = 10)
#' y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000, sd = 10)
#' y <- ifelse(running <= 50, y1, y0)
#' bin <- ifelse(y > mean(y), 1, 0)
#' w <- sample(c(1, 0.5), size = 1000, replace = TRUE)
#' raw <- data.frame(y, bin, running, cov1, cov2, w)
#'
#' set_optDiscRD(discRD.cutoff = 50, discRD.assign = "smaller")
#' a <- clean_rd_data(y ~ running + cov1, data = raw, weights = w, order = 2)
#' str(a)
#' }
#'
#'
clean_rd_data <- function(basemod,
                          covmod,
                          data,
                          subset,
                          weights,
                          cluster,
                          order = 1,
                          cutoff,
                          assign,
                          bw,
                          global = TRUE) {
  ## make formula
  mod <- basemod
  if (!missing(covmod)) { 
    mod <- update(mod, paste0(c(". ~ .", all.vars(covmod)), collapse = "+"))
  }

  ## weight, cluster and subset condition vector
  wv <- NULL
  if (!missing(weights)) {
    if (!rlang::is_call(weights)) weights <- rlang::enquo(weights)
    wv <- rlang::eval_tidy(weights, data)
  }

  gv <- NULL
  if (!missing(cluster)) {
    if (!rlang::is_call(cluster)) cluster <- rlang::enquo(cluster)
    gv <- rlang::eval_tidy(cluster, data)
  }

  tfv <- NULL
  if (!missing(subset)) {
    if (!rlang::is_call(subset)) subset <- rlang::enquo(subset)
    tfv <- rlang::eval_tidy(subset, data)
  }

  ## clean data by model.frame
  args <- list(
    formula = mod,
    data = data,
    subset = tfv,
    weights = wv,
    cluster = gv,
    na.action = na.omit
  )

  clean <- do.call("model.frame", args)

  # shape clean_dt to adopt RD desing easily
  running <- all.vars(basemod)[2]

  if (missing(cutoff)) {
    cutoff <- getOption("discRD.cutoff")
    message("Use cutoff value registered by global options")
  }

  if (missing(assign)) {
    assign <- getOption("discRD.assign")
    message("Use assignment rule registered by global options")
  }

  usedt <- clean
  usedt$x <- usedt[, running] - cutoff

  if (order > 1) {
    for (j in seq(2, order)) {
      lab <- paste0("x", j)
      usedt[[lab]] <- usedt$x ^ j
    }
  }

  if (assign == "greater") {
    usedt$d <- ifelse(usedt$x >= 0, 1, 0)
  } else if (assign == "smaller") {
    usedt$d <- ifelse(usedt$x <= 0, 1, 0)
  } else {
    stop("Unknown assignment rule is used.")
  }

  usedt <- usedt[, names(usedt) != running]

  # temporal output list
  output <- list()
  output$data <- usedt
  output$RD.info <- list(
    running.variable = running,
    cutoff = cutoff,
    assignment = assign
  )

  # subset by bandwidth if global = FALSE
  if (!global) {
    if (missing(bw)) {
      if (assign == "greater") {
        lwr <- max(usedt[usedt$d == 0, "x"])
        upr <- 0
      } else if (assign == "smaller") {
        lwr <- 0
        upr <- min(usedt[usedt$d == 0, "x"])
      }
    } else {
      if (length(bw) == 1) {
        lwr <- -abs(bw)
        upr <- abs(bw)
      } else {
        lwr <- bw[1]
        upr <- bw[2]
      }
    }

    output$data <- usedt[lwr <= usedt$x & usedt$x <= upr, ]
    output$bw.subset <- list(global = FALSE, bw = c(lwr, upr))
  } else {
    output$bw.subset <- list(global = TRUE)
  }

  output

}
