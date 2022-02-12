#' Global Linear Least Squares to Estimate Local ATE
#'
#' @description Estimate the local ATE
#'   at the treatment assignment threshold
#'   by the global least squares method
#'   using the [lm_robust()] function of the {estimatr} package.
#'
#' @param y string vector with outcome variables.
#'   If missing, find `options("discRD.outcome")`
#' @param covmod (list of) one-sided formula with
#'   covariates on rhs.
#'   If NULL, covariates are not controlled.
#'   If missing, find `options("discRD.xmod")`
#' @param data data.frame which you want to use.
#' @param onlydmod logical (default is TRUE).
#'   Whether to estimate a model without covariates.
#' @param order numeric vector of global polynomial orders.
#' @param subset_outcome numeric vector.
#'   Specify position of outcome variables you want to use
#'   when using `options("discRD.outcome")`.
#' @param \dots other arguments. See details.
#'
#' @details \dots can pass lm_robust arguments
#'   and some data formatting arguments.
#'   There are three arguments for data formatting.
#'   "x" is a string of running variables.
#'   "cutoff" is the threshold of the running variable
#'   in the treatment assignment.
#'   "assign" is a rule for assigning treatments.
#'   If `assign ="smaller"`, receive treatment
#'   when the running variable is less than or equal to
#'   the cutoff value.
#'   If `assign ="greater"`,
#'   receive treatment when the running variable is
#'   greater than or equal to the cutoff value.
#'   When using "weight" to run weighted least squares,
#'   specify a string of weight variable.
#'   When you don't specify "se_type",
#'   use "HC1" to calculate standard errors.
#'
#' @importFrom stats formula
#' @importFrom stats update
#' @importFrom estimatr lm_robust
#' @export
#' @examples
#' running <- sample(1:100, size = 1000, replace = TRUE)
#' cov1 <- rnorm(1000, sd = 2); cov2 <- rnorm(1000, mean = -1)
#' y0 <- running + cov1 + cov2 + rnorm(1000, sd = 10)
#' y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000, sd = 10)
#' y <- ifelse(running <= 50, y1, y0)
#' bin <- ifelse(y > mean(y), 1, 0)
#' w <- sample(c(1, 0.5), size = 1000, replace = TRUE)
#' raw <- data.frame(y, bin, running, cov1, cov2, w)
#'
#' set_optDiscRD(
#'   y + bin ~ running,
#'   xmod = list(~cov1, ~ cov1 + cov2),
#'   discRD.cutoff = 50,
#'   discRD.assign = "smaller"
#' )
#'
#' est <- global_lm(data = raw)
#' global_lm(subset_outcome = 1, data = raw)
#' global_lm(subset_outcome = 1, order = 3, data = raw)
#' global_lm(data = raw, cutoff = 30)
#' est2 <- global_lm(data = raw, se_type = "HC0", weight = "w")
#' summary(est2$res[[1]])
#' est3 <- global_lm(data = raw, cluster = raw$running, se_type = "stata")
#' summary(est3$res[[1]])
#'
global_lm <- function(
  y, covmod, data, onlydmod = TRUE, order = c(1, 2), subset_outcome, ...
) {
  # collect arguments
  args <- list(...)
  name_args_data <- c("x", "cutoff", "assign", "weight")
  pass_args_data <- args[names(args) %in% name_args_data]
  pass_args_data <- Filter(Negate(is.null), pass_args_data)
  pass_args_lm <- args[!(names(args) %in% name_args_data[1:3])]
  pass_args_lm <- Filter(Negate(is.null), pass_args_lm)

  # check global options for outcomes
  if (missing(y)) {
    y <- getOption("discRD.outcome")
    if (all(y == "")) stop("Specify outcome in argument or global option.")
  }
  if (missing(subset_outcome)) subset_outcome <- seq_len(length(y))
  y <- y[subset_outcome]

  # check global options for covariates
  if (missing(covmod)) covmod <- getOption("discRD.xmod")
  if (onlydmod) covmod <- append(list(""), covmod)
  cov <- lapply(covmod, all.vars)

  # generate model frame
  mod <- expand.grid(
    y = y,
    cov = cov,
    o = order,
    stringsAsFactors = FALSE
  )
  mod <- mod[!duplicated(mod), ]

  # estimation
  estimation <- lapply(seq_len(nrow(mod)), function(i) {
    # model component
    comp <- mod[i, ]
    # data shape
    data_args <- list(
      data = data,
      y = comp$y,
      cov = comp$cov[[1]]
    )
    data_args <- append(pass_args_data, data_args)
    usedt <- do.call("shape_data", data_args)

    # model formula
    baseline <- formula(paste0(
      comp$y, "~ d * poly(x, ", comp$o, ", raw = TRUE)"
    ))
    covariate <- formula(paste(c(". ~ .", comp$cov[[1]]), collapse = "+"))
    fullmod <- update(baseline, covariate)

    # run lm_robust
    lm_args <- list(formula = fullmod, data = usedt$data)
    if (is.null(pass_args_lm$se_type)) pass_args_lm$se_type <- "HC1"
    if (!is.null(pass_args_lm$weight)) {
      pass_args_lm$weight <- usedt$data[[pass_args_lm$weight]]
    }
    lm_args <- append(pass_args_lm, lm_args)
    est <- do.call("lm_robust", lm_args, envir = getNamespace("estimatr"))
    est$RD.info <- usedt$RD.info
    est
  })

  # output
  out <- list(
    model.frame = mod,
    res = estimation
  )
  class(out) <- append("discRD_global_lm", class(out))
  out
}
