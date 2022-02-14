#' Global Linear Least Squares to Estimate Local ATE
#'
#' @description Estimate the local ATE
#'   at the treatment assignment threshold
#'   by the global least squares method.
#'
#' @param basemod baseline formula. `outcome ~ running variable`.
#' @param covmod (list of) one-sided formula with
#'   covariates on rhs.
#'   If NULL, covariates are not controlled.
#'   If missing, find `options("discRD.xmod")`
#' @param data data.frame which you want to use.
#' @param subset subset condition.
#' @param weights weight variable.
#' @param cluster cluster variable.
#' @param submod numeric vector.
#'   Which baseline model you want to use.
#' @param onlydmod logical (default is TRUE).
#'   Whether to estimate a model without covariates.
#' @param order numeric vector of global polynomial orders.
#' @param cutoff numeric of cutoff points
#'   If missing, try to find `getOption("discRD.cutoff")`.
#' @param assign assignment rule of treatment.
#'   If "greater",
#'   treated whose running variable is greater than or equal to cutoff.
#'   If "smaller",
#'   treated whose running variable is less than or equal to cutoff.
#'   If missing, try to find `getOption("discRD.assign")`.
#' @param se_type character.
#'   Calculate robust variance-covariance matrix
#'   ("HC0", "HC1", "HC2", "HCj", "HC3", and "HC4")
#' @param cholesky logical (default is TRUE).
#'   When solving normal equation, use cholesky decomposition.
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
#' @importFrom stats pnorm
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
#'   covmod = list(~cov1, ~ cov1 + cov2),
#'   discRD.cutoff = 50,
#'   discRD.assign = "smaller"
#' )
#'
#' est <- global_lm(data = raw)
#' str(global_lm(submod = 1, data = raw))
#' str(global_lm(submod = 1, order = 3, data = raw))
#' str(global_lm(data = raw, cutoff = 30))
#' est2 <- global_lm(data = raw, hc = "HC1", weights = w)
#'
global_lm <- function(basemod,
                      covmod,
                      data,
                      weights,
                      subset,
                      submod,
                      onlydmod = TRUE,
                      order = c(1, 2),
                      cutoff,
                      assign,
                      se_type = "HC0",
                      cholesky = TRUE) {
  # check basemod and covmod if missing
  if (missing(basemod)) basemod <- getOption("discRD.basemod")
  if (!is.list(basemod)) basemod <- list(basemod)
  if (length(basemod) == 0) stop("Not find basemod")

  if (missing(covmod)) covmod <- getOption("discRD.covmod")
  if (!is.list(covmod)) covmod <- list(covmod)
  if (onlydmod) covmod <- append(list(""), covmod)

  # model list
  if (missing(submod)) submod <- seq_len(length(basemod))
  usemod <- basemod[submod]

  mod <- expand.grid(basemod = usemod, covmod = covmod, order = order)
  mod <- mod[!duplicated(mod), ]
  if (nrow(mod) == 0) stop("Cannot construct model")

  # data cleaning
  dtarg <- rlang::enquos(
    subset = subset,
    weights = weights,
    cluster = cluster,
    cutoff = cutoff,
    assign = assign
  )

  dtarg <- Filter(Negate(rlang::quo_is_missing), dtarg)

  clean <- lapply(seq_len(nrow(mod)), function(i) {
    dtarg$basemod <- mod[i, "basemod"][[1]]
    coveq <- mod[i, "covmod"][[1]]
    if (coveq != "") dtarg$covmod <- coveq
    do.call("clean_rd_data", dtarg)
  })

  # order of polynomial
  estdt <- lapply(seq_len(nrow(mod)), function(i) {
    # polynomial order
    dt <- clean[[i]]$data
    if (mod[i, "order"] > 1) {
      for (j in seq(2, mod[i, "order"])) {
        o <- j
        lab <- paste0("x", o)
        dt[[lab]] <- dt$x ^ o
      }
    }
    dt
  })

  # estimation in treated and control
  estarg_name <- c("cholesky", "hc")
  estarg <- arg[names(arg) %in% estarg_name]

  est <- lapply(seq_len(nrow(mod)), function(i) {
    # subset
    d1 <- estdt[[i]][estdt[[i]]$d == 1, ]
    d0 <- estdt[[i]][estdt[[i]]$d == 0, ]

    # variables
    yvar <- rlang::f_lhs(mod[i, "basemod"][[1]])
    exclude_x <- c(as.character(yvar), "(weights)", "d")

    # estimation in treated data
    estarg$y <- rlang::eval_tidy(yvar, d1)
    design <- as.matrix(d1[, !(names(d1) %in% exclude_x)])
    colnames(design) <- names(d1)[!(names(d1) %in% exclude_x)]
    estarg$x <- design
    if (!is.null(d1$"(weights)")) estarg$w <- d1$"(weights)"
    treat <- do.call("lm_internal", estarg)

    # estimation in control data
    estarg$y <- rlang::eval_tidy(yvar, d0)
    design <- as.matrix(d0[, !(names(d0) %in% exclude_x)])
    colnames(design) <- names(d0)[!(names(d0) %in% exclude_x)]
    estarg$x <- design
    if (!is.null(d0$"(weights)")) estarg$w <- d0$"(weights)"
    ctrl <- do.call("lm_internal", estarg)

    # local ATE
    ate <- treat$estimate[1, 1] - ctrl$estimate[1, 1]
    ate_se <- sqrt(treat$vcov[1, 1] + ctrl$vcov[1, 1])
    ate_z <- abs(ate) / ate_se
    ate_p <- 2 * pnorm(ate_z, lower.tail = FALSE)
    ate_mat <- matrix(c(ate, ate_se, ate_z, ate_p), nrow = 1)
    colnames(ate_mat) <- c("Estimate", "Std.Err.", "z", "P(>|z|)")
    rownames(ate_mat) <- c("Local ATE")

    estlist <- list(
      outcome = as.character(yvar),
      treat = treat,
      control = ctrl,
      local.ate = ate_mat
    )
    class(estlist) <- "local_ate_global_lm"
    estlist
  })

  # output
  output <- list()
  output$RD.info <- clean[[1]]$RD.info
  output$model.outline <- mod
  output$result <- est
  class(output) <- append("global_lm", class(output))
  output
}
