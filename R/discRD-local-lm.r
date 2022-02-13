#' Local Linear Regression to Estimate Local ATE
#'
#' @description Estimate the local ATE
#'   at the treatment assignment threshold
#'   by the local linear regression.
#'
#' @param basemod baseline formula. `outcome ~ running variable`.
#' @param data data.frame which you want to use.
#' @param weights weight variable.
#' @param subset subset condition.
#' @param submod numeric vector.
#'   Which baseline model you want to use.
#' @param order numeric vector of global polynomial orders.
#' @param cutoff numeric of cutoff points
#'   If missing, try to find `getOption("discRD.cutoff")`.
#' @param assign assignment rule of treatment.
#'   If "greater",
#'   treated whose running variable is greater than or equal to cutoff.
#'   If "smaller",
#'   treated whose running variable is less than or equal to cutoff.
#'   If missing, try to find `getOption("discRD.assign")`.
#' @param cholesky logical (default is TRUE).
#'   When solving normal equation, use cholesky decomposition.
#' @param hc character.
#'   Calculate robust variance-covariance matrix ("HC0" or "HC1")
#' @param bw bandwidth.
#' @param kernel character of kernel density ("uniform" or "triangular")
#' @param point value of the running variable that
#'   the kernel weights weigh the most
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
#' est <- local_lm(data = raw, bw = 3, kernel = "uniform")
#' str(local_lm(submod = 1, data = raw, bw = 3, kernel = "uniform"))
#' str(local_lm(submod = 1, order = 3, data = raw, bw = 3, kernel = "uniform"))
#' str(local_lm(data = raw, cutoff = 30, bw = 3, kernel = "uniform"))
#' est2 <- local_lm(
#' data = raw, hc = "HC1", weights = w, bw = 3, kernel = "uniform"
#' )
#'
local_lm <- function(basemod,
                     data,
                     weights,
                     subset,
                     submod,
                     order = c(1, 2),
                     cutoff,
                     assign,
                     cholesky = TRUE,
                     hc = "HC0",
                     bw,
                     kernel,
                     point = 0) {
  # collect arguments
  arg <- as.list(match.call())[-1]
  arg$data <- data

  # check basemod and covmod if missing
  if (missing(basemod)) basemod <- getOption("discRD.basemod")
  if (!is.list(basemod)) basemod <- list(basemod)
  if (length(basemod) == 0) stop("Not find basemod")

  # model list
  if (missing(submod)) submod <- seq_len(length(basemod))
  usemod <- basemod[submod]

  mod <- expand.grid(basemod = usemod, order = order)
  mod <- mod[!duplicated(mod), ]
  if (nrow(mod) == 0) stop("Cannot construct model")

  # data cleaning
  dt_arg_list_name <- c("data", "weights", "subset", "cutoff", "assign")
  dtarg <- arg[names(arg) %in% dt_arg_list_name]

  clean <- lapply(seq_len(nrow(mod)), function(i) {
    dtarg$basemod <- mod[i, "basemod"][[1]]
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

  # kernel weight
  estdt <- lapply(estdt, function(dt) {
    u <- abs(dt$x - point) / bw
    kw <- switch(kernel,
      "triangular" = ifelse(u <= 1, (1 - u), 0),
      "uniform" = ifelse(u <= 1, 1 / 2, 0)
    )
    if (is.null(dt$"(weights)")) dt$"(weights)" <- 1
    dt$"(weights)" <- dt$"(weights)" * kw
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
    exclude_x <- c(as.character(yvar), "(weights)", "d", "kw")

    # estimation in treated data
    estarg$y <- rlang::eval_tidy(yvar, d1)
    design <- as.matrix(d1[, !(names(d1) %in% exclude_x)])
    colnames(design) <- names(d1)[!(names(d1) %in% exclude_x)]
    estarg$x <- design
    estarg$w <- d1$"(weights)"
    treat <- do.call("lm_internal", estarg)

    # estimation in control data
    estarg$y <- rlang::eval_tidy(yvar, d0)
    design <- as.matrix(d0[, !(names(d0) %in% exclude_x)])
    colnames(design) <- names(d0)[!(names(d0) %in% exclude_x)]
    estarg$x <- design
    estarg$w <- d0$"(weights)"
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
      local.ate = ate_mat,
      method = list(
        kernel = kernel,
        bandwidth = bw,
        effective.nobs = nrow(d1[d1$"(weights)" > 0, ]) +
          nrow(d0[d0$"(weights)" > 0, ])
      )
    )
    class(estlist) <- "local_ate_local_lm"
    estlist
  })

  # output
  output <- list()
  output$RD.info <- clean[[1]]$RD.info
  output$model.outline <- mod
  output$result <- est
  class(output) <- append("local_lm", class(output))
  output
}
