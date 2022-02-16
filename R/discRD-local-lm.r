#' Local Linear Regression to Estimate Local ATE
#'
#' @description Estimate the local ATE
#'   at the treatment assignment threshold
#'   by the local linear regression.
#'
#' @param basemod baseline formula. `outcome ~ running variable`.
#' @param data data.frame which you want to use.
#' @param subset subset condition.
#' @param weights weight variable.
#' @param cluster cluster variable.
#' @param submod numeric vector.
#'   Which baseline model you want to use.
#' @param order numeric vector of polynomial orders.
#' @param cutoff numeric of cutoff points
#'   If missing, try to find `getOption("discRD.cutoff")`.
#' @param assign assignment rule of treatment.
#'   If "greater",
#'   treated whose running variable is greater than or equal to cutoff.
#'   If "smaller",
#'   treated whose running variable is less than or equal to cutoff.
#'   If missing, try to find `getOption("discRD.assign")`.
#' @param se character.
#'   How to calculate robust variance-covariance matrix
#'   ("HC0", "HC1", "HC2", "HCj", "HC3", and "HC4")
#' @param cholesky logical (default is TRUE).
#'   When solving normal equation, use cholesky decomposition.
#' @param bw bandwidth.
#' @param kernel character of kernel density ("uniform" or "triangular")
#' @param point value of the running variable that
#'   the kernel weights weigh the most
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
#' local <- local_lm(data = raw, bw = 3, kernel = "uniform")
#' str(local_lm(submod = 1, data = raw, bw = 3, kernel = "uniform"))
#' str(local_lm(submod = 1, order = 3, data = raw, bw = 3, kernel = "uniform"))
#' str(local_lm(data = raw, cutoff = 30, bw = 3, kernel = "uniform"))
#' est2 <- local_lm(
#'   data = raw, se = "HC1", weights = w, bw = 3, kernel = "uniform"
#' )
#'
local_lm <- function(basemod,
                     data,
                     subset,
                     weights,
                     cluster,
                     submod,
                     order = c(1, 2),
                     cutoff,
                     assign,
                     se = "HC0",
                     cholesky = TRUE,
                     bw,
                     kernel,
                     point = 0) {
  # check basemod if missing
  if (missing(basemod)) basemod <- getOption("discRD.basemod")
  if (!is.list(basemod)) basemod <- list(basemod)
  if (length(basemod) == 0) stop("Not find basemod")

  # model list
  if (missing(submod)) submod <- seq_len(length(basemod))
  usemod <- basemod[submod]

  mod <- expand.grid(basemod = usemod, order = order)
  mod <- mod[!duplicated(mod), ]
  if (nrow(mod) == 0) stop("Cannot construct model")

  output <- list()
  output$model.outline <- mod

  # collect arguments
  dtarg <- rlang::enquos(
    subset = subset,
    weights = weights,
    cluster = cluster
  )
  dtarg <- Filter(Negate(rlang::quo_is_missing), dtarg)

  if (!missing(cutoff)) dtarg$cutoff <- cutoff
  if (!missing(assign)) dtarg$assign <- assign
  dtarg$data <- data

  estarg <- list(
    se = se,
    cholesky = cholesky,
    global = FALSE,
    kernel = kernel,
    bw = bw,
    point = point
  )

  # estimation
  est <- lapply(seq_len(nrow(mod)), function(i) {
    # data cleaning
    dtarg$basemod <- mod[i, "basemod"][[1]]
    dtarg$order <- mod[i, "order"]
    useit <- do.call("clean_rd_data", dtarg)

    # treatment and control data
    d1 <- useit$data[useit$data$d == 1, ]
    d0 <- useit$data[useit$data$d == 0, ]

    # estimation
    y <- rlang::f_lhs(mod[i, "basemod"][[1]])
    treat <- do.call("fit_wls", append(estarg, list(data = d1, y = y)))
    ctrl <- do.call("fit_wls", append(estarg, list(data = d0, y = y)))

    # local ATE
    ate <- treat$estimate[1, 1] - ctrl$estimate[1, 1]
    ate_se <- sqrt(treat$vcov$matrix[1, 1] + ctrl$vcov$matrix[1, 1])
    ate_z <- abs(ate) / ate_se
    ate_p <- 2 * pnorm(ate_z, lower.tail = FALSE)
    ate_mat <- matrix(c(ate, ate_se, ate_z, ate_p), nrow = 1)
    colnames(ate_mat) <- c("Estimate", "Std.Err.", "z", "P(>|z|)")
    rownames(ate_mat) <- c("Local ATE")

    # output
    res <- list(
      outcome = rlang::as_label(y),
      RD.info = useit$RD.info,
      treat = treat,
      control = ctrl,
      local.ate = ate_mat
    )
    class(res) <- "local_lm"
    res
  })

  # output
  output$result <- est
  class(output) <- append("list_local_lm", class(output))
  output
}
