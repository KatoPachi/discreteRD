#' Mean difference test (t-test) for local random approach
#'
#' @param basemod baseline formula. `outcome ~ running variable`.
#' @param data data.frame which you want to use.
#' @param bootse numeric.
#'   Generate N averages of bootstrap samples and
#'   use the standard deviation as the standard error of the average.
#'   If missing, standard error of mean is calculated by
#'   sqrt(v / e) where v is unbiased variance of outcome,
#'   and e is effective sample size: e = sum(w) ^ 2 / sum(w ^ 2).
#'   If w is missing, we set w = 1, and obtain e = 1 / n.
#'   standard error of mean difference is obtained by
#'   sqrt((v1 / e1) + (v0 / e0))
#' @param bootp numeric.
#'   Perform a permutation test with N re-randomizations.
#'   The p-value is obtained at a rate
#'   where the absolute value of the mean difference
#'   due to rerandomization is greater than
#'   the absolute value of the observed mean difference.
#'   If missing, standard t-test is performed.
#'
#' @importFrom rlang f_lhs
#' @importFrom rlang eval_tidy
#' @importFrom stats var
#' @importFrom stats pt
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
#' useit <- clean_rd_data(y ~ running, data = raw)
#' str(local_random_test_int(y ~ running, useit$data))
#' str(local_random_test_int(y ~ running, useit$data, bootse = 100))
#' str(local_random_test_int(y ~ running, useit$data, bootp = 100))
#' }
#'
local_random_test_int <- function(basemod,
                                  data,
                                  bootse,
                                  bootp) {
  # create weights if null
  if (is.null(data$"(weights)")) data$"(weights)" <- 1

  # treated and control data
  d1 <- data$d == 1
  n1 <- sum(d1)
  n0 <- sum(!d1)

  yvar <- rlang::f_lhs(basemod)
  y1 <- rlang::eval_tidy(yvar, data[d1, ])
  y0 <- rlang::eval_tidy(yvar, data[!d1, ])

  w1 <- data[d1, "(weights)"]
  w0 <- data[!d1, "(weights)"]

  # average and mean difference
  ymu1 <- mean(y1 * w1) / mean(w1)
  ymu0 <- mean(y0 * w0) / mean(w0)
  diff_mu <- ymu1 - ymu0

  # create output list
  out <- list()
  out$outcome <- as.character(yvar)
  out$observe$treat$N <- n1
  out$observe$treat$mean <- ymu1
  out$observe$control$N <- n0
  out$observe$control$mean <- ymu0
  out$local.ate$estimate <- diff_mu

  # standard errors
  if (missing(bootse)) {
    effective_size_1 <- sum(w1)^2 / sum(w1^2)
    effective_size_0 <- sum(w0)^2 / sum(w0^2)

    yse1 <- var(y1) / effective_size_1
    yse0 <- var(y0) / effective_size_0
    mean_se <- sqrt(yse1 + yse0)
  } else {
    yse1 <- bootstrap(y1, w1, bootse)$boot$se
    yse0 <- bootstrap(y0, w0, bootse)$boot$se
    mean_se <- sqrt(yse1 + yse0)
  }

  out$observe$treat$se <- yse1
  out$observe$control$se <- yse0
  out$local.ate$se <- mean_se

  # two-sided test or permutation test
  if (missing(bootp)) {
    dfval <- n1 + n0 - 2
    tval <- abs(diff_mu) / mean_se
    pval <- 2 * pt(tval, df = dfval, lower.tail = FALSE)

    out$local.ate$df <- dfval
    out$local.ate$t <- tval
    out$local.ate$p.value <- pval
    out$local.ate$method <- "Two-sided t-test"
  } else {
    y <- rlang::eval_tidy(yvar, data)
    d <- data$d
    w <- data$"(weights)"

    perm <- permutation(y, d, w, bootp)

    out$local.ate$p.value <- perm$ate$p.value
    out$local.ate$method <- perm$ate$method
  }

  out
}

#'
#' Perform Local Randomization Approach
#'
#' @description An analysis of the difference between
#'   the means of the two groups is performed
#'   under the assumption that
#'   the treatment assignments are random near the cutoff (local random).
#'   Statistical inference has a permutation test for small sample sizes
#'   in addition to the standard t-test.
#'
#' @param basemod baseline formula. `outcome ~ running variable`.
#' @param data data.frame which you want to use.
#' @param weights weight variable.
#' @param submod numeric vector.
#'   Which baseline model you want to use.
#' @param subset subset condition.
#' @param bw numeric vector of bandwidth.
#'   If specified, use data
#'   whose running variables are within this range will be used.
#'   If missing, use data from treatment and control groups
#'   where the running variable is closest to the cutoff
#' @param global logical (default is FALSE).
#'   Whether to use all observations.
#' @param bootse numeric.
#'   Generate N averages of bootstrap samples and
#'   use the standard deviation as the standard error of the average.
#'   If missing, standard error of mean is calculated by
#'   sqrt(v / e) where v is unbiased variance of outcome,
#'   and e is effective sample size: e = sum(w) ^ 2 / sum(w ^ 2).
#'   If w is missing, we set w = 1, and obtain e = 1 / n.
#'   standard error of mean difference is obtained by
#'   sqrt((v1 / e1) + (v0 / e0))
#' @param bootp numeric.
#'   Perform a permutation test with N re-randomization.
#'   The p-value is obtained at a rate
#'   where the absolute value of the mean difference
#'   due to re-randomization is greater than
#'   the absolute value of the observed mean difference.
#'   If missing, standard t-test is performed.
#' @param cutoff numeric of cutoff points
#'   If missing, try to find `getOption("discRD.cutoff")`.
#' @param assign assignment rule of treatment.
#'   If "greater",
#'   treated whose running variable is greater than or equal to cutoff.
#'   If "smaller",
#'   treated whose running variable is less than or equal to cutoff.
#'   If missing, try to find `getOption("discRD.assign")`.
#'
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
#'   discRD.cutoff = 50,
#'   discRD.assign = "smaller"
#' )
#'
#' est1 <- local_random_test(data = raw, weights = w, global = TRUE)
#' str(est1)
#' est2 <- local_random_test(data = raw, bootp = 1000, submod = 1)
#' str(est2)
#'
local_random_test <- function(basemod,
                              data,
                              weights,
                              submod,
                              subset,
                              bw,
                              global = FALSE,
                              bootse,
                              bootp,
                              cutoff,
                              assign) {
  # collect arguments
  arg <- as.list(match.call())[-1]
  arg$data <- data

  # check basemod
  if (missing(basemod)) basemod <- getOption("discRD.basemod")
  if (!is.list(basemod)) basemod <- list(basemod)
  if (length(basemod) == 0) stop("Baseline model not found")
  if (missing(submod)) submod <- seq_len(length(basemod))
  usemod <- basemod[submod]

  # output list
  output <- list()

  # check arguments
  dt_arg_list_name <- c("data", "weights", "subset", "cutoff", "assign")
  dtarg <- arg[names(arg) %in% dt_arg_list_name]

  clean <- lapply(usemod, function(m) {
    dtarg$basemod <- m
    clean <- do.call("clean_rd_data", dtarg)
  })

  output$RD.info <- clean[[1]]$RD.info

  bw_arg_list_name <- c("bw", "global")
  bwarg <- arg[names(arg) %in% bw_arg_list_name]

  # cleaning data
  dtlist <- lapply(clean, function(m) {
    bwarg$data <- m
    do.call("data_bwfilter", bwarg)
  })

  output$bandwidth <- dtlist[[1]]$bwinfo

  # run test
  test_arg_list_name <- c("bootse", "bootp")
  testarg <- arg[names(arg) %in% test_arg_list_name]
  reslist <- lapply(seq_len(length(usemod)), function(i) {
    testarg$basemod <- usemod[[i]]
    testarg$data <- dtlist[[i]]$data
    do.call("local_random_test_int", testarg)
  })

  # output
  output$estimate <- reslist
  class(output) <- append("local_random_test", class(output))
  output
}
