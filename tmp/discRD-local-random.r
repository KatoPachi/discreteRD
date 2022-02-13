#' Mean difference test (t-test) for local random approach
#'
#' @param y string of outcome variable
#' @param x string of running variable
#' @param d string of treatment variable
#' @param w string of weight variable
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
#'   Perform a permutation test with N re-randomizations.
#'   The p-value is obtained at a rate
#'   where the absolute value of the mean difference
#'   due to rerandomization is greater than
#'   the absolute value of the observed mean difference.
#'   If missing, standard t-test is performed.
#' @param data data.frame which you want to use.
#'
#' @importFrom stats var
#' @importFrom stats sd
#' @importFrom stats pt
#' @examples
#' \dontrun{
#' loc_rand_ttest("y", data = dt2)
#' loc_rand_ttest("y", global = TRUE, data = dt2)
#' loc_rand_ttest("y", bootse = 100, data = dt2)
#' loc_rand_ttest("y", bootp = 100, data = dt2)
#' loc_rand_ttest("y", bw = c(-5, 5), bootp = 100, data = dt2)
#' }
#'
loc_rand_ttest <- function(
  y, x = "x", d = "d", w,
  bw, global = FALSE,
  bootse, bootp,
  data
) {
  # subsample data by bandwidth
  if (!global) {
    if (missing(bw)) {
      greater_assign_flag <- all(data[data[[d]] == 1, x] >= 0)
      if (greater_assign_flag) {
        bw <- c(max(data[data[[d]] == 0, x]), 0)
      } else {
        bw <- c(0, min(data[data[[d]] == 0, x]))
      }
    }
    data <- data[bw[1] <= data[[x]] & data[[x]] <= bw[2], ]
  }

  # weight
  data$w <- ifelse(missing(w), 1, data[[w]])

  # treated and control data
  treat <- data[data[[d]] == 1, ]
  ctrl <- data[data[[d]] == 0, ]

  # observations
  n1 <- nrow(treat); n0 <- nrow(ctrl)

  # average and mean difference
  ymean1 <- sum(treat[[y]] * treat$w) / sum(treat$w)
  ymean0 <- sum(ctrl[[y]] * ctrl$w) / sum(ctrl$w)
  diff_mean <- ymean1 - ymean0

  # standard errors
  if (missing(bootse)) {
    effective_size_1 <- sum(treat$w)^2 / sum(treat$w^2)
    effective_size_0 <- sum(ctrl$w)^2 / sum(ctrl$w^2)

    yse1 <- var(treat[[y]]) / effective_size_1
    yse0 <- var(ctrl[[y]]) / effective_size_0

    mean_se <- sqrt(yse1 + yse0)
  } else {
    boot_yse1 <- numeric(bootse)
    boot_yse0 <- numeric(bootse)

    set.seed(120511)
    for (i in seq_len(bootse)) {
      pick <- sample(seq_len(nrow(data)), size = nrow(data), replace = TRUE)
      bootdt <- data[pick, ]
      boottreat <- bootdt[bootdt[[d]] == 1, ]
      bootctrl <- bootdt[bootdt[[d]] == 0, ]
      boot_yse1[i] <- sum(boottreat[[y]] * boottreat$w) / sum(boottreat$w)
      boot_yse0[i] <- sum(bootctrl[[y]] * bootctrl$w) / sum(bootctrl$w)
    }

    yse1 <- sd(boot_yse1)
    yse0 <- sd(boot_yse0)

    mean_se <- sqrt(yse1 + yse0)
  }

  # two-sided test or permutation test
  if (missing(bootp)) {
    dfval <- n1 + n0 - 2
    tval <- abs(diff_mean) / mean_se
    pval <- 2 * pt(tval, df = dfval, lower.tail = FALSE)

    # output
    data.frame(
      outcome = y,
      mean_y1 = ymean1, se_y1 = sqrt(yse1), n1 = n1,
      mean_y0 = ymean0, se_y0 = sqrt(yse0), n0 = n0,
      mean_diff = diff_mean, mean_diff_se = mean_se,
      t = tval, df = dfval, p = pval,
      test = "t-test"
    )
  } else {
    boot_diff <- numeric(bootp)

    set.seed(120511)
    for (i in seq_len(bootp)) {
      treated <- sample(seq_len(nrow(data)), size = n1)
      treated_v <- rep(FALSE, nrow(data))
      treated_v[treated] <- TRUE
      perm_treat <- data[treated_v, ]
      perm_ctrl <- data[!treated_v, ]
      treat_mean <- sum(perm_treat[[y]] * perm_treat$w) / sum(perm_treat$w)
      ctrl_mean <- sum(perm_ctrl[[y]] * perm_ctrl$w) / sum(perm_ctrl$w)
      boot_diff[i] <- treat_mean - ctrl_mean
    }

    pval <- mean(abs(boot_diff) >= abs(diff_mean))

    # output
    data.frame(
      outcome = y,
      mean_y1 = ymean1, se_y1 = sqrt(yse1), n1 = n1,
      mean_y0 = ymean0, se_y0 = sqrt(yse0), n0 = n0,
      mean_diff = diff_mean, p = pval,
      test = "permutation"
    )
  }
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
#' @param y string vector with outcome variables
#'   If missing, try to find `getOption("discRD.outcome")`.
#' @param x string of running variable
#'   If missing, try to find `getOption("discRD.running")`.
#' @param cutoff numeric of cutoff points
#'   If missing, try to find `getOption("discRD.cutoff")`.
#' @param assign assignment rule of treatment.
#'   If "greater",
#'   treated whose running variable is greater than or equal to cutoff.
#'   If "smaller",
#'   treated whose running variable is less than or equal to cutoff.
#'   If missing, try to find `getOption("discRD.assign")`.
#' @param w string of weight variable
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
#'   Perform a permutation test with N re-randomizations.
#'   The p-value is obtained at a rate
#'   where the absolute value of the mean difference
#'   due to rerandomization is greater than
#'   the absolute value of the observed mean difference.
#'   If missing, standard t-test is performed.
#' @param data data.frame which you want to use.
#' @param suby numeric vector.
#' Specify position of outcome variables you want to use.
#'
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' running <- sample(1:100, size = 1000, replace = TRUE)
#' cov1 <- rnorm(1000, sd = 2); cov2 <- rnorm(1000, mean = -1)
#' y0 <- running + cov1 + cov2 + rnorm(1000)
#' y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000)
#' y <- ifelse(running <= 50, y1, y0)
#' bin <- ifelse(y > mean(y), 1, 0)
#' raw <- data.frame(y, bin, running, cov1, cov2)
#'
#' set_optDiscRD(
#'   y + bin ~ running,
#'   discRD.cutoff = 50,
#'   discRD.assign = "smaller"
#' )
#'
#' local_random(data = raw)
#' local_random(data = raw, bootp = 100, bootse = 100, suby = 1)
#' local_random("y", data = raw, bootp = 100)
#'
local_random <- function(
  y, x, cutoff, assign, w,
  bw, global = FALSE,
  bootse, bootp,
  data, suby
) {
  # collect arguments
  args <- as.list(match.call())[-1]

  # check global options for outcomes
  if (missing(y)) {
    y <- getOption("discRD.outcome")
    if (all(y == "")) stop("Specify outcome in argument or global option.")
  }
  if (missing(suby)) suby <- seq_len(length(y))
  y <- y[suby]

  # shaping data
  if (missing(w)) {
    usedt <- shape_data(y, x, cutoff, assign, data)
    args$data <- usedt$data
  } else {
    usedt <- shape_data(y, x, cutoff, assign, data, w = w)
    args$data <- usedt$data
  }

  # run t-test/permutation test
  test <- lapply(y, function(item) {
    args$y <- item
    args$suby <- NULL
    do.call("loc_rand_ttest", args)
  })

  # output
  out <- list(
    res = dplyr::bind_rows(test),
    RD.info = usedt$RD.info
  )
  class(out) <- append("local_random", class(out))
  out
}
