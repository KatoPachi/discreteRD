#' Flexible RD Plot
#' @export
#' 
rdplot <- function(object, ...) {
  UseMethod("rdplot")
}

#'
#' @param object object
#' @param usemod numeric vector. which models do you want to plot?
#' @param treat_label string vector of treatment and control label
#' @param ate_label_size In-plot text size
#' @param ate_label_digits decimal places of in-plot ATE result.
#' @param ate_label_format string of label format of in-plot ATE result.
#'   The format produces labels according to a unique grammar.
#'   If you want to embed a numerical value related to the estimation result,
#'   you need to enclose it in {}.
#'   \itemize{
#'     \item `{estimate}`: embed coefficient value (Local ATE)
#'     \item `{std.error}`: embed standard error of coefficient
#'     \item `{statistic}`: embed z-score of coefficient
#'     \item `{p.value}`: embed p-value of coefficient
#'     \item `{star}`: show *** if p-value <= 0.01;
#'       ** if p-value <= 0.05; * if p-value <= 0.1
#'   }
#' @param outcome_label Outcome label in plot title
#' @param ylim numeric vector of limits of y-axis
#' @param vjust numeric. Adjust in-plot text vertically
#' @param hjust numeric. Adjust in-plot text horizontally
#' @param xlab label of x-axis
#' @param ylab label of y-axis
#' @param patchwork logical.
#'   add a list of plots into one composition by `patchwork::wrap_plots`
#' @param ncol the dimensions of the grid to create.
#'   argument of `patchwork::wrap_plots`
#' @param nrow the dimensions of the grid to create.
#'   argument of `patchwork::wrap_plots`
#' @param \dots arguments of [simplegg()]
#'
#' @importFrom stats aggregate
#' @importFrom stats predict
#' @importFrom patchwork wrap_plots
#' @importFrom patchwork plot_layout
#' @importFrom ggplot2 theme
#' @importFrom rlang quo
#' @importFrom rlang enquos
#' @importFrom rlang quo_is_missing
#' @importFrom rlang eval_tidy
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
#' est <- global_lm(data = raw, weights = w)
#'
#' rdplot(
#'   est,
#'   usemod = c(1, 2),
#'   treat_label = c("Treated", "Control"),
#'   outcome_label = switch(x,
#'     "1" = "Simulated outcome",
#'     "2" = "Simulated outcome > 0"
#'   ),
#'   ylab = "Weighted Average",
#'   ate_label_size = 4
#' )
#'
#' @name rdplot
#'
rdplot.list_global_lm <- function(object,
                                  usemod,
                                  treat_label = c("treated", "control"),
                                  ate_label_size = 5,
                                  ate_label_digits = 3,
                                  ate_label_format,
                                  outcome_label,
                                  ylim,
                                  vjust = 0,
                                  hjust = 0,
                                  xlab = "Running variable",
                                  ylab = "Average",
                                  patchwork = TRUE,
                                  ncol = NULL,
                                  nrow = NULL,
                                  ...) {
  # observed data aggregated by mass points
  x <- i <- NULL
  vars <- c("outcome", "weights", "d")
  aggregate_quo <- rlang::quo({
    data <- recover_data(object, i)
    data$outcome <- data$outcome * data$weights
    agdt <- aggregate(data[, vars], by = list(x = data$x), mean)
    agdt$outcome <- agdt$outcome / agdt$weights
    agdt$x <- agdt$x + object$result[[i]]$RD.info$cutoff
    agdt$d <- factor(agdt$d, levels = c(1, 0), labels = treat_label)
    agdt
  })

  # prediction data
  prediction_quo <- rlang::quo({
    data <- recover_data(object, i)
    newdt <- data[, !(names(data) %in% vars)]
    newdt <- newdt[!duplicated(newdt), ]
    predict(object$result[[i]], newdata = newdt)
  })

  # subset condition for prediction data
  condmake <- rlang::quo({
    switch(object$result[[i]]$RD.info$assign,
      "greater" = rlang::quo(x >= 0),
      "smaller" = rlang::quo(x <= 0)
    )
  })

  # In-plot label about result of local ATE
  if (missing(ate_label_format)) {
    label <- "Local ATE: {estimate}{star}({std.error})"
  } else {
    label <- ate_label_format
  }

  label_quo <- rlang::quo({
    res <- object$result[[i]]
    label_maker(res, label, ate_label_digits)
  })

  # Other arguments for plot manipulation
  pargs <- rlang::enquos(
    ate_label_size = ate_label_size,
    outcome_label = outcome_label,
    ylim = ylim,
    vjust = vjust,
    hjust = hjust,
    xlab = xlab,
    ylab = ylab
  )

  pargs <- Filter(Negate(rlang::quo_is_missing), pargs)

  # Arguments for template
  tmparg <- list(...)

  # draw plots
  if (missing(usemod)) usemod <- seq_len(length(object$result))

  plist <- lapply(usemod, function(m) {
    # eval arguments
    args <- list()
    args$aggregate <- rlang::eval_tidy(aggregate_quo, list(i = m))
    pred <- rlang::eval_tidy(prediction_quo, list(i = m))
    cond <- rlang::eval_tidy(condmake, list(i = m))
    bool <- rlang::eval_tidy(cond, pred)
    args$predict1 <- pred[bool, ]
    args$predict0 <- pred[!bool, ]
    args$ate_label <- rlang::eval_tidy(label_quo, list(i = m))
    args$cutoff <- object$result[[m]]$RD.info$cutoff

    # eval pargs
    eval_pargs <- lapply(pargs, rlang::eval_tidy, list(x = as.character(m)))
    args <- append(args, eval_pargs)

    # draw plot
    do.call("rdplot_internal_cutoff", append(args, tmparg))
  })

  if (patchwork) {
    patchwork::wrap_plots(plist, ncol = ncol, nrow = nrow) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  } else {
    plist
  }
}

#'
#' @name rdplot
#' @param force logical. Whether to ignore error about estimation
#'
#' @importFrom stats aggregate
#' @importFrom patchwork wrap_plots
#' @importFrom patchwork plot_layout
#' @importFrom ggplot2 theme
#' @importFrom rlang quo
#' @importFrom rlang enquos
#' @importFrom rlang quo_is_missing
#' @importFrom rlang eval_tidy
#' @export
#' @examples
#' nonpara <- local_lm(
#'   submod = 1,
#'   data = raw,
#'   kernel = "uniform",
#'   bw = 5,
#'   order = 1:3
#' )
#'
#' rdplot(
#'   nonpara,
#'   usemod = 2,
#'   outcome_label = switch(x,
#'     "1" = "Order = 1",
#'     "2" = "Order = 2",
#'     "3" = "Order = 3"
#'   )
#' )
#'
rdplot.list_local_lm <- function(object,
                                 usemod,
                                 treat_label = c("treated", "control"),
                                 ate_label_size = 5,
                                 ate_label_digits = 3,
                                 ate_label_format,
                                 outcome_label,
                                 ylim,
                                 vjust = 0,
                                 hjust = 0,
                                 xlab = "Running variable",
                                 ylab = "Average",
                                 patchwork = TRUE,
                                 ncol = NULL,
                                 nrow = NULL,
                                 force = TRUE,
                                 ...) {
  # observed data aggregated by mass points
  i <- NULL
  vars <- c("outcome", "sweight", "d")
  aggregate_quo <- rlang::quo({
    data <- recover_data(object, i)
    data$outcome <- data$outcome * data$sweight
    agdt <- aggregate(data[, vars], by = list(x = data$x), mean)
    agdt$outcome <- agdt$outcome / agdt$sweight
    agdt$x <- agdt$x + object$result[[i]]$RD.info$cutoff
    agdt$d <- factor(agdt$d, levels = c(1, 0), labels = treat_label)
    agdt
  })

  # prediction data
  predict1_quo <- rlang::quo({
    fit <- fit_local_lm(
      object$result[[i]]$treat, extend = 0, force = force
    )
    colnames(fit)[colnames(fit) == "yhat"] <- "yhat1"
    fit
  })

  predict0_quo <- rlang::quo({
    fit <- fit_local_lm(
      object$result[[i]]$control, extend = 0, force = force
    )
    colnames(fit)[colnames(fit) == "yhat"] <- "yhat0"
    fit
  })

  # In-plot label about result of local ATE
  if (missing(ate_label_format)) {
    label <- "Local ATE: {estimate}{star}({std.error})"
  } else {
    label <- ate_label_format
  }

  label_quo <- rlang::quo({
    res <- object$result[[i]]
    label_maker(res, label, ate_label_digits)
  })

  # Other arguments for plot manipulation
  pargs <- rlang::enquos(
    ate_label_size = ate_label_size,
    outcome_label = outcome_label,
    ylim = ylim,
    vjust = vjust,
    hjust = hjust,
    xlab = xlab,
    ylab = ylab
  )

  pargs <- Filter(Negate(rlang::quo_is_missing), pargs)

  # Arguments for template
  tmparg <- list(...)

  # draw plots
  if (missing(usemod)) usemod <- seq_len(length(object$result))

  plist <- lapply(usemod, function(m) {
    # eval arguments
    args <- list()
    args$aggregate <- rlang::eval_tidy(aggregate_quo, list(i = m))
    args$predict1 <- rlang::eval_tidy(predict1_quo, list(i = m))
    args$predict0 <- rlang::eval_tidy(predict0_quo, list(i = m))
    args$ate_label <- rlang::eval_tidy(label_quo, list(i = m))
    args$cutoff <- object$result[[m]]$RD.info$cutoff

    # eval pargs
    eval_pargs <- lapply(pargs, rlang::eval_tidy, list(x = as.character(m)))
    args <- append(args, eval_pargs)

    # draw plot
    do.call("rdplot_internal_cutoff", append(args, tmparg))
  })

  if (patchwork) {
    patchwork::wrap_plots(plist, ncol = ncol, nrow = nrow) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  } else {
    plist
  }
}
