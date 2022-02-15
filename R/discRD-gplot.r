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
#' @param ate_label_name label of in-plot ATE result.
#' @param outcome_label Outcome label in plot title
#' @param ylim numeric vector of limits of y-axis
#' @param vjust numeric. Adjust in-plot text vertically
#' @param hjust numeric. Adjust in-plot text horizontally
#' @param xlab label of x-axis
#' @param ylab label of y-axis
#' @param \dots arguments of [simplegg()]
#'
#' @importFrom stats setNames
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
#' gplot(
#'   est,
#'   usemod = c(1, 2),
#'   treat_label = c("Treated", "Control"),
#'   outcome_label = switch(x,
#'     "1" = "Simulated outcome",
#'     "2" = "Simulated outcome > 0"
#'   ),
#'   ylab = "Weighted Average",
#'   ate_label_size = 4,
#'   ate_label_name = "RD estimate"
#' )
#'
#' @name rdplot
#'
rdplot.list_global_lm <- function(object,
                                  usemod,
                                  treat_label = c("treated", "control"),
                                  ate_label_size = 5,
                                  ate_label_digits = 3,
                                  ate_label_name = "Local ATE",
                                  outcome_label,
                                  ylim,
                                  vjust = 0,
                                  hjust = 0,
                                  xlab = "Running variable",
                                  ylab = "Average",
                                  patchwork = TRUE,
                                  ...) {
  # observed data aggregated by mass points
  i <- NULL
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
  numform <- paste0("%1.", ate_label_digits, "f")
  label1 <- paste0(ate_label_name, ": ", numform, "***(", numform, ")")
  label2 <- paste0(ate_label_name, ": ", numform, "**(", numform, ")")
  label3 <- paste0(ate_label_name, ": ", numform, "*(", numform, ")")
  label4 <- paste0(ate_label_name, ": ", numform, "(", numform, ")")

  label_quo <- rlang::quo({
    ate <- object$result[[i]]$local.ate
    dplyr::case_when(
      ate[, 4] <= 0.01 ~ sprintf(label1, ate[, 1], ate[, 2]),
      ate[, 4] <= 0.05 ~ sprintf(label2, ate[, 1], ate[, 2]),
      ate[, 4] <= 0.10 ~ sprintf(label3, ate[, 1], ate[, 2]),
      TRUE ~ sprintf(label4, ate[, 1], ate[, 2])
    )
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
    do.call("gplot_internal_cutoff", append(args, tmparg))
  })

  if (patchwork) {
    patchwork::wrap_plots(plist) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  } else {
    plist
  }
}

#'
#' @name rdplot
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
                                 ate_label_name = "Local ATE",
                                 outcome_label,
                                 ylim,
                                 vjust = 0,
                                 hjust = 0,
                                 xlab = "Running variable",
                                 ylab = "Average",
                                 patchwork = TRUE,
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
  exclude <- c("weights", "kweight", "(Intercept)")
  args <- list()
  args$y <- rlang::quo(outcome)
  args$global <- FALSE

  predict1_quo <- rlang::quo({
    data <- recover_data(object, i)
    data <- data[, !(names(data) %in% exclude)]
    colnames(data)[colnames(data) == "sweight"] <- "(weights)"
    data <- data[data$d == 1, ]

    args$data <- data
    args$se <- object$result[[i]]$treat$vcov$type
    args$kernel <- object$result[[i]]$treat$input$local.wls$kernel
    args$bw <- object$result[[i]]$treat$input$local.wls$bandwidth

    point <- unique(c(0, data$x))
    pred <- lapply(point, function(x) {
      args$point <- x
      tryCatch({
          est <- structure(do.call("fit_wls", args), class = "local_lm")
          predict(est)
      }, error = function(e) {})
    })
    pred <- dplyr::bind_rows(pred)
    colnames(pred)[colnames(pred) == "yhat"] <- "yhat1"
    pred
  })

  predict0_quo <- rlang::quo({
    data <- recover_data(object, i)
    data <- data[, !(names(data) %in% exclude)]
    colnames(data)[colnames(data) == "sweight"] <- "(weights)"
    data <- data[data$d == 0, ]

    args$data <- data
    args$se <- object$result[[i]]$control$vcov$type
    args$kernel <- object$result[[i]]$control$input$local.wls$kernel
    args$bw <- object$result[[i]]$control$input$local.wls$bandwidth

    point <- unique(c(0, data$x))
    pred <- lapply(point, function(x) {
      args$point <- x
      tryCatch({
          est <- structure(do.call("fit_wls", args), class = "local_lm")
          predict(est)
      }, error = function(e) {})
    })
    pred <- dplyr::bind_rows(pred)
    colnames(pred)[colnames(pred) == "yhat"] <- "yhat0"
    pred
  })

  # In-plot label about result of local ATE
  numform <- paste0("%1.", ate_label_digits, "f")
  label1 <- paste0(ate_label_name, ": ", numform, "***(", numform, ")")
  label2 <- paste0(ate_label_name, ": ", numform, "**(", numform, ")")
  label3 <- paste0(ate_label_name, ": ", numform, "*(", numform, ")")
  label4 <- paste0(ate_label_name, ": ", numform, "(", numform, ")")

  label_quo <- rlang::quo({
    ate <- object$result[[i]]$local.ate
    dplyr::case_when(
      ate[, 4] <= 0.01 ~ sprintf(label1, ate[, 1], ate[, 2]),
      ate[, 4] <= 0.05 ~ sprintf(label2, ate[, 1], ate[, 2]),
      ate[, 4] <= 0.10 ~ sprintf(label3, ate[, 1], ate[, 2]),
      TRUE ~ sprintf(label4, ate[, 1], ate[, 2])
    )
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
    do.call("gplot_internal_cutoff", append(args, tmparg))
  })

  if (patchwork) {
    patchwork::wrap_plots(plist) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  } else {
    plist
  }
}
