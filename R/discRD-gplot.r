#' Flexible RD Plot
#' @export
#' 
gplot <- function(object, ...) {
  UseMethod("gplot")
}

#'
#' @param object object
#' @param order numeric. which polynomial order model you use
#' @param treat_label string vector of treatment and control label
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
#'   order = 1,
#'   treat_label = c("Treated", "Control"),
#'   ylim = list("y" = c(0, 110)),
#'   vjust = c("y" = 20),
#'   hjust = c("y" = -0.6),
#'   ylab = "Weighted Average",
#'   ate_label_size = 4,
#'   ate_label_name = "RD estimate"
#' )
#'
#' @name gplot
#'
gplot.global_lm <- function(object,
                            order,
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
  # flag using estimation results
  outline <- object$model.outline
  modlen <- seq_len(nrow(outline))
  useres <- modlen[outline$order == order & unlist(outline$covmod) == ""]

  # label of estimation results
  label <- sapply(useres, function(i) {
    as.character(rlang::f_lhs(outline[i, ]$basemod[[1]]))
  })

  # recover data
  dt <- lapply(useres, recover_data, object = object)
  names(dt) <- label

  # observed data aggregated by mass points
  vars <- c("outcome", "weights", "d")
  agg <- lapply(dt, function(d) {
    d$outcome <- d$outcome * d$weights
    agdt <- aggregate(d[, vars], by = list(x = d$x), mean)
    agdt$outcome <- agdt$outcome / agdt$weights
    agdt$x <- agdt$x + object$RD.info$cutoff
    agdt$d <- factor(agdt$d, levels = c(1, 0), labels = treat_label)
    agdt
  })

  # prediction data
  pred <- lapply(seq_len(length(useres)), function(i) {
    newdt <- dt[[i]]
    newdt <- newdt[, !(names(newdt) %in% vars)]
    newdt <- newdt[!duplicated(newdt), ]
    predict(object$result[[useres[i]]], newdata = newdt)
  })
  names(pred) <- label

  # treated prediction data
  pd1 <- if (object$RD.info$assign == "greater") {
    lapply(pred, function(d) d[x >= 0, ])
  } else {
    lapply(pred, function(d) d[x <= 0, ])
  }

  pd1 <- lapply(pd1, function(d) {
    names(d)[names(d) == "yhat1"] <- "yhat"
    d
  })

  # control prediction data
  pd0 <- if (object$RD.info$assign == "greater") {
    lapply(pred, function(d) d[x <= 0, ])
  } else {
    lapply(pred, function(d) d[x >= 0, ])
  }

  pd0 <- lapply(pd0, function(d) {
    names(d)[names(d) == "yhat0"] <- "yhat"
    d
  })

  numform <- paste0("%1.", ate_label_digits, "f")
  label1 <- paste0(ate_label_name, ": ", numform, "***(", numform, ")")
  label2 <- paste0(ate_label_name, ": ", numform, "**(", numform, ")")
  label3 <- paste0(ate_label_name, ": ", numform, "*(", numform, ")")
  label4 <- paste0(ate_label_name, ": ", numform, "(", numform, ")")

  ate_label <- lapply(useres, function(i) {
    ate <- object$result[[i]]$local.ate
    p <- ate[, 4]
    dplyr::case_when(
      p <= 0.01 ~ sprintf(label1, ate[, 1], ate[, 2]),
      p <= 0.05 ~ sprintf(label2, ate[, 1], ate[, 2]),
      p <= 0.10 ~ sprintf(label3, ate[, 1], ate[, 2]),
      TRUE      ~ sprintf(label4, ate[, 1], ate[, 2])
    )
  })
  names(ate_label) <- label

  # collect arguments
  args <- as.list(match.call())[-1]

  parg_name <- c(
    "ate_label_size",
    "outcome_label",
    "ylim",
    "vjust",
    "hjust",
    "xlab",
    "ylab"
  )

  parg <- args[names(args) %in% parg_name]
  parg <- lapply(parg, eval)
  if (is.null(parg$outcome_label)){
    parg$outcome_label <- setNames(label, label)
  }

  parg <- append(
    parg,
    list(
      aggregate = agg,
      predict1 = pd1,
      predict0 = pd0,
      cutoff = object$RD.info$cutoff,
      ate_label = ate_label
    )
  )

  tmparg <- list(...)

  # plot
  plist <- lapply(label, function(l) {
    parg <- lapply(parg, function(a) {
      if (any(names(a) != "")) {
        if (is.list(a)) a[names(a) == l][[l]] else a[names(a) == l]
      } else {
        a
      }
    })
    bool <- unlist(lapply(parg, function(l) length(l) != 0))
    parg <- parg[bool]
    do.call("gplot_internal_cutoff", append(parg, tmparg))
  })

  if (patchwork) {
    patchwork::wrap_plots(plist) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  } else {
    plist
  }
}
