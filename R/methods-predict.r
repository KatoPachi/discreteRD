#' @export
#'
#'
predict.global_lm <- function(object, ...) {
  args <- list(...)
  if (is.null(args$newdata)) {
    d0 <- cbind(
      object$control$yhat,
      object$control$input$design,
      d = 0
    )

    d1 <- cbind(
      object$treat$yhat,
      object$treat$input$design,
      d = 1
    )

    xlab <- colnames(object$control$input$design)
    d <- data.frame(rbind(d0, d1))
    colnames(d) <- c("outcome", xlab, "d")
    d
  } else {
    newdt <- args$newdata
    design <- as.matrix(newdt)

    b0 <- object$control$estimate[, 1]
    b0 <- matrix(b0[colnames(design)], ncol = 1)
    yhat0 <- design %*% b0

    b1 <- object$treat$estimate[, 1]
    b1 <- matrix(b1[colnames(design)], ncol = 1)
    yhat1 <- design %*% b1
    out <- cbind(yhat1, yhat0, design)
    out <- data.frame(out)
    colnames(out) <- c("yhat1", "yhat0", colnames(design))
    out
  }
}

#'
#' @export
#'
predict.fit_wls <- function(object, ...) {
  args <- list(...)

  # new design matrix
  if (is.null(args$newdata)) {
    point <- object$input$local.wls$data.point
    xmat <- matrix(c(1, point), nrow = 1)
    colnames(xmat) <- c("(Intercept)", "x")

    order <- ncol(object$input$design) - 1
    if (order > 1) {
      for (o in seq(2, order)) {
        xmat <- cbind(xmat, point^o)
        colnames(xmat)[o + 1] <- paste0("x", o)
      }
    }
  } else {
    newdt <- args$newdata
    xmat <- as.matrix(newdt)
  }

  # prediction
  b <- object$estimate[, 1]
  b <- matrix(b[colnames(xmat)], ncol = 1)
  yhat <- xmat %*% b

  # output
  out <- cbind(yhat, xmat)
  out <- data.frame(out)
  colnames(out) <- c("yhat", colnames(xmat))
  out
}

#'
#' @export
#'
predict.local_lm <- function (object, ...) {
  args <- list(...)
  yhat1 <- do.call("predict", append(args, list(object = object$treat)))
  colnames(yhat1)[colnames(yhat1) == "yhat"] <- "yhat1"

  yhat0 <- do.call("predict", append(args, list(object = object$control)))
  colnames(yhat0)[colnames(yhat0) == "yhat"] <- "yhat0"

  out <- cbind(yhat1 = yhat1[, "yhat1"], yhat0)
  out
}
