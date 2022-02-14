#' @export 
#' 
#' 
predict.local_ate_global_lm <- function(object, ...) {
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