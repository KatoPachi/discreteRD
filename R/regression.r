#' Least Squares
#'
#' @param y outcome vector.
#' @param x design matrix
#' @param w weight matrix
#' @param cholesky logical (default is TRUE).
#'   When solving normal equation, use cholesky decomposition.
#' @param hc character.
#'   Calculate robust variance-covariance matrix ("HC0" or "HC1")
#'
#' @examples
#' \dontrun{
#' x <- cbind(x1 = rnorm(100), x2 = rnorm(100))
#' y <- x[, 1] + 2 * x[, 2] + rnorm(100)
#' w <- sample(c(1, 0.5), 100, TRUE)
#' est <- lm_internal(y, x)
#' est$estimate
#' }
#'
#'
lm_internal <- function(y,
                        x,
                        w,
                        cholesky = TRUE,
                        hc = "HC0") {
  # check matrix
  if (!is.matrix(y)) y <- matrix(y, ncol = 1)
  if (!is.matrix(x)) x <- as.matrix(x)
  x <- cbind("(Intercept)" = 1, x)
  if (nrow(y) != nrow(x)) stop("different number of rows x and y")
  w <- if (missing(w)) diag(1, nrow(x)) else diag(w)

  # solve normal equation
  if (cholesky) {
    cholesky <- chol(t(x) %*% w %*% x)
    p <- solve(cholesky) %*% t(solve(cholesky)) %*% t(x) %*% w
    # ghat <- forwardsolve(t(cholesky), t(x) %*% w %*% y)
    # coef <- backsolve(cholesky, ghat)
  } else {
    p <- solve(t(x) %*% w %*% x) %*% t(x) %*% w
  }
  b <- p %*% y

  # predictions and residuals
  yhat <- x %*% b
  ehat <- y - yhat

  # variance-covariance matrix
  df <- switch(
    hc,
    "HC0" = 1,
    "HC1" = nrow(x) / (nrow(x) - nrow(x))
  )

  vcov <- p %*% (diag(c(ehat^2)) * df) %*% t(p)
  se <- sqrt(diag(vcov))

  # create coefficient table
  btab <- cbind(b, se)
  t <- sapply(seq_len(nrow(btab)), function(i) btab[i, 1] / btab[i, 2])
  btab <- cbind(btab, t)
  dft <- nrow(x) - ncol(x)
  p <- apply(btab, 2, function(t) 2 * pt(-abs(t), dft))[, 3]
  btab <- cbind(btab, p)
  colnames(btab) <- c("Estimate", "Std.Err.", "t", "P(>|t|)")

  # output
  output <- list(
    input = list(
      response = y,
      design = x
    ),
    estimate = btab,
    N = nrow(x),
    yhat = yhat,
    ehat = ehat,
    vcov = vcov,
    se.type = hc
  )

  if (!missing(w)) output$input$weights <- w
  output
}
