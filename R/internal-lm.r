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
                        se_type = "HC0",
                        cluster,
                        cholesky = TRUE) {
  # check matrix
  if (!is.matrix(y)) y <- matrix(y, ncol = 1)
  if (!is.matrix(x)) x <- as.matrix(x)
  x <- cbind("(Intercept)" = 1, x)
  if (nrow(y) != nrow(x)) stop("different number of rows x and y")
  if (missing(w)) w <- rep(1, nrow(x))
  omega <- diag(w)

  # output list
  output <- list()
  output$input$response <- y
  output$input$design <- x
  output$input$weights <- w

  # observation and degree of freedom
  n <- nrow(x)
  df <- nrow(x) - ncol(x)

  output$N <- n
  output$df <- df

  ##########################################################################
  # **solve normal equation**
  # Note: When using cholesky decomposition, there is another way to solve
  # 1. run `forwardsolve(t(cholesky), t(x) %*% w %*% y)`
  # 2. use its return value (ghat) and run `backsolve(cholesky, ghat)`
  ##########################################################################

  if (cholesky) {
    cholesky <- chol(t(x) %*% omega %*% x)
    xx <- solve(cholesky) %*% t(solve(cholesky))
  } else {
    xx <- solve(t(x) %*% omega %*% x)
  }

  p <- xx %*% t(x) %*% omega
  b <- p %*% y

  # predictions, residuals, and residual std.err.
  proj <- x %*% p
  yh <- proj %*% y
  eh <- y - yh

  yh <- c(yh)
  eh <- c(eh)

  s2 <- sum(eh ^ 2) / df

  output$yhat <- yh
  output$ehat <- eh
  output$resid.std.err <- s2

  # variance-covariance matrix
  if (missing(cluster)) {
    ## quotation
    standard <- rlang::quo(s2 * xx)
    hce <- rlang::quo(p %*% sigma %*% t(p))

    ## component of hce
    ## reference: https://economics.mit.edu/files/7422
    h <- diag(proj)
    util <- eh / (1 - h)
    mutil <- matrix(util, ncol = 1)
    sigma <- switch(se_type,
      "HC0" = diag(eh^2),
      "HC1" = diag(eh^2) * (n / df),
      "HC2" = diag(eh^2 / (1 - h)),
      "HCj" = (diag(util^2) - mutil %*% t(mutil) / n) * (n - 1) / n,
      "HC3" = diag(util^2),
      "HC4" = diag(eh^2 / (1 - h)^min(4, n * h / (n - df)))
    )

    vcov <- if (se_type == "standard") {
      rlang::eval_tidy(standard)
    } else {
      rlang::eval_tidy(hce, list(sigma = sigma))
    }

    output$vcov$matrix <- vcov
    output$vcov$type <- se_type
  } else {
    ## cluster-robust estimate of the variance matrix
    ## see http://cameron.econ.ucdavis.edu/research/Cameron_Miller_JHR_2015_February.pdf
    g <- unique(cluster)
    gvcov <- lapply(g, function(i) {
      bool <- cluster == i
      gx <- x[bool, ]
      gomega <- diag(w[bool])
      geh <- matrix(eh[bool], ncol = 1)
      t(gx) %*% gomega %*% geh %*% t(geh) %*% gomega %*% gx
    })
    gvcov <- Reduce("+", gvcov)
    vcov <- xx %*% gvcov %*% xx

    output$vcov$matrix <- vcov
    output$vcov$type <- "cluster-robust"
    output$vcov$cluster <- cluster

    df <- length(g) - 1

    output$df <- df
  }

  # create coefficient table
  bmat <- cbind(b, sqrt(diag(vcov)))
  t <- sapply(seq_len(nrow(bmat)), function(i) bmat[i, 1] / bmat[i, 2])
  bmat <- cbind(bmat, t)
  p <- apply(bmat, 2, function(t) 2 * pt(-abs(t), df))[, 3]
  bmat <- cbind(bmat, p)
  colnames(bmat) <- c("Estimate", "Std.Err.", "t", "P(>|t|)")

  output$estimate <- bmat
  output
}
