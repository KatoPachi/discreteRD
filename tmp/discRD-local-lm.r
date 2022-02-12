running <- sample(1:100, size = 1000, replace = TRUE)
cov1 <- rnorm(1000, sd = 2)
cov2 <- rnorm(1000, mean = -1)
y0 <- running + cov1 + cov2 + rnorm(1000, sd = 10)
y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000, sd = 10)
y <- ifelse(running <= 50, y1, y0)
bin <- ifelse(y > mean(y), 1, 0)
w <- sample(c(1, 0.5), size = 1000, replace = TRUE)
raw <- data.frame(y, bin, running, cov1, cov2, w)

clear_optDiscRD()

set_optDiscRD(
  y + bin ~ running,
  xmod = ~ cov1 + cov2,
  discRD.cutoff = 50,
  discRD.assign = "smaller"
)

bw <- 5
point <- 0
kernel <- "triangular"
hc <- 0

useit <- shape_data(data = raw)

u <- abs((useit$data$x - point) / bw)
useit$data$k <- switch(kernel,
  "triangular" = ifelse(u <= 1, (1 - u), 0),
  "uniform" = ifelse(u <= 1, 1 / 2, 0)
)

d1 <- subset(useit$data, d == 1)
d0 <- subset(useit$data, d == 0)

## inverse method
inv_method <- lapply(list(d1, d0), function(d) {
  x <- cbind(1, x = d$x)
  w <- diag(d$k)
  y <- matrix(d$y, ncol = 1)

  p <- solve(t(x) %*% w %*% x) %*% t(x) %*% w
  coef <- p %*% y
  response <- x %*% coef
  resid <- y - response

  df <- if (hc == 0) 1 else if (hc == 1) nrow(x) / (nrow(x) - nrow(coef))
  vcov <- p %*% (diag(c(resid^2)) * df)  %*% t(p)

  list(b = coef, yhat = c(response), ehat = c(resid), vcov = vcov)
})

## cholesky decomposition approach
chol_method <- lapply(list(d1, d0), function(d) {
  x <- cbind(1, x = d$x)
  w <- diag(d$k)
  y <- matrix(d$y, ncol = 1)

  cholesky <- chol(t(x) %*% w %*% x)
  p <- solve(cholesky) %*% t(solve(cholesky)) %*% t(x) %*% w
  # ghat <- forwardsolve(t(cholesky), t(x) %*% w %*% y)
  # coef <- backsolve(cholesky, ghat)
  coef <- p %*% y
  response <- x %*% coef
  resid <- y - response

  df <- if (hc == 0) 1 else if (hc == 1) nrow(x) / (nrow(x) - nrow(coef))
  vcov <- p %*% (diag(c(resid^2)) * df) %*% t(p)

  list(b = coef, yhat = c(response), ehat = c(resid), vcov = vcov)
})

## difference in intercepts (local ATE)
target <- chol_method[[1]]$b[1, ] - chol_method[[2]]$b[1, ]
target_se <- sqrt(chol_method[[1]]$vcov[1, 1] + chol_method[[2]]$vcov[1, 1])
zscore <- abs(target) / target_se
p <- 2 * pnorm(zscore, lower.tail = FALSE)

cat(paste0(
  "Local ATE by Local Linear Regression \n\n",

  "Treatment information \n",
  "Cutoff value = ", getOption("discRD.cutoff"), "\n",
  "Assignment rule = ", getOption("discRD.assign"), "\n\n",

  "Regression Information \n",
  "Observation of control = ", sprintf("%1d ", nrow(d0)),
  "(Obs with positive kernel weight = ",
  sprintf("%1d)\n", nrow(d0[d0$k > 0, ])),
  "Observation of treatment = ", sprintf("%1d ", nrow(d1)),
  "(Obs with positive kernel weight = ",
  sprintf("%1d)\n", nrow(d1[d1$k > 0, ])),
  "Kernel density: ", kernel, "\n",
  "Bandwidth: ", bw, "\n",
  "Robust variance-covariance matrix: HC", hc, "\n\n",

  "--------------------------------------------------------\n",
  "              Estimate   Std.Err.   z-value    Pr(>|z|) \n",
  "--------------------------------------------------------\n",
  "Local ATE     ",
  sprintf("%1.3f", target), "     ",
  sprintf("%1.3f", target_se), "      ",
  sprintf("%1.3f", zscore), "      ",
  sprintf("%1.3f", p), "\n",
  "---------------------------------------------------------"
))


## check lm_robust
est <- estimatr::lm_robust(
  y ~ d * x, data = useit$data,
  weights = useit$data$k, se_type = "HC0"
)
summary(est)$coefficients[2, ]
