#' Bootstrap Methods
#'
#' @param x numeric vector.
#' @param weights numeric vector of weight
#' @param boot how many bootstrap sample is generated?
#' @param alpha significant value to construct confidential interval
#' @param seed seed value
#' @param bca logical. use bias corrected and accelerated (BCa) bootstrap
#'   to construct confidential intervals.
#'
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stats quantile
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' bootstrap(x, boot = 1000)
#' }
#' 
bootstrap <- function(x,
                      weights,
                      boot = 100,
                      alpha = 0.05,
                      seed = 120511,
                      bca = FALSE) {
  # output list
  output <- list()
  # observation information
  x <- na.omit(x)
  n <- length(x)
  output$n <- n
  if (missing(weights)) weights <- rep(1, n)
  output$mu <- mean(x * weights) / mean(weights)

  # bootstrap
  set.seed(seed)
  bootmu <- lapply(seq_len(boot), function(i) {
    pick <- sample(seq_len(n), size = n, replace = TRUE)
    bx <- x[pick]
    bw <- weights[pick]
    mean(bx * bw) / mean(bw)
  })
  bootmu <- unlist(bootmu)
  output$boot$distribution <- bootmu

  # bootstrap standard error
  output$boot$se <- sd(bootmu)

  # bootstrap CI
  output$boot$ci$alpha <- alpha
  if (!bca) {
    # quantile bootstrap
    output$boot$ci$method <- "quantile"
    output$boot$ci$interval <- quantile(
      bootmu, probs = c(alpha / 2, 1 - alpha / 2)
    )
  } else {
    # Bias corrected and accelerated (BCa) bootstrap
    # reference: https://www.erikdrysdale.com/bca_python/
    output$boot$ci$method <- "BCa"

    # bias-correction factor
    z0 <- qnorm(mean(bootmu < output$mu), lower.tail = TRUE)

    # acceleration factor by jackknife
    jack <- lapply(seq_len(n), function(i) mean(x[-i]))
    jack <- unlist(jack)
    jack_mean <- mean(jack)
    a <- sum((jack_mean - jack)^3) / (6 * sum((jack_mean - jack)^2)^(3 / 2))

    # quantile point of normal distribution
    za1 <- qnorm(alpha, lower.tail = TRUE)
    za2 <- qnorm(1 - alpha, lower.tail = TRUE)
    # bias corrected quantile point
    a1 <- pnorm(
      z0 + (z0 + za1) / (1 - a * (z0 + za1))
    )
    a2 <- pnorm(
      z0 + (z0 + za2) / (1 - a * (z0 + za2))
    )

    output$boot$ci$correction.factor <- z0
    output$boot$ci$acceleration.factor <- a
    output$boot$ci$interval <- quantile(bootmu, probs = c(a1, a2))
  }
  output
}

#' Permutation test
#'
#' @param y numeric vector of outcome
#' @param d numeric vector of treatment indicator (contain 1 and 0 only)
#' @param weights numeric vector of weights
#' @param boot how many re-randomization are generated
#' @param seed seed value
#'
#' @examples
#' \dontrun{
#' y1 <- 2 + rnorm(20, sd = 4)
#' y0 <- rnorm(20, sd = 4)
#' d <- sample(c(1, 0), replace = TRUE, size = 40)
#' y <- ifelse(d == 1, y1, y0)
#' permutation(y, d, boot = 200)
#' }
#'
permutation <- function(y,
                        d,
                        weights,
                        boot = 100,
                        seed = 120511) {
  # check same length of outcome and treatment vector
  if (length(y) != length(d)) stop("Same length of y and d")

  # check binary treatment indicator
  if (!(all(unique(d) %in% c(1, 0)))) {
    stop("d must be binary indicator of treated.")
  }

  # create weight vector if missing
  weights <- rep(1, length(y))

  # check missing values
  na_y <- !is.na(y)
  na_d <- !is.na(d)
  na_w <- !is.na(weights)
  keep <- na_y & na_d & na_w

  # use vector
  y <- y[keep]
  w <- weights[keep]
  d <- d[keep]

  y1 <- y[d == 1]
  y0 <- y[d == 0]
  w1 <- w[d == 1]
  w0 <- w[d == 0]

  # output list
  output <- list()

  # information
  n1 <- sum(d == 1)
  n0 <- sum(d == 0)
  output$treat$N <- n1
  output$control$N <- n0

  mean_y1 <- mean(y1 * w1) / mean(w1)
  mean_y0 <- mean(y0 * w0) / mean(w0)
  obs_diff_mean <- mean_y1 - mean_y0

  output$treat$mean <- mean_y1
  output$control$mean <- mean_y0
  output$ate$estimate <- obs_diff_mean

  # re-randomization
  output$ate$method <- "permutation test"
  set.seed(seed)
  bootdiff <- lapply(seq_len(boot), function(i) {
    sim_treated <- sample(seq_len(n1 + n0), size = n1)
    simd <- rep(0, n1 + n0)
    simd[sim_treated] <- 1

    simy1 <- y[simd == 1]
    simy0 <- y[simd == 0]
    simw1 <- w[simd == 1]
    simw0 <- w[simd == 0]

    sim_mean_y1 <- mean(simy1 * simw1) / mean(simw1)
    sim_mean_y0 <- mean(simy0 * simw0) / mean(simw0)
    sim_mean_y1 - sim_mean_y0
  })
  bootdiff <- unlist(bootdiff)
  output$ate$re.random <- bootdiff

  # exact test
  pval <- mean(abs(bootdiff) >= abs(obs_diff_mean))
  output$ate$p.value <- pval

  output
}
