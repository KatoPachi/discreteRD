running <- sample(1:100, size = 1000, replace = TRUE)
cov1 <- rnorm(1000, sd = 2); cov2 <- rnorm(1000, mean = -1)
y0 <- running + cov1 + cov2 + rnorm(1000, sd = 10)
y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000, sd = 10)
y <- ifelse(running <= 50, y1, y0)
bin <- ifelse(y > mean(y), 1, 0)
w <- sample(c(1, 0.5), size = 1000, replace = TRUE)
raw <- data.frame(y, bin, running, cov1, cov2, w)

clear_optDiscRD()

set_optDiscRD(
  y + bin ~ running,
  # xmod = ~ cov1 + cov2,
  discRD.cutoff = 50,
  discRD.assign = "smaller"
)

library(magrittr)
library(modelsummary)

local_random(data = raw) %>% discrd_tab()

discrd_tab(est)

### parameter set: order of polynomial
est <- global_lm(data = raw)
o <- 1
eflab <- "Effect:"
outlab <- c(
  "y" = "A. Simulated Outcome",
  "bin" = "B. Simulated Outcome > 0"
)
###

# extract model.frame and make predicted data
nocov <- unlist(lapply(est$model.frame$cov, identical, character(0)))
o_bool <- est$model.frame$o == o
if (is.null(outlab)) {
  outlab <- unique(est$model.frame[nocov & order_bool, "y"])
  names(outlab) <- outlab
}
polylab <- paste0("poly(x, ", o, ", raw = TRUE)")

mdt <- lapply(names(outlab), function(out) {
  # matched estimation result
  y_bool <- est$model.frame$y == out
  res <- est$res[nocov & o_bool & y_bool][[1]]

  # extract and reshape model.frame
  dt <- model.frame(res)
  dt[[polylab]] <- dt[[polylab]][, 1]
  names(dt)[names(dt) == polylab] <- "x"

  # prediction
  newdt <- expand.grid(list(d = unique(dt$d), x = unique(dt$x)))
  newdt$pred <- predict(res, newdata = newdt)

  # RD effect label
  coef <- broom::tidy(res)
  coef <- coef[coef$term == "d", c("estimate", "std.error", "p.value")]
  estimate <- dplyr::case_when(
    coef$p.value <= .01 ~ sprintf("%1.3f***", coef$estimate),
    coef$p.value <= .05 ~ sprintf("%1.3f**", coef$estimate),
    coef$p.value <= .1 ~ sprintf("%1.3f*", coef$estimate),
    TRUE ~ sprintf("%1.3f", coef$estimate)
  )
  se <- sprintf("(%1.3f)", coef$std.error)
  text <- paste(eflab, estimate, se)
  res$RD.info$eflab <- text

  list(obs = dt, predict = newdt, RD.info = res$RD.info)
})

# set name of list
names(mdt) <- names(outlab)

### parameter set
dlab <- c("Treated", "Control")
###

# make plot data
pdt <- lapply(seq_len(length(mdt)), function (i) {
  # pick up list and its name
  l <- mdt[[i]]
  lname <- names(mdt)[i]
  # recover original running variables
  l$obs$x <- l$obs$x + l$RD.info$cutoff
  l$predict$x <- l$predict$x + l$RD.info$cutoff

  # create weights if NULL
  if (is.null(l$obs$"(weights)")) l$obs$"(weights)" <- 1

  # aggregate observed data by mass points
  l$obs <- aggregate(
    cbind(
      l$obs[[lname]] * l$obs$"(weights)",
      l$obs$d,
      l$obs$"(weights)"
    ),
    by = list(x = l$obs$x), mean
  )

  ## weighted average of outcome
  l$obs$V1 <- l$obs$V1 / l$obs$V3

  ## factor of treatment
  l$obs$V2 <- factor(l$obs$V2, levels = c(1, 0), labels = dlab)

  ## del weight
  l$obs <- l$obs[, c("x", "V1", "V2")]

  ## rename colnames
  colnames(l$obs) <- c("x", "y", "d")

  # separate prediction data by cutoff and treatment
  if (l$RD.info$assign == "smaller") {
    d1 <- subset(l$predict, x <= l$RD.info$cutoff & d == 1)
    d0 <- subset(l$predict, x >= l$RD.info$cutoff & d == 0)
  } else {
    d1 <- subset(l$predict, x >= l$RD.info$cutoff & d == 1)
    d0 <- subset(l$predict, x <= l$RD.info$cutoff & d == 0)
  }

  # output
  list(obs = l$obs, d1 = d1, d0 = d0)
})

# set name of list
names(pdt) <- names(outlab)

### parameter
xlab <- "Running Variables"
ylab <- "Average"
inplot_text_size <- 5
adjust_x_pos <- 0
adjust_y_pos <- quote(ifelse(out == "bin", -0.4, 0))
###

# list of plot
plot_list <- lapply(names(outlab), function(out) {
  # plot data
  p <- pdt[[out]]
  m <- mdt[[out]]

  # base position of in-plot text
  base_x_pos <- (max(p$obs$x) + m$RD.info$cutoff) / 2
  base_y_pos <- p$obs$y[p$obs$x == base_x_pos] / 2

  # plot
  gplot <- ggplot2::ggplot(p$obs, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(ggplot2::aes(shape = d), size = 2) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = pred), p$d0) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = pred), p$d1) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = m$RD.info$cutoff), linetype = 3
    ) +
    ggplot2::annotate(
      "text",
      x = base_x_pos + eval(adjust_x_pos),
      y = base_y_pos + eval(adjust_y_pos),
      label = m$RD.info$eflab,
      size = inplot_text_size
    ) +
    ggplot2::labs(x = xlab, y = ylab, title = outlab[out], shape = NULL) +
    simplegg()

})

library(patchwork)
patchwork::wrap_plots(plot_list, ncol = 2)
