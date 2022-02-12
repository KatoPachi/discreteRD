running <- sample(1:100, size = 1000, replace = TRUE)
cov1 <- rnorm(1000, sd = 2); cov2 <- rnorm(1000, mean = -1)
y0 <- running + cov1 + cov2 + rnorm(1000, sd = 10)
y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000, sd = 10)
y <- ifelse(running <= 50, y1, y0)
bin <- ifelse(y > mean(y), 1, 0)
w <- sample(c(1, 0.5), size = 1000, replace = TRUE)
raw <- data.frame(y, bin, running, cov1, cov2, w)

set_optDiscRD(
  y + bin ~ running,
  xmod = ~ cov1 + cov2,
  discRD.cutoff = 50,
  discRD.assign = "smaller"
)

library(magrittr)
library(modelsummary)
local_random(data = raw) %>%
  discrd_tab()

undebug(global_lm)
global_lm(data = raw, weight = "w") %>%
  discrd_tab()

o <- 1
nocov <- unlist(lapply(est$model.frame$cov, identical, character(0)))
order_bool <- est$model.frame$o == o
useit <- est$res[nocov * order_bool]

head(model.frame(useit[[2]]))
str(est$res[[1]])
