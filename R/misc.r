#' Parse Model
#'
#' @param mod formula
#'
#' @importFrom stats terms
#'
#'
parse_model <- function(mod) {
  full_var <- all.vars(mod)
  rhs <- attr(terms(mod), "term.labels")
  lhs <- full_var[!(full_var %in% rhs)]
  list(lhs = lhs, rhs = rhs)
}

#' Data Shape
#'
#' @param y string vector with outcome variables
#' @param x string with running variable
#' @param cutoff numeric of cutoff points
#' @param assign assignment rule of treatment.
#'   If "greater",
#'   treated whose running variable is greater than or equal to cutoff.
#'   If "smaller",
#'   treated whose running variable is less than or equal to cutoff.
#' @param data original data.frame
#' @param \dots other string vector with variables to keep
#'
#' @importFrom stats complete.cases
#'
#' @examples 
#' \dontrun{
#' running <- sample(1:100, size = 1000, replace = TRUE)
#' cov1 <- rnorm(1000, sd = 2); cov2 <- rnorm(1000, mean = -1)
#' y0 <- running + cov1 + cov2 + rnorm(1000)
#' y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000)
#' y <- ifelse(running <= 50, y1, y0)
#' raw <- data.frame(y, running, cov1, cov2)
#'
#' with(raw, plot(running, y, pch = ifelse(running <= 50, 16, 17)))
#'
#' dt <- shape_data("y", "running", 50, "smaller", raw)
#' with(dt, plot(x, y, pch = ifelse(d == 1, 16, 17)))
#'
#' set_optDiscRD(
#'   y ~ running,
#'   discRD.cutoff = 50,
#'   discRD.assign = "smaller"
#' )
#' dt2 <- shape_data(data = raw, cov = c("cov1", "cov2"))
#' with(dt2, plot(x, y, pch = ifelse(d == 1, 16, 17)))
#' }
#'
#' 
shape_data <- function(y, x, cutoff, assign, data, ...) {
  # collect unnamed arguments
  args <- list(...)
  xlist <- unique(unlist(args))

  # check global options for outcome and running
  if (missing(y)) {
    y <- getOption("discRD.outcome") 
    if (all(y == "")) stop("Specify outcome in argument or global option.") 
  }

  if (missing(x)) {
    x <- getOption("discRD.running")
    if (x == "") stop("Specify running variable in argument or global option.")
  }

  # data shaping
  usedt <- data[,c(y, x, xlist)]
  usedt <- usedt[complete.cases(usedt),]
  colnames(usedt) <- c(y, "x", xlist)

  # normalize running variable
  ## check global options for cutoff if missng
  if (missing(cutoff)) {
    cutoff <- getOption("discRD.cutoff")
    message("Find and use cutoff value from global options")
  }

  ## normalize running variable
  usedt[, "x"] <- usedt[, "x"] - cutoff

  # create treatment variable
  ## check global options for assignment rule is missing
  if (missing(assign)) {
    assign <- getOption("discRD.assign")
    message("Find and use assignment rule from global options")
  }

  if (assign == "greater") {
    usedt$d <- ifelse(usedt$x >= 0, 1, 0)
  } else if (assign == "smaller") {
    usedt$d <- ifelse(usedt$x <= 0, 1, 0)
  } else {
    stop("Unknown assignment rule is used.")
  }

  list(
    data = usedt,
    RD.info = list(cutoff = cutoff, assign = assign)
  )
}
