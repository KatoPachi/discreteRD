#' Data Shape
#'
#' @param basemod baseline formula. `outcome ~ running variable`.
#' @param covmod one-sided formula with covariates on rhs.
#' @param data data.frame
#' @param subset subset condition.
#' @param weights weight variable.
#' @param na.action how missing values are treated.
#'   Default is na.omit (imcomplete cases removed).
#' @param cutoff numeric of cutoff point.
#'   If missing, search `option("discRD.cutoff")`
#' @param assign assignment rule of treatment.
#'   If "greater",
#'   treated whose running variable is greater than or equal to cutoff.
#'   If "smaller",
#'   treated whose running variable is less than or equal to cutoff.
#'   If missing, search `option("discRD.assign")`
#'
#' @importFrom stats update
#' @importFrom stats na.omit
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#' @importFrom stats model.frame
#'
#' @examples 
#' \dontrun{
#' running <- sample(1:100, size = 1000, replace = TRUE)
#' cov1 <- rnorm(1000, sd = 2); cov2 <- rnorm(1000, mean = -1)
#' y0 <- running + cov1 + cov2 + rnorm(1000, sd = 10)
#' y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000, sd = 10)
#' y <- ifelse(running <= 50, y1, y0)
#' bin <- ifelse(y > mean(y), 1, 0)
#' w <- sample(c(1, 0.5), size = 1000, replace = TRUE)
#' raw <- data.frame(y, bin, running, cov1, cov2, w)
#'
#' set_optDiscRD(discRD.cutoff = 50, discRD.assign = "smaller")
#' a <- new_clean_rd_data(y ~ running + cov1, data = raw2, weights = w)
#' str(a)
#' }
#'
#'
clean_rd_data <- function(basemod,
                          covmod,
                          data,
                          subset,
                          weights,
                          na.action = na.omit,
                          cutoff,
                          assign) {
  ## make formula
  mod <- basemod
  if (!missing(covmod)) { 
    mod <- update(mod, paste0(c(". ~ .", all.vars(covmod)), collapse = "+"))
  }

  ## weight & subset condition vector
  wv <- NULL
  if (!missing(weights)) {
    weights <- rlang::enquo(weights)
    wv <- rlang::eval_tidy(weights, data)
  }

  tfv <- NULL
  if (!missing(subset)) {
    subset <- rlang::enquo(subset)
    tfv <- rlang::eval_tidy(subset, data)
  }

  ## clean data by model.frame
  args <- list(
    formula = mod,
    data = data,
    subset = tfv,
    weights = wv,
    na.action = na.action
  )

  clean <- do.call("model.frame", args)

  # shape clean_dt to adopt RD desing easily
  running <- all.vars(basemod)[2]

  if (missing(cutoff)) {
    cutoff <- getOption("discRD.cutoff")
    message("Use cutoff value registered by global options")
  }

  if (missing(assign)) {
    assign <- getOption("discRD.assign")
    message("Use assignment rule registered by global options")
  }

  usedt <- clean
  usedt$x <- usedt[, running] - cutoff

  if (assign == "greater") {
    usedt$d <- ifelse(usedt$x >= 0, 1, 0)
  } else if (assign == "smaller") {
    usedt$d <- ifelse(usedt$x <= 0, 1, 0)
  } else {
    stop("Unknown assignment rule is used.")
  }

  usedt <- usedt[, names(usedt) != running]

  list(
    data = usedt,
    RD.info = list(
      running.variable = running,
      cutoff = cutoff,
      assignment = assign
    )
  )
}