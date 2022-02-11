#' Output Table of Empirical Analysis for Discrete RD
#'
#' @param \dots some arguments.
#'   See [discrd_tab.local_random()] for arguments of "local_random" class.
#'
#' @export
#' 
discrd_tab <- function(...) {
  UseMethod("discrd_tab")
}

#' Output Table for Local Random Approach
#'
#' @param data data.frame with "local_random" class
#' @param ylab string vector with outcome labels.
#'   Specify c("original label" = "new label", ...)
#' @param outcome_lab string of label of outcome column.
#' @param meanlab string of label of columns showing average.
#' @param selab string of label of columns showing standard error.
#' @param nlab string of label of columns showing number of observations.
#' @param meandiff_lab string of label of columns showing
#'   mean difference.
#' @param plab string of label of columns showing p-value of
#'   t-test (or permutation test).
#' @param treat_header string of label of treatment
#' @param control_header string of label of control
#' @param title string of table title
#' @param footnote string of footnote
#' @param output string of output format
#'   (default is getOption("discRD.table_output"))
#' @param fontsize numeric of font size
#'   (default is getOption("discRD.table_fontsize"))
#' @param digits numeric of number of decimal places to display
#'   (Default is 3).
#' @param \dots Other arguments to pass to `kableExtra::kable_styling`
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#' @importFrom modelsummary datasummary
#' @importFrom tables Heading
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_row
#' @importFrom flextable fontsize
#' @method discrd_tab local_random
#' @export
#'
#' @examples
#' running <- sample(1:100, size = 1000, replace = TRUE)
#' cov1 <- rnorm(1000, sd = 2); cov2 <- rnorm(1000, mean = -1)
#' y0 <- running + cov1 + cov2 + rnorm(1000)
#' y1 <- 2 + 1.5 * running + cov1 + cov2 + rnorm(1000)
#' y <- ifelse(running <= 50, y1, y0)
#' bin <- ifelse(y > mean(y), 1, 0)
#' raw <- data.frame(y, bin, running, cov1, cov2)
#'
#' set_optDiscRD(
#'   y + bin ~ running,
#'   discRD.cutoff = 50,
#'   discRD.assign = "smaller"
#' )
#'
#' library(modelsummary)
#' library(magrittr)
#'
#' ylab <- c("y" = "Simulated Outcome", "bin" = "Binary")
#' footnote <- paste0(
#'   "Observations with running variables of -45 to 55 were used.",
#'   "The treatment variable is determined to take 1",
#'   "if the running variable is 50 or less."
#' )
#'
#' local_random(data = raw, bw = c(-5, 5)) %>%
#'   discrd_tab(ylab, digits = 2, footnote = footnote)
#'
discrd_tab.local_random <- function(
  data, ylab,
  outcome_lab = "Outcomes",
  meanlab = "Mean", selab = "S.E.", nlab = "N",
  meandiff_lab = "Mean difference", plab = "P-value",
  treat_header = "Treated", control_header = "Control",
  title = NULL,
  footnote = NULL,
  output = getOption("discRD.table_output"),
  fontsize = getOption("discRD.table_fontsize"),
  digits = 3,
  ...
) {
  if (!missing(ylab)) {
    outcome <- NULL
    data <- data %>%
      dplyr::mutate(outcome = factor(
        dplyr::recode(outcome, !!!ylab), levels = ylab
      ))
  }

  rawvalue <- function(x) x
  tab <- modelsummary::datasummary(
    (Heading(outcome_lab, character.only = TRUE) * outcome) ~ rawvalue * (
      (Heading(meanlab, character.only = TRUE) * mean_y1) +
        (Heading(selab, character.only = TRUE) * se_y1) +
        (Heading(nlab, character.only = TRUE) * n1 * Format(digits = 0)) +
        (Heading(meanlab, character.only = TRUE) * mean_y0) +
        (Heading(selab, character.only = TRUE) * se_y0) +
        (Heading(nlab, character.only = TRUE) * n0 * Format(digits = 0)) +
        (Heading(meandiff_lab, character.only = TRUE) * mean_diff) +
        (Heading(plab, character.only = TRUE) * p)
    ),
    data = data,
    align = "lcccccccc",
    title = title, output = output, fmt = digits
  )

  header <- c(" ", rep(treat_header, 3), rep(control_header, 3), " ", " ")
  parse_header <- rle(header)
  head_length <- parse_header$lengths
  names(head_length) <- parse_header$values

  if (output == "kableExtra") {
    tab %>%
      kableExtra::kable_styling(font_size = fontsize, ...) %>%
      kableExtra::add_header_above(head_length) %>%
      kableExtra::footnote(
        general_title = "",
        general = footnote,
        threeparttable = TRUE,
        escape = FALSE
      )
  } else if (output == "flextable") {
    tab %>%
      flextable::add_footer_lines(values = footnote) %>%
      flextable::add_header_row(
        values = parse_header$values,
        colwidths = parse_header$lengths
      ) %>%
      flextable::fontsize(size = fontsize, part = "all")
  } else {
    tab
  }
}
