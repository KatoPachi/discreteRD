#' Output Table of Empirical Analysis for Discrete RD
#'
#' @param \dots some arguments.
#'   See [tabular.local_random_test()] for arguments of "local_random" class.
#'   See [tabular.global_lm()]
#'   for arguments of "discRD_global_lm" class.
#'
#' @export
#'
tabular <- function(...) {
  UseMethod("tabular")
}

#' Output Table for Local Random Approach
#'
#' @param object object with "local_random_test" class
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
#' @param size numeric of font size
#'   (default is getOption("discRD.table_fontsize"))
#' @param digits numeric of number of decimal places to display
#'   (Default is 3).
#' @param \dots Other arguments to pass to `kableExtra::kable_styling`
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
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
#' @method tabular local_random_test
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
#' local_random_test(data = raw, bw = c(-5, 5)) %>%
#'   tabular(ylab, digits = 2, footnote = footnote)
#'
tabular.local_random_test <- function(object,
                                      ylab,
                                      outcome_lab = "Outcomes",
                                      meanlab = "Mean",
                                      selab = "S.E.",
                                      nlab = "N",
                                      meandiff_lab = "Mean difference",
                                      plab = "P-value",
                                      treat_header = "Treated",
                                      control_header = "Control",
                                      title = NULL,
                                      footnote = NULL,
                                      output = getOption("discRD.table_output"),
                                      size = getOption("discRD.table_fontsize"),
                                      digits = 3,
                                      ...) {
  res <- object$estimate
  data <- lapply(res, function(l) {
    data.frame(
      outcome = l$outcome,
      mean_y1 = l$observe$treat$mean,
      se_y1 = l$observe$treat$se,
      n1 = l$observe$treat$N,
      mean_y0 = l$observe$control$mean,
      se_y0 = l$observe$control$se,
      n0 = l$observe$control$N,
      mean_diff = l$local.ate$estimate,
      p = l$local.ate$p.value
    )
  })

  data <- dplyr::bind_rows(data)

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
      (Heading(nlab, character.only = TRUE) * n1 * Format(digits = 0)) +
      (Heading(meanlab, character.only = TRUE) * mean_y1) +
      (Heading(selab, character.only = TRUE) * se_y1) +
      (Heading(nlab, character.only = TRUE) * n0 * Format(digits = 0)) +
      (Heading(meanlab, character.only = TRUE) * mean_y0) +
      (Heading(selab, character.only = TRUE) * se_y0) +
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
      kableExtra::kable_styling(font_size = size, ...) %>%
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
      flextable::fontsize(size = size, part = "all")
  } else {
    tab
  }
}

#' Output Table for Global Local Polynomials
#'
#' @param object object with "global_lm" class
#' @param ylab a named string vector of outcome variables
#' @param dlab a string of label of treatment variable
#' @param olab a string of label of "Order of polynomial"
#' @param covariate_labs list.
#'   You can specify how to display information about
#'   whether you are controlling a variable that is not displayed by a factor.
#'   For example,
#'   `list("label 1" = "x1")` changes the label of "x1" to "label 1".
#'   Also, when specified as `list("label" = c ("x1", "x2"))`,
#'   if x1 and x2 are not controlled or
#'   the coefficients are not displayed in all models,
#'   "x1" and "x2" labels are aggregated into one line with the name "label".
#' @param stars a named numeric vector to indicate statistical significance.
#'   `"***" = 0.01` means show *** if p-value is less than or equal to 0.01.
#' @param title a string of title
#' @param footnote a string of footnote
#' @param output a string of output format.
#'   If missing, find `options("discRD.table_output")`.
#' @param fontsize numeric of font size.
#'   If missing, find `options("discRD.table_fontsize")`.
#' @param digits numeric.
#'   the number of digits to keep after the period.
#' @param \dots Other arguments to pass to `kableExtra::kable_styling`
#'
#' @importFrom magrittr %>%
#' @importFrom modelsummary modelsummary
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra pack_rows
#' @importFrom kableExtra footnote
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#' @importFrom flextable as_grouped_data
#' @importFrom flextable as_flextable
#' @importFrom flextable set_header_labels
#' @importFrom flextable set_caption
#' @importFrom flextable add_footer_row
#' @importFrom flextable align
#' @importFrom flextable border_remove
#' @importFrom flextable hline_top
#' @importFrom officer fp_border
#' @importFrom flextable hline_bottom
#' @importFrom flextable fontsize
#' @importFrom flextable autofit
#' @method tabular global_lm
#' @export
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
#' set_optDiscRD(
#'   y + bin ~ running,
#'   xmod = ~ cov1 + cov2,
#'   discRD.cutoff = 50,
#'   discRD.assign = "smaller"
#' )
#'
#' library(magrittr)
#'
#' global_lm(data = raw) %>%
#'   tabular(
#'     title = "Estimate Local ATE by Global Polynomial Fitting",
#'     ylab = c("y" = "Simulated Outcome", "bin" = "Simulated Outcome > 0"),
#'     dlab = "Treatment",
#'     covariate_labs = list("Covariates" = c("cov1", "cov2")),
#'     footnote = "***: p < 0.01, **: p < 0.05, *: p < 0.1"
#'   )
#' }
#'
tabular.global_lm <- function(object,
                              ylab,
                              dlab = "treated",
                              olab = "Order of polynomial",
                              covariate_labs,
                              stars = c("***" = .01, "**" = .05, "*" = .1),
                              title = NULL,
                              footnote = NULL,
                              output = getOption("discRD.table_output"),
                              fontsize = getOption("discRD.table_fontsize"),
                              digits = 3,
                              ...) {
  # Step 1: Create add_rows tabulation
  xlist <- lapply(object$model.outline$covmod, all.vars)
  base_addtab <- data.frame(x = unique(unlist(xlist)))

  ## which covariates each model includes
  for (i in seq_len(length(xlist))) {
    base_addtab[, i + 1] <- apply(
      as.matrix(base_addtab[, 1], ncol = 1),
      MARGIN = 1, function(x) sum(xlist[[i]] == x)
    )
  }

  ## create flag whether it can be grouped
  if (!missing(covariate_labs)) {
    x <- NULL
    flag <- NULL
    for (i in seq_len(length(covariate_labs))) {
      ctrl <- apply(
        subset(base_addtab, x %in% covariate_labs[[i]])[, -1], 2, sum
      )
      flag[i] <- ifelse(
        sum(0 < ctrl & ctrl < length(covariate_labs[[i]])) != 0, 0, 1
      )
    }

    "%out%" <- Negate("%in%")
    # replace grouped covariates into group label
    addtab <- base_addtab
    for (i in seq_len(length(flag))) {
      if (flag[i] == 1) {
        bool <- apply(
          subset(base_addtab, x %in% covariate_labs[[i]])[, -1],
          2, function(x) 1 * (sum(x) > 0)
        )
        addtab <- data.frame(x = names(covariate_labs)[i], t(bool))
        sub_base_addtab <- subset(base_addtab, x %out% covariate_labs[[i]])
        addtab <- dplyr::bind_rows(sub_base_addtab, addtab)
      }
    }
  } else {
    addtab <- base_addtab
  }

  ## recode flags
  for (i in seq_len(ncol(addtab) - 1)) {
    addtab[, 1 + i] <- ifelse(addtab[, 1 + i] == 1, "X", "")
  }

  ## add info of polynomial order
  addtab <- rbind(addtab, c(olab, as.character(object$model.outline$order)))

  ## add columns
  addtab <- cbind(rep("", nrow(addtab)), addtab)
  addtab <- cbind(rep("add_rows", nrow(addtab)), addtab)

  ## rename columns
  colnames(addtab) <- c(
    "part", "group", "term",
    paste("Model", seq_len(length(object$result)))
  )

  # Step 2: run {modelsummary} and reshape
  keep_coef <- dlab
  names(keep_coef) <- "Local ATE"

  tab <- modelsummary::modelsummary(
    object$result,
    coef_map = keep_coef,
    gof_omit = "se",
    group = outcome + term ~ model,
    stars = stars,
    fmt = digits,
    output = "data.frame"
  )

  ## Add rows
  tab <- tab[tab$term != "R2", ]
  tab$term <- ifelse(tab$statistic == "modelsummary_tmp2", "", tab$term)
  tab <- tab[, -4]
  tab <- dplyr::bind_rows(tab, addtab)

  ## Separate tables by group
  group_id <- if (missing(ylab)) {
    unique(tab[tab$part == "estimates", "group"])
  } else {
    names(ylab)
  }

  group <- NULL
  septab <- lapply(group_id, function(x) {
    estimates <- tab[tab$group == x, ]
    keep <- apply(estimates, MARGIN = 2, function(x) any(x != ""))
    append_tab <- dplyr::bind_rows(
      estimates[, keep],
      tab[tab$part != "estimates", keep]
    )
    append_tab$group <- x
    append_tab <- append_tab[, -1]
    colnames(append_tab) <- c(
      "group", "term", paste0("Model", seq_len(ncol(append_tab) - 2))
    )
    append_tab
  })

  ## append seprated tab
  tab <- dplyr::bind_rows(septab)

  # Step 3: kebleExtra and flextable format
  if (output == "kableExtra") {
    ktab <- knitr::kable(
      tab[, -1],
      caption = title,
      align = paste0(c("l", rep("c", ncol(tab) - 2))),
      col.names = c("", paste0("(", seq_len(ncol(tab) - 2), ")")),
      booktabs = TRUE, linesep = "", escape = FALSE,
    )

    ktab <- kableExtra::kable_styling(ktab, font_size = fontsize, ...)

    for (i in group_id) {
      numrow <- seq_len(nrow(tab))
      start_line <- min(numrow[tab$group == i])
      end_line <- max(numrow[tab$group == i])
      lab_line <- if (missing(ylab)) i else ylab[i]
      ktab <- kableExtra::pack_rows(ktab, lab_line, start_line, end_line)
    }

    ktab %>%
      kableExtra::footnote(
        general_title = "",
        general = footnote,
        threeparttable = TRUE,
        escape = FALSE
      )

  } else if (output == "flextable") {

    if (!missing(ylab)) {
      ftab <- tab %>%
        dplyr::mutate(group = dplyr::recode(group, !!!ylab))
    } else {
      ftab <- tab
    }

    header_labs <- vector("list", ncol(tab))
    names(header_labs) <- colnames(tab)
    header_labs$term <- ""
    for (i in seq_len(length(header_labs) - 2)) {
      header_labs[[i + 2]] <- paste0("(", i, ")")
    }

    ftab <- ftab %>%
      flextable::as_grouped_data(groups = "group") %>%
      flextable::as_flextable(hide_grouplabel = TRUE) %>%
      flextable::set_header_labels(values = header_labs)

    if (!is.null(title)) {
      ftab <- ftab %>% flextable::set_caption(title)
    }

    if (!is.null(footnote)) {
      ftab <- ftab %>% flextable::add_footer_row(
        values = footnote,
        colwidths = ncol(tab) - 1
      )
    }

    ftab %>%
      flextable::align(
        j = seq(2, 1 + ncol(tab) - 2), align = "center",
        part = "all"
      ) %>%
      flextable::border_remove() %>%
      flextable::hline_top(part = "all", border = officer::fp_border()) %>%
      flextable::hline_bottom(
        part = "header", border = officer::fp_border()
      ) %>%
      flextable::hline_bottom(
        part = "body", border = officer::fp_border()
      ) %>%
      flextable::fontsize(size = fontsize, part = "all") %>%
      flextable::autofit()

  } else {

    if (!missing(ylab)) {
      tab %>% dplyr::mutate(group = dplyr::recode(group, !!!ylab))
    } else {
      tab
    }

  }
}

#' Output Table for Local Polynomials
#'
#' @param object object with "global_lm" class
#' @param ylab a named string vector of outcome variables
#' @param dlab a string of label of treatment variable
#' @param olab a string of label of "Order of polynomial"
#' @param stars a named numeric vector to indicate statistical significance.
#'   `"***" = 0.01` means show *** if p-value is less than or equal to 0.01.
#' @param title a string of title
#' @param footnote a string of footnote
#' @param output a string of output format.
#'   If missing, find `options("discRD.table_output")`.
#' @param fontsize numeric of font size.
#'   If missing, find `options("discRD.table_fontsize")`.
#' @param digits numeric.
#'   the number of digits to keep after the period.
#' @param \dots Other arguments to pass to `kableExtra::kable_styling`
#'
#' @importFrom magrittr %>%
#' @importFrom modelsummary modelsummary
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra pack_rows
#' @importFrom kableExtra footnote
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#' @importFrom flextable as_grouped_data
#' @importFrom flextable as_flextable
#' @importFrom flextable set_header_labels
#' @importFrom flextable set_caption
#' @importFrom flextable add_footer_row
#' @importFrom flextable align
#' @importFrom flextable border_remove
#' @importFrom flextable hline_top
#' @importFrom officer fp_border
#' @importFrom flextable hline_bottom
#' @importFrom flextable fontsize
#' @importFrom flextable autofit
#' @method tabular local_lm
#' @export
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
#' set_optDiscRD(
#'   y + bin ~ running,
#'   xmod = ~ cov1 + cov2,
#'   discRD.cutoff = 50,
#'   discRD.assign = "smaller"
#' )
#'
#' library(magrittr)
#'
#' local_lm(data = raw) %>%
#'   tabular(
#'     title = "Estimate Local ATE by Local Polynomial Fitting",
#'     ylab = c("y" = "Simulated Outcome", "bin" = "Simulated Outcome > 0"),
#'     dlab = "Treatment",
#'     covariate_labs = list("Covariates" = c("cov1", "cov2")),
#'     footnote = "***: p < 0.01, **: p < 0.05, *: p < 0.1"
#'   )
#' }
#'
tabular.local_lm <- function(object,
                              ylab,
                              dlab = "treated",
                              olab = "Order of polynomial",
                              stars = c("***" = .01, "**" = .05, "*" = .1),
                              title = NULL,
                              footnote = NULL,
                              output = getOption("discRD.table_output"),
                              fontsize = getOption("discRD.table_fontsize"),
                              digits = 3,
                              ...) {
  # Step 1: Create add_rows tabulation
  addtab <- data.frame(
    t(c(olab, as.character(object$model.outline$order)))
  )

  ## add columns
  addtab <- cbind(rep("", nrow(addtab)), addtab)
  addtab <- cbind(rep("add_rows", nrow(addtab)), addtab)

  ## rename columns
  colnames(addtab) <- c(
    "part", "group", "term",
    paste("Model", seq_len(length(object$result)))
  )

  # Step 2: run {modelsummary} and reshape
  keep_coef <- dlab
  names(keep_coef) <- "Local ATE"

  tab <- modelsummary::modelsummary(
    object$result,
    coef_map = keep_coef,
    gof_omit = "se",
    group = outcome + term ~ model,
    stars = stars,
    fmt = digits,
    output = "data.frame"
  )

  ## Add rows
  tab <- tab[tab$term != "R2", ]
  tab$term <- ifelse(tab$statistic == "modelsummary_tmp2", "", tab$term)
  tab <- tab[, -4]
  tab <- dplyr::bind_rows(tab, addtab)

  ## Separate tables by group
  group_id <- if (missing(ylab)) {
    unique(tab[tab$part == "estimates", "group"])
  } else {
    names(ylab)
  }

  group <- NULL
  septab <- lapply(group_id, function(x) {
    estimates <- tab[tab$group == x, ]
    keep <- apply(estimates, MARGIN = 2, function(x) any(x != ""))
    append_tab <- dplyr::bind_rows(
      estimates[, keep],
      tab[tab$part != "estimates", keep]
    )
    append_tab$group <- x
    append_tab <- append_tab[, -1]
    colnames(append_tab) <- c(
      "group", "term", paste0("Model", seq_len(ncol(append_tab) - 2))
    )
    append_tab
  })

  ## append seprated tab
  tab <- dplyr::bind_rows(septab)

  # Step 3: kebleExtra and flextable format
  if (output == "kableExtra") {
    ktab <- knitr::kable(
      tab[, -1],
      caption = title,
      align = paste0(c("l", rep("c", ncol(tab) - 2))),
      col.names = c("", paste0("(", seq_len(ncol(tab) - 2), ")")),
      booktabs = TRUE, linesep = "", escape = FALSE,
    )

    ktab <- kableExtra::kable_styling(ktab, font_size = fontsize, ...)

    for (i in group_id) {
      numrow <- seq_len(nrow(tab))
      start_line <- min(numrow[tab$group == i])
      end_line <- max(numrow[tab$group == i])
      lab_line <- if (missing(ylab)) i else ylab[i]
      ktab <- kableExtra::pack_rows(ktab, lab_line, start_line, end_line)
    }

    ktab %>%
      kableExtra::footnote(
        general_title = "",
        general = footnote,
        threeparttable = TRUE,
        escape = FALSE
      )

  } else if (output == "flextable") {

    if (!missing(ylab)) {
      ftab <- tab %>%
        dplyr::mutate(group = dplyr::recode(group, !!!ylab))
    } else {
      ftab <- tab
    }

    header_labs <- vector("list", ncol(tab))
    names(header_labs) <- colnames(tab)
    header_labs$term <- ""
    for (i in seq_len(length(header_labs) - 2)) {
      header_labs[[i + 2]] <- paste0("(", i, ")")
    }

    ftab <- ftab %>%
      flextable::as_grouped_data(groups = "group") %>%
      flextable::as_flextable(hide_grouplabel = TRUE) %>%
      flextable::set_header_labels(values = header_labs)

    if (!is.null(title)) {
      ftab <- ftab %>% flextable::set_caption(title)
    }

    if (!is.null(footnote)) {
      ftab <- ftab %>% flextable::add_footer_row(
        values = footnote,
        colwidths = ncol(tab) - 1
      )
    }

    ftab %>%
      flextable::align(
        j = seq(2, 1 + ncol(tab) - 2), align = "center",
        part = "all"
      ) %>%
      flextable::border_remove() %>%
      flextable::hline_top(part = "all", border = officer::fp_border()) %>%
      flextable::hline_bottom(
        part = "header", border = officer::fp_border()
      ) %>%
      flextable::hline_bottom(
        part = "body", border = officer::fp_border()
      ) %>%
      flextable::fontsize(size = fontsize, part = "all") %>%
      flextable::autofit()

  } else {

    if (!missing(ylab)) {
      tab %>% dplyr::mutate(group = dplyr::recode(group, !!!ylab))
    } else {
      tab
    }

  }
}
