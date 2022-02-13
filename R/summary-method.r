#' Summary function
#'
#' @param object object with "local_random_test" class
#' @param \dots additional arguments affecting the summary produced.
#'
#' @method summary local_random_test
#' @export
#'
summary.local_random_test <- function(object, ...) {
  rdinfo <- object$RD.info
  bwinfo <- object$bandwidth
  res <- object$estimate

  show <- paste0(
    "Local ATE by Local Random Approach\n\n",
    "RD Design Information\n",
    "Running variable: ", rdinfo$running.variable, "\n"
  )

  if (object$RD.info$assignment == "greater") {
    show <- paste0(
      show,
      "Assignment rule: 1[x >=", rdinfo$cutoff, "]\n\n"
    )
  } else {
    show <- paste0(
      show,
      "Assignment rule: 1[x <=", rdinfo$cutoff, "]\n\n"
    )
  }

  show <- paste0(
    show,
    "Test information \n"
  )

  if (object$bandwidth$global) {
    show <- paste0(
      show,
      "Use all observation: ", bwinfo$global, "\n"
    )
  } else {
    show <- paste0(
      show,
      "Use all observation: ", bwinfo$global, "\n",
      "Bandiwdth: [", bwinfo$bw[1] + rdinfo$cutoff, ",",
      bwinfo$bw[2] + rdinfo$cutoff, "]\n"
    )
  }

  show <- paste0(
    show,
    "Method of statistical test: ", res[[1]]$local.ate$method,
    "\n\n"
  )

  for (i in seq_len(length(res))) {
    show <- paste0(
      show,
      "Outcome variables: ", res[[i]]$outcome, "\n",
      "---------------------------------------------------------------\n",
      "    Treated                 Control             Local ATE      \n",
      "N    Mean   S.E.        N    Mean   S.E.    Estimate    P-value\n",
      "---------------------------------------------------------------\n",
      res[[i]]$observe$treat$N, "  ",
      sprintf("%1.2f", res[[i]]$observe$treat$mean),  "  ",
      sprintf("%1.2f", res[[i]]$observe$treat$se), "       ",
      res[[i]]$observe$control$N, "  ",
      sprintf("%1.2f", res[[i]]$observe$control$mean), "  ",
      sprintf("%1.2f", res[[i]]$observe$control$se), "     ",
      sprintf("%1.2f", res[[i]]$local.ate$estimate), "     ",
      sprintf("%1.2f", res[[i]]$local.ate$p.value), "\n",
      "---------------------------------------------------------------\n\n"
    )
  }

  cat(show)

}
