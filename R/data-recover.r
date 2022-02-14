#' Recover Data from estimation object
#'
#' @param object estimation object
#' @param \dots other arguments
#' @export
#'
recover_data <- function(object, ...) {
  UseMethod("recover_data")
}

#'
#' @param object object with global_lm class
#' @param modnum numeric. which model you use.
#' @name recover_data
#' @export
#'
recover_data.global_lm <- function(object, modnum) {
  # result list
  use_result <- object$result[[modnum]]

  # control and treated data
  d0 <- cbind(
    use_result$control$input$response,
    use_result$control$input$design,
    weights = use_result$control$input$weights,
    d = 0
  )

  d1 <- cbind(
    use_result$treat$input$response,
    use_result$treat$input$design,
    weights = use_result$treat$input$weights,
    d = 1
  )

  xlab <- colnames(use_result$control$input$design)
  d <- data.frame(rbind(d0, d1))
  colnames(d) <- c("outcome", xlab, "weights", "d")
  d
}