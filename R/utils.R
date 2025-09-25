#' Action log table (prototype)
#' @param history list of actions from triage_apply
#' @return data.frame
#' @export
sem_action_log_table <- function(history) {
  if (is.null(history) || length(history) == 0) return(data.frame())
  do.call(rbind, lapply(history, function(h) {
    data.frame(step = h$step, type = h$type, kept = h$kept, note = h$note)
  }))
}

#' One-line pre/post sentence (prototype)
#' @export
prepost_sentence <- function(dat, model, res) {
  pre <- res$fit$pre; post <- res$fit$post
  sprintf("Fit improved from CFI %.3f, RMSEA %.3f to CFI %.3f, RMSEA %.3f.",
          pre$cfi %||% NA, pre$rmsea %||% NA, post$cfi %||% NA, post$rmsea %||% NA)
}
