#' Pre/post comparison table (prototype)
#' @return data.frame with pre/post fit measures
#' @export
sem_compare_table <- function(dat, model, res) {
  pre  <- res$fit$pre
  post <- res$fit$post
  data.frame(
    Stage = c("Pre-screening","Post-screening"),
    CFI   = c(pre$cfi %||% NA, post$cfi %||% NA),
    TLI   = c(pre$tli %||% NA, post$tli %||% NA),
    RMSEA = c(pre$rmsea %||% NA, post$rmsea %||% NA),
    SRMR  = c(pre$srmr %||% NA, post$srmr %||% NA),
    AIC   = c(pre$aic %||% NA, post$aic %||% NA),
    BIC   = c(pre$bic %||% NA, post$bic %||% NA),
    stringsAsFactors = FALSE
  )
}

#' infix helper for NULL
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a
