#' Apply a triage plan (prototype)
#' @param dat data.frame
#' @param model lavaan model syntax
#' @param plan object from triage_plan()
#' @param policy strategy (unused placeholder)
#' @param config optional config override
#' @return A semScreen_result with status, final data, history, and pre/post fit.
#' @export
triage_apply <- function(dat, model, plan, policy = "one_by_one", config = NULL) {
  cfg <- if (is.null(config)) plan$config else config
  history <- list()
  status <- "completed_no_changes"
  dat_final <- dat
  # Honor careless extra proposals under caps
  if (!is.null(plan$extra_rows) && nrow(plan$extra_rows) > 0) {
    cap_cr <- floor(nrow(dat) * cfg$careless$max_row_drop_frac_cr)
    keep_n <- min(nrow(plan$extra_rows), cap_cr)
    drop_idx <- head(plan$extra_rows$id, keep_n)
    dat_final <- dat_final[-unique(drop_idx), , drop = FALSE]
    history[[length(history)+1]] <- list(step = 1, type = "row_drop_careless",
                                         rows = drop_idx, kept = TRUE, note = "careless")
    status <- "careless_applied"
  }
  # Enforce global caps
  removed_frac <- 1 - (nrow(dat_final) / nrow(dat))
  if (removed_frac > cfg$limits$max_row_drop_frac) {
    status <- "exceeded_row_drop_cap"
    dat_final <- dat
    history[[length(history)+1]] <- list(step = NA, type = "cap_hit", rows = integer(0), kept = FALSE,
                                         note = "exceeded_row_drop_cap")
  }
  # Minimal fit extraction if lavaan available
  fit_pre <- fit_post <- NULL
  if (requireNamespace("lavaan", quietly = TRUE)) {
    fit0 <- try(lavaan::cfa(model, data = dat, silent = TRUE, warn = FALSE), silent = TRUE)
    fit1 <- try(lavaan::cfa(model, data = dat_final, silent = TRUE, warn = FALSE), silent = TRUE)
    get_meas <- function(f) {
      if (inherits(f, "try-error")) return(list())
      fm <- try(lavaan::fitMeasures(f, c("cfi","tli","rmsea","srmr","aic","bic")), silent = TRUE)
      if (inherits(fm, "try-error")) return(list())
      as.list(fm)
    }
    fit_pre  <- get_meas(fit0)
    fit_post <- get_meas(fit1)
  }
  out <- list(
    status = status,
    data_final = dat_final,
    history = history,
    fit = list(pre = fit_pre, post = fit_post)
  )
  class(out) <- "semScreen_result"
  out
}
