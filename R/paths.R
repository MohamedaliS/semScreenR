#' Pre/post SEM paths (prototype)
#' @export
sem_paths_prepost <- function(dat, model, res) {
  if (!requireNamespace("lavaan", quietly = TRUE) || !requireNamespace("semPlot", quietly = TRUE)) {
    message("Install lavaan and semPlot for path diagrams.")
    return(invisible(NULL))
  }
  f0 <- try(lavaan::cfa(model, data = dat, silent = TRUE, warn = FALSE), silent = TRUE)
  f1 <- try(lavaan::cfa(model, data = res$data_final, silent = TRUE, warn = FALSE), silent = TRUE)
  if (!inherits(f0, "try-error")) semPlot::semPaths(f0, whatLabels = "std", title = FALSE)
  if (!inherits(f1, "try-error")) semPlot::semPaths(f1, whatLabels = "std", title = FALSE)
  invisible(NULL)
}
