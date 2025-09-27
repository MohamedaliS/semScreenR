#' Draw pre and post path diagrams and save to file
#'
#' Uses semPlot if available. Saves as PNG or SVG.
#'
#' @param dat data.frame used
#' @param model lavaan model syntax
#' @param res semScreen_result from triage_apply
#' @param file_base base path without extension
#' @param type "png" or "svg"
#' @return character vector of output file path(s) that were written
#' @export
sem_paths_prepost <- function(dat, model, res, file_base = "sem_paths", type = c("png","svg")) {
  type <- match.arg(type)
  if (!.pkg_available("lavaan") || !.pkg_available("semPlot")) {
    message("Install lavaan and semPlot for path diagrams.")
    return(character())
  }
  f0 <- .safe_fit(model, dat)
  f1 <- .safe_fit(model, res$data_final)

  out <- character()
  if (!is.null(f0)) {
    f <- paste0(file_base, "_pre.", type)
    with_device(f, type = type, code = {
      semPlot::semPaths(f0, whatLabels = "std", title = FALSE)
    })
    out <- c(out, f)
  }
  if (!is.null(f1)) {
    f <- paste0(file_base, "_post.", type)
    with_device(f, type = type, code = {
      semPlot::semPaths(f1, whatLabels = "std", title = FALSE)
    })
    out <- c(out, f)
  }
  out
}
