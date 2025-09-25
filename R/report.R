#' Export a brief report (prototype)
#' @param dat data
#' @param model model
#' @param res result from triage_apply
#' @param file output path (.html recommended)
#' @return Path to the report file
#' @export
export_sem_report <- function(dat, model, res, file = "sem_report.html") {
  lines <- c(
    "<html><head><meta charset='utf-8'><title>semScreenR Report</title></head><body>",
    "<h1>semScreenR Report</h1>",
    sprintf("<p>Status: <b>%s</b></p>", res$status),
    "<h2>Fit (pre)</h2>",
    paste0("<pre>", capture.output(str(res$fit$pre)), "</pre>"),
    "<h2>Fit (post)</h2>",
    paste0("<pre>", capture.output(str(res$fit$post)), "</pre>"),
    "<h2>History</h2>",
    paste0("<pre>", capture.output(str(res$history)), "</pre>"),
    "<p><i>Prototype report. For full APA tables and diagrams, integrate with gt and semPlot.</i></p>",
    "</body></html>"
  )
  writeLines(lines, con = file)
  invisible(normalizePath(file))
}
