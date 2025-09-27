#' Export a compact HTML report with pre/post fit and action log
#'
#' If gt is available, includes an APA-like fit table. Otherwise plain HTML.
#'
#' @param dat data
#' @param model model syntax
#' @param res result from triage_apply
#' @param file output .html file path
#' @return normalized file path (invisible)
#' @export
export_sem_report <- function(dat, model, res, file = "sem_report.html") {
  pre  <- res$fit$pre  %||% list()
  post <- res$fit$post %||% list()

  table_html <- ""
  tb <- apa_fit_table(pre, post)
  if (.pkg_available("gt") && inherits(tb, "gt_tbl")) {
    table_html <- gt::as_raw_html(tb)
  } else {
    # simple fallback
    df <- as.data.frame(tb)
    rows <- apply(df, 1, function(r) paste0("<tr>", paste0(sprintf("<td>%s</td>", r), collapse=""), "</tr>"))
    header <- paste0("<tr>", paste0(sprintf("<th>%s</th>", names(df)), collapse=""), "</tr>")
    table_html <- paste0("<table border='1'>", header, paste0(rows, collapse=""), "</table>")
  }

  # Action log as text
  hist_txt <- paste(capture.output(str(res$history)), collapse = "\n")

  html <- c(
    "<html><head><meta charset='utf-8'><title>semScreenR Report</title></head><body>",
    "<h1>semScreenR Report</h1>",
    sprintf("<p><b>Status:</b> %s</p>", res$status),
    "<h2>Pre vs Post Fit</h2>",
    table_html,
    "<h2>Action Log</h2>",
    "<pre>", hist_txt, "</pre>",
    "</body></html>"
  )

  writeLines(html, file)
  invisible(normalizePath(file))
}
