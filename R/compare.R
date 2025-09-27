#' Pre/post comparison table (APA-like)
#' @param dat data used (not shown; kept for API consistency)
#' @param model model string (not shown; kept for API consistency)
#' @param res result from triage_apply()
#' @return gt table if gt is installed, else data.frame
#' @export
sem_compare_table <- function(dat, model, res) {
  pre  <- res$fit$pre  %||% list()
  post <- res$fit$post %||% list()
  apa_fit_table(pre, post)
}
