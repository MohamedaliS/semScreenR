#' Create a triage plan
#' @param dat data.frame
#' @param model lavaan model syntax
#' @param id_cols Optional ID column(s).
#' @param protected Indicators not to remove.
#' @param config Configuration from triage_rules().
#' @param extra_row_proposals Optional data.frame with columns id, reason.
#' @return A plan object for triage_apply().
#' @export
triage_plan <- function(dat, model, id_cols = NULL, protected = character(), config,
                        extra_row_proposals = NULL) {
  plan <- list(
    ids = id_cols, protected = protected, model = model,
    extra_rows = extra_row_proposals, config = config
  )
  class(plan) <- "semScreen_plan"
  plan
}
