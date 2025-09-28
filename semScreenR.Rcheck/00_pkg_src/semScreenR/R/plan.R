#' Create a triage plan
#'
#' Holds the model, IDs, protected indicators, config, and any extra row proposals.
#'
#' @param dat data.frame
#' @param model lavaan model syntax
#' @param id_cols optional character vector of ID column names
#' @param protected character vector of indicators never to remove
#' @param config output from triage_rules()
#' @param extra_row_proposals optional data.frame with columns id, reason
#' @return object of class 'semScreen_plan'
#' @export
triage_plan <- function(
  dat, model, id_cols = NULL, protected = character(), config,
  extra_row_proposals = NULL
) {
  plan <- list(
    ids = id_cols,
    protected = protected,
    model = model,
    extra_rows = extra_row_proposals,
    config = config
  )
  class(plan) <- "semScreen_plan"
  plan
}
