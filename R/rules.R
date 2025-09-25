#' Create triage rules for semScreenR
#' @param preset Either "conservative","balanced","aggressive".
#' @return A named list with gates, limits, and careless options.
#' @export
triage_rules <- function(preset = c("balanced","conservative","aggressive")) {
  preset <- match.arg(preset)
  gates <- list(
    min_delta = list(CFI = +0.005, TLI = +0.005, RMSEA = -0.005, SRMR = -0.005),
    use_validation = TRUE, validation_k = 5L
  )
  limits <- switch(preset,
    conservative = list(max_row_drop_frac = 0.05, max_item_drop_frac = 0.05,
                        min_n = 300L, min_items_per_factor = 3L, max_iterations = 3L),
    balanced     = list(max_row_drop_frac = 0.10, max_item_drop_frac = 0.10,
                        min_n = 200L, min_items_per_factor = 3L, max_iterations = 5L),
    aggressive   = list(max_row_drop_frac = 0.15, max_item_drop_frac = 0.15,
                        min_n = 150L, min_items_per_factor = 3L, max_iterations = 7L)
  )
  careless <- list(items = character(), rt_col = NULL,
                   longstring_k = 10, irv_min = 0.15, mahal_p = 0.001, fast_q = 0.02,
                   min_indicators = 2L, max_row_drop_frac_cr = 0.05)
  list(gates = gates, limits = limits, careless = careless, meta = list(preset = preset))
}
