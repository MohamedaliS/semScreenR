#' Create triage rules for semScreenR
#'
#' Defines gates, limits, and careless-responding options. Presets tune how
#' strict the caps are. All thresholds are transparent and reproducible.
#'
#' @param preset One of "conservative","balanced","aggressive".
#' @param loading_min minimum standardized loading to keep an indicator during item triage.
#' @param max_mod_index optional MI threshold (for future use).
#' @return List with components: gates, limits, careless, meta.
#' @export
triage_rules <- function(
  preset = c("balanced","conservative","aggressive"),
  loading_min = 0.40,
  max_mod_index = NULL
) {
  preset <- match.arg(preset)

  gates <- list(
    # minimum improvements required to accept a proposed change
    min_delta = list(CFI = +0.005, TLI = +0.005, RMSEA = -0.005, SRMR = -0.005),
    # validation settings
    use_validation = TRUE, validation_k = 5L,
    # perfect-fit guard (reject if exactly perfect fit is approached artificially)
    perfect_fit_guard = TRUE
  )

  limits <- switch(
    preset,
    conservative = list(
      max_row_drop_frac = 0.05,
      max_item_drop_frac = 0.05,
      min_n = 300L,
      min_items_per_factor = 3L,
      max_iterations = 3L
    ),
    balanced = list(
      max_row_drop_frac = 0.10,
      max_item_drop_frac = 0.10,
      min_n = 200L,
      min_items_per_factor = 3L,
      max_iterations = 5L
    ),
    aggressive = list(
      max_row_drop_frac = 0.15,
      max_item_drop_frac = 0.15,
      min_n = 150L,
      min_items_per_factor = 3L,
      max_iterations = 7L
    )
  )

  careless <- list(
    items = character(),     # optional vector of item columns to scan
    rt_col = NULL,           # optional response-time column
    longstring_k = 10,
    irv_min = 0.15,
    mahal_p = 0.001,
    fast_q = 0.02,
    max_row_drop_frac_cr = 0.05  # cap specifically for careless-based row removal
  )

  list(
    gates = gates,
    limits = limits,
    careless = careless,
    thresholds = list(loading_min = loading_min, max_mod_index = max_mod_index),
    meta = list(preset = preset)
  )
}
