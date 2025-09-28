#' Compute multiple careless-responding signals
#'
#' Flags rows using four common indicators:
#' longest identical string (longstring), in-row variance (IRV),
#' Mahalanobis distance on standardized items, and too-fast response time.
#'
#' @param df data.frame
#' @param items character vector of item column names
#' @param rt_col optional response-time column
#' @param longstring_k threshold for longest identical run
#' @param irv_min minimum within-person SD
#' @param mahal_p p-value cutoff for Mahalanobis flag
#' @param fast_q quantile threshold for too-fast responses
#' @return data.frame with one row per case and logical flags
#' @export
careless_signals <- function(
  df, items, rt_col = NULL,
  longstring_k = 10, irv_min = 0.15, mahal_p = 0.001, fast_q = 0.02
) {
  stopifnot(length(items) > 0)
  stopifnot(all(items %in% names(df)))
  # Standardize items
  Z <- scale(df[, items, drop = FALSE])

  # Longest identical run per row
  longest_run <- function(v) {
    r <- rle(as.numeric(v))
    if (length(r$lengths) == 0) return(0L)
    max(r$lengths)
  }
  ls_vec <- apply(df[, items, drop = FALSE], 1, longest_run)

  # In-row variance
  irv_vec <- apply(df[, items, drop = FALSE], 1, function(v) stats::sd(v, na.rm = TRUE))

  # Mahalanobis distance on Z
  S <- try(stats::cov(Z, use = "pairwise.complete.obs"), silent = TRUE)
  inv <- try(solve(S), silent = TRUE)
  md  <- if (inherits(inv, "try-error")) rep(NA_real_, nrow(Z)) else rowSums((Z %*% inv) * Z)
  p_md <- stats::pchisq(md, df = ncol(Z), lower.tail = FALSE)

  # Too fast
  fast_flag <- rep(FALSE, nrow(df))
  if (!is.null(rt_col) && rt_col %in% names(df)) {
    thr <- stats::quantile(df[[rt_col]], probs = fast_q, na.rm = TRUE)
    fast_flag <- df[[rt_col]] <= thr
  }

  data.frame(
    row_id = seq_len(nrow(df)),
    longstring = ls_vec >= longstring_k,
    low_irv = irv_vec <= irv_min,
    mahal = p_md <= mahal_p,
    too_fast = fast_flag,
    stringsAsFactors = FALSE
  )
}
