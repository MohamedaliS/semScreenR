#' Compute careless-responding signals
#' @param df data.frame
#' @param items Character vector of item names.
#' @param rt_col Optional response-time column name.
#' @param longstring_k Threshold for longest identical run.
#' @param irv_min Minimum within-person SD to avoid flag.
#' @param mahal_p P-value cut for Mahalanobis flag on standardized items.
#' @param fast_q Response-time quantile for "too fast".
#' @return A data.frame of row-level flags.
#' @export
careless_signals <- function(df, items, rt_col = NULL,
                             longstring_k = 10,
                             irv_min = 0.15,
                             mahal_p = 0.001,
                             fast_q = 0.02) {
  stopifnot(all(items %in% names(df)))
  Z <- scale(df[, items, drop = FALSE])
  longest_run <- function(v) {
    r <- rle(as.numeric(v))
    if (length(r$lengths) == 0) return(0L)
    max(r$lengths)
  }
  ls_vec <- apply(df[, items, drop = FALSE], 1, longest_run)
  irv_vec <- apply(df[, items, drop = FALSE], 1, function(v) stats::sd(v, na.rm = TRUE))
  S <- try(stats::cov(Z, use = "pairwise.complete.obs"), silent = TRUE)
  inv <- try(solve(S), silent = TRUE)
  md  <- if (inherits(inv, "try-error")) rep(NA_real_, nrow(Z)) else rowSums((Z %*% inv) * Z)
  p_md <- stats::pchisq(md, df = ncol(Z), lower.tail = FALSE)
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
