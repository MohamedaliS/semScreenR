#' Apply a triage plan
#'
#' Steps
#' 1) Optional careless-responding removals under a strict cap.
#' 2) Initial CFA fit.
#' 3) Iterative item triage: remove at most one non-protected indicator per step
#'    if standardized loading < loading_min AND global fit improves by min_delta.
#' 4) K-fold validation: accept only if validation folds do not show degradation.
#' 5) Enforce global caps and safeguards; keep a full action log.
#'
#' @param dat data.frame
#' @param model lavaan syntax
#' @param plan semScreen_plan
#' @param policy reserved for future strategies
#' @param config optional override for plan$config
#' @return semScreen_result list with fields: status, data_final, history, fit
#' @export
triage_apply <- function(dat, model, plan, policy = "one_by_one", config = NULL) {
  cfg <- if (is.null(config)) plan$config else config
  history <- list()
  status <- "completed_no_changes"
  dat_work <- dat

  # 0) Carless-responding suggested removals (user-supplied through plan$extra_rows)
  if (!is.null(plan$extra_rows) && nrow(plan$extra_rows) > 0) {
    cap_cr <- floor(nrow(dat_work) * cfg$careless$max_row_drop_frac_cr)
    keep_n <- min(nrow(plan$extra_rows), cap_cr)
    drop_idx <- head(plan$extra_rows$id, keep_n)
    # assume index rows; if 'ids' maps, user should pre-map to row indices
    drop_idx <- intersect(drop_idx, seq_len(nrow(dat_work)))
    if (length(drop_idx)) {
      dat_work <- dat_work[-unique(drop_idx), , drop = FALSE]
      history[[length(history)+1]] <- list(
        step = length(history)+1,
        type = "row_drop_careless",
        rows = drop_idx,
        kept = TRUE,
        note = sprintf("Dropped %d rows on careless signals (cap %.0f%%).",
                       length(drop_idx), 100*cfg$careless$max_row_drop_frac_cr)
      )
      status <- "careless_applied"
    }
  }

  # 1) Enforce minimum N
  if (nrow(dat_work) < cfg$limits$min_n) {
    status <- sprintf("insufficient_n_min_required_%d", cfg$limits$min_n)
    prefit <- .fit_measures(.safe_fit(model, dat))
    return(structure(list(status = status, data_final = dat,
                          history = history, fit = list(pre = prefit, post = list())),
                     class = "semScreen_result"))
  }

  # 2) Initial fit
  fit0 <- .safe_fit(model, dat_work)
  pre  <- .fit_measures(fit0)

  # Early exit if lavaan unavailable
  if (is.null(fit0)) {
    return(structure(list(status = "lavaan_missing", data_final = dat_work,
                          history = history, fit = list(pre = list(), post = list())),
                     class = "semScreen_result"))
  }

  # Helper: propose an item to remove based on standardized loadings
  propose_item_drop <- function(fit, protected, loading_min, min_items_per_factor) {
    std <- try(lavaan::standardizedSolution(fit), silent = TRUE)
    if (inherits(std, "try-error")) return(NULL)
    # retain only loadings: op =~
    L <- subset(std, op == "=~")
    if (nrow(L) == 0) return(NULL)
    # find lowest loading not in protected
    L$abs_est <- abs(L$est.std)
    L <- L[!(L$rhs %in% protected), ]
    # ensure each factor keeps minimum indicators after a potential drop
    ok <- logical(nrow(L))
    for (i in seq_len(nrow(L))) {
      fac <- L$lhs[i]
      # how many current indicators for this factor?
      n_now <- sum(std$op == "=~" & std$lhs == fac)
      ok[i] <- n_now - 1 >= min_items_per_factor
    }
    L <- L[ok, ]
    if (nrow(L) == 0) return(NULL)
    # candidate is the smallest loading below threshold
    L <- L[order(L$abs_est, decreasing = FALSE), , drop = FALSE]
    cand <- subset(L, abs_est < loading_min)
    if (nrow(cand) == 0) return(NULL)
    cand[1, c("lhs","rhs","est.std")]
  }

  # Helper: K-fold validation of an action (drop one item)
  kfold_validate <- function(data_in, model_in, drop_item, k = 5L, loading_min) {
    n <- nrow(data_in)
    if (k > n) k <- max(2L, floor(n / 20)) # ensure usable folds
    idx <- sample(rep(1:k, length.out = n))
    deltas <- matrix(NA_real_, nrow = k, ncol = 4, dimnames = list(NULL, c("cfi","tli","rmsea","srmr")))
    for (fold in 1:k) {
      train <- data_in[idx != fold, , drop = FALSE]
      valid <- data_in[idx == fold, , drop = FALSE]
      # pre on valid
      f_pre  <- .safe_fit(model_in, valid)
      m_pre  <- .fit_measures(f_pre)
      # post: remove the item column from both train and valid
      # but we keep model syntax intact; lavaan will drop if var missing
      cols_ok <- setdiff(names(valid), drop_item)
      valid2  <- valid[, cols_ok, drop = FALSE]
      f_post <- .safe_fit(model_in, valid2)
      m_post <- .fit_measures(f_post)
      deltas[fold, "cfi"]   <- (m_post$cfi   %||% NA) - (m_pre$cfi   %||% NA)
      deltas[fold, "tli"]   <- (m_post$tli   %||% NA) - (m_pre$tli   %||% NA)
      deltas[fold, "rmsea"] <- (m_post$rmsea %||% NA) - (m_pre$rmsea %||% NA)
      deltas[fold, "srmr"]  <- (m_post$srmr  %||% NA) - (m_pre$srmr  %||% NA)
    }
    # majority rule: cfi/tli non-negative and rmsea/srmr non-positive in most folds
    ok_cfi   <- sum(deltas[, "cfi"]   >= 0, na.rm = TRUE) >= ceiling(k/2)
    ok_tli   <- sum(deltas[, "tli"]   >= 0, na.rm = TRUE) >= ceiling(k/2)
    ok_rmsea <- sum(deltas[, "rmsea"] <= 0, na.rm = TRUE) >= ceiling(k/2)
    ok_srmr  <- sum(deltas[, "srmr"]  <= 0, na.rm = TRUE) >= ceiling(k/2)
    list(ok = all(c(ok_cfi, ok_tli, ok_rmsea, ok_srmr)),
         deltas = deltas)
  }

  # 3) Iterative item triage
  iter <- 0L
  removed_items <- character()
  while (iter < cfg$limits$max_iterations) {
    iter <- iter + 1L

    # propose one item
    prop <- propose_item_drop(
      fit0,
      protected = plan$protected,
      loading_min = cfg$thresholds$loading_min,
      min_items_per_factor = cfg$limits$min_items_per_factor
    )
    if (is.null(prop)) break

    candidate_item <- as.character(prop$rhs[1])

    # Post-fit if we remove candidate column from data
    if (!(candidate_item %in% names(dat_work))) break
    dat_prop <- dat_work[, setdiff(names(dat_work), candidate_item), drop = FALSE]
    fit_prop <- .safe_fit(model, dat_prop)
    if (is.null(fit_prop)) break

    post <- .fit_measures(fit_prop)

    # Check improvement gates
    deltas <- c(
      cfi   = (post$cfi   %||% NA) - (pre$cfi   %||% NA),
      tli   = (post$tli   %||% NA) - (pre$tli   %||% NA),
      rmsea = (post$rmsea %||% NA) - (pre$rmsea %||% NA),
      srmr  = (post$srmr  %||% NA) - (pre$srmr  %||% NA)
    )
    gate <- cfg$gates$min_delta
    improve <- (is.na(deltas["cfi"])   || deltas["cfi"]   >= gate$CFI)  &&
               (is.na(deltas["tli"])   || deltas["tli"]   >= gate$TLI)  &&
               (is.na(deltas["rmsea"]) || deltas["rmsea"] <= gate$RMSEA) &&
               (is.na(deltas["srmr"])  || deltas["srmr"]  <= gate$SRMR)

    if (!improve) break

    # Optional k-fold validation
    ok_val <- TRUE
    val_details <- NULL
    if (isTRUE(cfg$gates$use_validation)) {
      v <- kfold_validate(dat_work, model, candidate_item, k = cfg$gates$validation_k,
                          loading_min = cfg$thresholds$loading_min)
      ok_val <- isTRUE(v$ok)
      val_details <- v$deltas
    }
    if (!ok_val) break

    # Accept the removal
    dat_work <- dat_prop
    fit0 <- fit_prop
    history[[length(history)+1]] <- list(
      step = length(history)+1,
      type = "item_drop",
      item = candidate_item,
      kept = TRUE,
      deltas = deltas,
      note = sprintf("Removed item '%s' due to low loading; %s",
                     candidate_item, one_line_delta(pre, post))
    )
    removed_items <- c(removed_items, candidate_item)
    pre <- post

    # enforce global item drop cap
    total_items <- sum(lavaan::parameterTable(.safe_fit(model, dat))$op == "=~")
    if (length(removed_items) / max(1, total_items + length(removed_items)) > cfg$limits$max_item_drop_frac) {
      history[[length(history)+1]] <- list(
        step = length(history)+1,
        type = "cap_hit",
        kept = FALSE,
        note = "Exceeded max_item_drop_frac; stopping."
      )
      break
    }
  }

  # 4) Final measures
  post <- pre
  status <- if (length(history)) "completed_with_changes" else status
  structure(
    list(
      status = status,
      data_final = dat_work,
      history = history,
      fit = list(pre = .fit_measures(.safe_fit(model, dat)), post = post)
    ),
    class = "semScreen_result"
  )
}
