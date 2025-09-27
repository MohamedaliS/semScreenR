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
  # Enhanced input validation
  if (!is.data.frame(dat)) stop("dat must be a data.frame")
  if (!is.character(model) || length(model) != 1) stop("model must be a character string")
  if (!inherits(plan, "semScreen_plan")) stop("plan must be a semScreen_plan object")
  if (nrow(dat) == 0) stop("dat cannot be empty")
  
  # Check if model variables exist in data
  model_vars <- extract_model_variables(model)
  missing_vars <- setdiff(model_vars, names(dat))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }
  
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
    if (is.null(fit)) return(NULL)
    
    std <- try(lavaan::standardizedSolution(fit), silent = TRUE)
    if (inherits(std, "try-error") || is.null(std) || nrow(std) == 0) return(NULL)
    
    # retain only loadings: op =~
    L <- subset(std, op == "=~")
    if (nrow(L) == 0) return(NULL)
    
    # find lowest loading not in protected
    L$abs_est <- abs(L$est.std)
    L <- L[!(L$rhs %in% protected), ]
    if (nrow(L) == 0) return(NULL)
    
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

  # Helper: K-fold validation of an action (drop one item) - FIXED VERSION
  kfold_validate <- function(data_in, model_in, drop_item, k = 5L, loading_min) {
    n <- nrow(data_in)
    if (k > n) k <- max(2L, floor(n / 20)) # ensure usable folds
    idx <- sample(rep(1:k, length.out = n))
    deltas <- matrix(NA_real_, nrow = k, ncol = 4, dimnames = list(NULL, c("cfi","tli","rmsea","srmr")))
    
    # Create modified model syntax by removing the drop_item - CRITICAL FIX
    model_post <- modify_model_syntax(model_in, drop_item)
    if (is.null(model_post)) {
      warning(sprintf("Cannot create valid model after dropping '%s'", drop_item))
      return(list(ok = FALSE, deltas = deltas))
    }
    
    for (fold in 1:k) {
      train <- data_in[idx != fold, , drop = FALSE]
      valid <- data_in[idx == fold, , drop = FALSE]
      
      # Check if we have enough observations in this fold
      if (nrow(valid) < 10) next  # Skip very small folds
      
      # Pre-fit on validation fold with original model
      f_pre  <- .safe_fit(model_in, valid)
      m_pre  <- .fit_measures(f_pre)
      
      # Post-fit on validation fold with modified model - CRITICAL FIX
      f_post <- .safe_fit(model_post, valid)  # Use modified model, not modified data
      m_post <- .fit_measures(f_post)
      
      # Only compute deltas if both fits succeeded
      if (length(m_pre) > 0 && length(m_post) > 0) {
        deltas[fold, "cfi"]   <- (m_post$cfi   %||% NA) - (m_pre$cfi   %||% NA)
        deltas[fold, "tli"]   <- (m_post$tli   %||% NA) - (m_pre$tli   %||% NA)
        deltas[fold, "rmsea"] <- (m_post$rmsea %||% NA) - (m_pre$rmsea %||% NA)
        deltas[fold, "srmr"]  <- (m_post$srmr  %||% NA) - (m_pre$srmr  %||% NA)
      }
    }
    
    # majority rule: cfi/tli non-negative and rmsea/srmr non-positive in most folds
    valid_folds <- rowSums(!is.na(deltas)) > 0
    if (sum(valid_folds) < ceiling(k/3)) {
      # Too few valid folds to make a reliable decision
      return(list(ok = FALSE, deltas = deltas))
    }
    
    ok_cfi   <- sum(deltas[, "cfi"]   >= 0, na.rm = TRUE) >= ceiling(sum(valid_folds)/2)
    ok_tli   <- sum(deltas[, "tli"]   >= 0, na.rm = TRUE) >= ceiling(sum(valid_folds)/2)
    ok_rmsea <- sum(deltas[, "rmsea"] <= 0, na.rm = TRUE) >= ceiling(sum(valid_folds)/2)
    ok_srmr  <- sum(deltas[, "srmr"]  <= 0, na.rm = TRUE) >= ceiling(sum(valid_folds)/2)
    
    list(ok = all(c(ok_cfi, ok_tli, ok_rmsea, ok_srmr)), deltas = deltas)
  }

  # 3) Iterative item triage
  iter <- 0L
  removed_items <- character()
  current_model <- model  # Track the evolving model syntax
  
  while (iter < cfg$limits$max_iterations) {
    iter <- iter + 1L

    # propose one item based on current model state
    prop <- propose_item_drop(
      fit0,
      protected = plan$protected,
      loading_min = cfg$thresholds$loading_min,
      min_items_per_factor = cfg$limits$min_items_per_factor
    )
    if (is.null(prop)) break

    candidate_item <- as.character(prop$rhs[1])

    # Create modified model syntax for post-fit - CRITICAL FIX
    model_prop <- modify_model_syntax(current_model, candidate_item)
    if (is.null(model_prop)) {
      history[[length(history)+1]] <- list(
        step = length(history)+1,
        type = "item_drop_rejected",
        item = candidate_item,
        kept = FALSE,
        note = sprintf("Cannot drop '%s' - would leave factor with too few indicators", candidate_item)
      )
      break
    }

    # Post-fit with modified model syntax
    fit_prop <- .safe_fit(model_prop, dat_work)
    if (is.null(fit_prop)) {
      history[[length(history)+1]] <- list(
        step = length(history)+1,
        type = "item_drop_rejected",
        item = candidate_item,
        kept = FALSE,
        note = sprintf("Item '%s' rejected - model fitting failed", candidate_item)
      )
      break
    }

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

    if (!improve) {
      history[[length(history)+1]] <- list(
        step = length(history)+1,
        type = "item_drop_rejected",
        item = candidate_item,
        kept = FALSE,
        note = sprintf("Item '%s' rejected - insufficient improvement", candidate_item)
      )
      break
    }

    # Optional k-fold validation
    ok_val <- TRUE
    val_details <- NULL
    if (isTRUE(cfg$gates$use_validation)) {
      v <- kfold_validate(dat_work, current_model, candidate_item, k = cfg$gates$validation_k,
                          loading_min = cfg$thresholds$loading_min)
      ok_val <- isTRUE(v$ok)
      val_details <- v$deltas
      
      if (!ok_val) {
        history[[length(history)+1]] <- list(
          step = length(history)+1,
          type = "item_drop_rejected",
          item = candidate_item,
          kept = FALSE,
          note = sprintf("Item '%s' rejected - failed k-fold validation", candidate_item)
        )
        break
      }
    }

    # Accept the removal
    current_model <- model_prop  # Update working model syntax
    fit0 <- fit_prop             # Update current fit
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
    original_items <- extract_model_variables(plan$model)
    total_original_items <- length(original_items)
    if (length(removed_items) / max(1, total_original_items) > cfg$limits$max_item_drop_frac) {
      history[[length(history)+1]] <- list(
        step = length(history)+1,
        type = "cap_hit",
        kept = FALSE,
        note = "Exceeded max_item_drop_frac; stopping."
      )
      break
    }
  }

  # 4) Prepare final result
  final_status <- if (length(history) > 0 && any(sapply(history, function(x) x$kept %||% FALSE))) {
    "completed_with_changes"
  } else {
    status
  }
  
  # Create preliminary result for ethical checks
  preliminary_result <- structure(
    list(
      status = final_status,
      data_final = dat_work,
      history = history,
      fit = list(pre = .fit_measures(.safe_fit(plan$model, dat)), post = pre)
    ),
    class = "semScreen_result"
  )

  # Ethical safeguards check
  if (exists(".check_ethical_flags")) {
    .check_ethical_flags(plan, preliminary_result)
  }
  
  # Return final result
  preliminary_result
}