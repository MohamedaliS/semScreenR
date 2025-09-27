#' Ethical safeguards and warnings for semScreenR
#' @keywords internal
NULL

#' Display ethical warning before first use
#' @export
ethical_reminder <- function() {
  cat("\n")
  cat("════════════════════════════════════════════════════════════════════════════════\n")
  cat("                           ETHICAL USE REMINDER                                 \n") 
  cat("════════════════════════════════════════════════════════════════════════════════\n")
  cat("\n")
  cat("semScreenR promotes ethical research through transparent data screening.\n")
  cat("\n")
  cat("BEFORE using this package:\n")
  cat("  ✓ Preregister your screening parameters and rationale\n")
  cat("  ✓ Justify your parameter choices theoretically, not statistically\n")
  cat("  ✓ Plan to report complete screening procedures and results\n")
  cat("\n")
  cat("NEVER:\n")
  cat("  ✗ Adjust parameters based on results\n")
  cat("  ✗ Run multiple configurations to find 'best' outcome\n")
  cat("  ✗ Hide screening procedures in your reporting\n")
  cat("\n")
  cat("For full ethical guidelines: help('ethical_guidelines', package = 'semScreenR')\n")
  cat("To suppress this message: options(semScreenR.ethical_reminder = FALSE)\n")
  cat("\n")
  cat("════════════════════════════════════════════════════════════════════════════════\n")
  cat("\n")
}

#' Check for potential ethical violations
#' @keywords internal
.check_ethical_flags <- function(plan, result) {
  warnings <- character()
  
  # Check for excessive removal
  if (!is.null(result$history)) {
    items_removed <- sum(sapply(result$history, function(x) x$type == "item_drop" && x$kept))
    total_items <- length(extract_model_variables(plan$model))
    removal_rate <- items_removed / max(1, total_items)
    
    if (removal_rate > 0.20) {
      warnings <- c(warnings, sprintf(
        "WARNING: %.1f%% of items removed. Consider if this level of screening is scientifically justified.",
        100 * removal_rate
      ))
    }
  }
  
  # Check for very liberal settings
  if (plan$config$meta$preset == "aggressive" && plan$config$limits$max_item_drop_frac > 0.15) {
    warnings <- c(warnings, 
      "WARNING: Very liberal screening settings detected. Ensure strong theoretical justification.")
  }
  
  # Check for disabled validation
  if (!isTRUE(plan$config$gates$use_validation)) {
    warnings <- c(warnings, 
      "WARNING: K-fold validation disabled. Results may not generalize well.")
  }
  
  if (length(warnings) > 0) {
    cat("\n⚠️  ETHICAL CONCERNS DETECTED:\n")
    for (w in warnings) {
      cat("   ", w, "\n")
    }
    cat("\n")
  }
}

#' Generate ethical reporting checklist
#' @param result semScreen_result object
#' @export
ethical_checklist <- function(result) {
  cat("\n")
  cat("══════════════════════════════════════════════════════════════════\n")
  cat("                    ETHICAL REPORTING CHECKLIST                   \n")
  cat("══════════════════════════════════════════════════════════════════\n")
  cat("\n")
  cat("For transparent reporting, ensure you include:\n")
  cat("\n")
  cat("□ PREREGISTRATION\n")
  cat("  □ Link to preregistered screening protocol\n")
  cat("  □ Any deviations from preregistered plan (with justification)\n")
  cat("\n")
  cat("□ METHODS SECTION\n")
  cat("  □ semScreenR version number:", utils::packageVersion("semScreenR"), "\n")
  cat("  □ Preset used and justification\n")
  cat("  □ Any custom parameters and rationale\n")
  cat("  □ Protected indicators and reasons\n")
  cat("\n")
  cat("□ RESULTS SECTION\n")
  if (!is.null(result)) {
    total_removed <- length(result$history)
    cat("  □ Number of screening actions taken:", total_removed, "\n")
    cat("  □ Sample size change:", nrow(result$data_final), "(report original N too)\n")
    if (total_removed > 0) {
      cat("  □ Pre-screening fit measures\n")
      cat("  □ Post-screening fit measures\n")
      cat("  □ Specific items/cases removed (see action log below)\n")
    }
  }
  cat("  □ Sensitivity analyses with alternative parameters\n")
  cat("\n")
  cat("□ SUPPLEMENTARY MATERIALS\n")
  cat("  □ Complete semScreenR action log\n")
  cat("  □ Code used for screening\n")
  cat("  □ Rationale for all screening decisions\n")
  cat("\n")
  cat("□ DISCUSSION\n") 
  cat("  □ Limitations introduced by screening\n")
  cat("  □ Impact of screening on interpretations\n")
  cat("  □ Comparison with unscreened results (if meaningful)\n")
  cat("\n")
  
  if (!is.null(result) && length(result$history) > 0) {
    cat("ACTION LOG FOR REPORTING:\n")
    cat("----------------------------------------\n")
    for (i in seq_along(result$history)) {
      action <- result$history[[i]]
      cat(sprintf("Step %d: %s", action$step, action$type))
      if (!is.null(action$item)) cat(" (", action$item, ")")
      if (!is.null(action$kept)) cat(ifelse(action$kept, " [ACCEPTED]", " [REJECTED]"))
      cat("\n")
      if (!is.null(action$note)) cat("  Reason:", action$note, "\n")
    }
  }
  
  cat("\n")
  cat("══════════════════════════════════════════════════════════════════\n")
  cat("Remember: Complete transparency serves scientific integrity.\n")
  cat("══════════════════════════════════════════════════════════════════\n")
  cat("\n")
}

# Show ethical reminder on package load
.onAttach <- function(libname, pkgname) {
  if (getOption("semScreenR.ethical_reminder", TRUE)) {
    ethical_reminder()
  }
}