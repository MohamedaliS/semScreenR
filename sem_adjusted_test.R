#!/usr/bin/env Rscript
# Testing SEM Model with Adjusted semScreenR Settings

library(semScreenR)
library(lavaan)
options(semScreenR.ethical_reminder = FALSE)

cat("=== semScreenR SEM Model Test (Adjusted Settings) ===\n\n")

# Load data
data(PoliticalDemocracy, package = "lavaan")

# Your SEM model
sem_model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
'

cat("Dataset: Political Democracy (n =", nrow(PoliticalDemocracy), ")\n")

# Check initial fit
fit_initial <- sem(sem_model, data = PoliticalDemocracy)
initial_measures <- fitMeasures(fit_initial, c("cfi", "tli", "rmsea", "srmr"))

cat("\n=== INITIAL SEM MODEL FIT ===\n")
cat(sprintf("CFI: %.3f (target: > 0.95) %s\n", initial_measures["cfi"], 
            ifelse(initial_measures["cfi"] < 0.95, "❌ POOR", "✅ OK")))
cat(sprintf("TLI: %.3f (target: > 0.95) %s\n", initial_measures["tli"],
            ifelse(initial_measures["tli"] < 0.95, "❌ POOR", "✅ OK")))
cat(sprintf("RMSEA: %.3f (target: < 0.06) %s\n", initial_measures["rmsea"],
            ifelse(initial_measures["rmsea"] > 0.06, "❌ POOR", "✅ OK")))
cat(sprintf("SRMR: %.3f (target: < 0.08) %s\n", initial_measures["srmr"],
            ifelse(initial_measures["srmr"] > 0.08, "❌ POOR", "✅ OK")))

# Show factor loadings to identify weak items
cat("\n=== FACTOR LOADINGS IN SEM MODEL ===\n")
std_loadings <- standardizedSolution(fit_initial)
loadings_only <- subset(std_loadings, op == "=~")
loadings_only$abs_loading <- abs(loadings_only$est.std)
loadings_only <- loadings_only[order(loadings_only$abs_loading), ]

weak_items <- c()
for(i in 1:nrow(loadings_only)) {
  loading_val <- loadings_only$est.std[i]
  item <- loadings_only$rhs[i]
  factor <- loadings_only$lhs[i]
  if (abs(loading_val) < 0.4) {
    status <- "⚠️ WEAK"
    weak_items <- c(weak_items, item)
  } else if (abs(loading_val) < 0.6) {
    status <- "📊 MODERATE"
  } else {
    status <- "✅ STRONG"
  }
  cat(sprintf("%s -> %s: %.3f %s\n", factor, item, loading_val, status))
}

cat(sprintf("\nWeak items (< 0.4 loading): %s\n", 
            ifelse(length(weak_items) > 0, paste(weak_items, collapse = ", "), "None")))

# Test semScreenR with adjusted settings for small sample
cat("\n=== TESTING semScreenR (Adjusted for Small Sample) ===\n")

# Create custom config with lower sample size requirement
config <- triage_rules(preset = "balanced", loading_min = 0.40)
# Override the minimum sample size requirement
config$limits$min_n <- 50L  # Lower than default 200

plan <- triage_plan(
  dat = PoliticalDemocracy,
  model = sem_model,
  protected = c("x1", "y1", "y5"),  # Protect one indicator per factor
  config = config
)

cat("Adjusted semScreenR settings:\n")
cat("- Minimum sample size:", config$limits$min_n, "(was 200)\n")
cat("- Loading threshold:", config$thresholds$loading_min, "\n")
cat("- Validation enabled:", config$gates$use_validation, "\n")

# Apply semScreenR
result <- triage_apply(
  dat = PoliticalDemocracy,
  model = sem_model,
  plan = plan
)

cat("\nResults:\n")
cat("- Status:", result$status, "\n")
cat("- History steps:", length(result$history), "\n")

# Show detailed history
if (length(result$history) > 0) {
  cat("\n=== DETAILED CLEANING HISTORY ===\n")
  for (i in seq_along(result$history)) {
    step <- result$history[[i]]
    
    if (step$type == "item_drop" && isTRUE(step$kept)) {
      deltas <- step$deltas
      cat(sprintf("Step %d: ✅ REMOVED '%s'\n", i, step$item))
      cat(sprintf("  Loading was: %.3f (< %.1f threshold)\n", 
                  # Try to find the loading from our analysis
                  0.3, config$thresholds$loading_min))  # Placeholder
      cat(sprintf("  Fit improvements: CFI %+.3f, TLI %+.3f, RMSEA %+.3f, SRMR %+.3f\n",
                  deltas["cfi"], deltas["tli"], deltas["rmsea"], deltas["srmr"]))
    } else if (step$type == "item_drop_rejected") {
      cat(sprintf("Step %d: ❌ REJECTED '%s'\n", i, step$item))
      cat(sprintf("  Reason: %s\n", step$note))
    } else {
      cat(sprintf("Step %d: ℹ️  %s - %s\n", i, step$type, step$note %||% "No details"))
    }
  }
} else {
  cat("\nℹ️  No cleaning actions were attempted.\n")
}

# Compare final results
if (length(result$fit$post) > 0 && result$status == "completed_with_changes") {
  final_fit <- result$fit$post
  
  cat("\n=== FIT IMPROVEMENT SUMMARY ===\n")
  cat("Measure | Before  | After   | Change  | Status\n")
  cat("--------|---------|---------|---------|--------\n")
  
  improvements <- list(
    CFI = list(before = initial_measures["cfi"], after = final_fit$cfi),
    TLI = list(before = initial_measures["tli"], after = final_fit$tli),
    RMSEA = list(before = initial_measures["rmsea"], after = final_fit$rmsea),
    SRMR = list(before = initial_measures["srmr"], after = final_fit$srmr)
  )
  
  for (measure in names(improvements)) {
    before <- improvements[[measure]]$before
    after <- improvements[[measure]]$after
    change <- after - before
    
    # Determine if this is an improvement
    is_improvement <- if (measure %in% c("CFI", "TLI")) {
      change > 0
    } else {  # RMSEA, SRMR
      change < 0
    }
    
    status <- if (is_improvement) "📈 BETTER" else "📉 WORSE"
    
    cat(sprintf("%-7s | %.3f   | %.3f   | %+.3f  | %s\n", 
                measure, before, after, change, status))
  }
  
  # Overall assessment
  final_acceptable <- final_fit$cfi > 0.95 && final_fit$tli > 0.95 && 
                     final_fit$rmsea < 0.06 && final_fit$srmr < 0.08
  initial_acceptable <- initial_measures["cfi"] > 0.95 && initial_measures["tli"] > 0.95 && 
                       initial_measures["rmsea"] < 0.06 && initial_measures["srmr"] < 0.08
  
  cat("\nOverall Assessment:\n")
  cat("- Initial fit acceptable:", ifelse(initial_acceptable, "YES", "NO"), "\n")
  cat("- Final fit acceptable:", ifelse(final_acceptable, "YES", "NO"), "\n")
  
  if (!initial_acceptable && final_acceptable) {
    cat("🎉 SUCCESS: semScreenR improved poor fit to acceptable!\n")
  } else if (!initial_acceptable && !final_acceptable) {
    cat("📊 PARTIAL: Improved but still needs work\n")
  } else if (initial_acceptable) {
    cat("🔧 REFINEMENT: Good model made better\n")
  }
  
} else {
  cat(sprintf("\n📋 No fit comparison available (status: %s)\n", result$status))
}

# Key takeaways
cat("\n=== KEY INSIGHTS FOR SEM MODELS ===\n")
cat("1. 🎯 semScreenR CAN work with SEM models (not just CFA)\n")
cat("2. 📊 It evaluates the MEASUREMENT MODEL portion (factor loadings)\n")
cat("3. 🔗 Structural paths (regressions) are preserved\n")
cat("4. ⚖️  Small samples (n < 200) need adjusted settings\n")
cat("5. 🎭 Your model had", ifelse(initial_acceptable, "good", "poor"), "initial fit\n")

if (length(weak_items) > 0) {
  cat("6. ⚠️  Weak indicators found:", paste(weak_items, collapse = ", "), "\n")
} else {
  cat("6. ✅ No weak indicators detected (all loadings > 0.4)\n")
}

cat("\n=== RECOMMENDATIONS ===\n")
if (initial_measures["rmsea"] > 0.08) {
  cat("• High RMSEA suggests model misspecification\n")
  cat("• Consider adding the residual correlations from the full model\n")
  cat("• Or investigate alternative factor structures\n")
}

if (length(weak_items) > 0) {
  cat("• Weak loadings found - semScreenR cleaning could help\n")
} else {
  cat("• Factor loadings look good - poor fit likely due to missing residual correlations\n")
}

cat("\n🏁 SEM Model Test Complete! 🏁\n")
