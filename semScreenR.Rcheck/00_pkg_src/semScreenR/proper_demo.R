#!/usr/bin/env Rscript
# Proper semScreenR Demo: Realistic Data Cleaning (No Overfitting)

library(semScreenR)
library(lavaan)
library(MASS)
options(semScreenR.ethical_reminder = FALSE)

cat("=== semScreenR PROPER Demonstration ===\n")
cat("Showing realistic cleaning without overfitting...\n\n")

set.seed(789)

# Create data with moderate problems (not extreme)
create_realistic_data <- function(n = 400) {  # Larger sample
  # Generate realistic factor structure
  factor_cors <- matrix(c(
    1.0, 0.5, 0.4,
    0.5, 1.0, 0.6,
    0.4, 0.6, 1.0
  ), nrow = 3)
  
  factors <- mvrnorm(n, mu = c(0, 0, 0), Sigma = factor_cors)
  data <- data.frame(matrix(NA, nrow = n, ncol = 15))
  colnames(data) <- c(paste0("anx", 1:5), paste0("dep", 1:5), paste0("str", 1:5))
  
  # Realistic loadings with some weak items
  loadings <- list(
    anxiety = c(0.75, 0.70, 0.35, 0.30, 0.65),      # anx3, anx4 moderately weak
    depression = c(0.80, 0.25, 0.70, 0.65, 0.60),   # dep2 weak
    stress = c(0.75, 0.70, 0.65, 0.28, 0.62)        # str4 weak
  )
  
  # Moderate error variance
  error_sd <- 0.6
  
  for (i in 1:5) {
    data[, paste0("anx", i)] <- loadings$anxiety[i] * factors[, 1] + rnorm(n, 0, error_sd)
    data[, paste0("dep", i)] <- loadings$depression[i] * factors[, 2] + rnorm(n, 0, error_sd)
    data[, paste0("str", i)] <- loadings$stress[i] * factors[, 3] + rnorm(n, 0, error_sd)
  }
  
  # Scale to 1-7 Likert
  for (col in 1:ncol(data)) {
    data[, col] <- round(pmax(1, pmin(7, data[, col] + 4)))
  }
  
  as.data.frame(data)
}

# Generate data
realistic_data <- create_realistic_data(400)

model_syntax <- '
  anxiety =~ anx1 + anx2 + anx3 + anx4 + anx5
  depression =~ dep1 + dep2 + dep3 + dep4 + dep5  
  stress =~ str1 + str2 + str3 + str4 + str5
'

# Check initial fit
cat("=== INITIAL MODEL FIT ===\n")
fit_initial <- cfa(model_syntax, data = realistic_data)
initial_measures <- fitMeasures(fit_initial, c("cfi", "tli", "rmsea", "srmr"))

cat(sprintf("CFI: %.3f (target: > 0.95) %s\n", initial_measures["cfi"], 
            ifelse(initial_measures["cfi"] < 0.95, "❌ POOR", "✅ OK")))
cat(sprintf("TLI: %.3f (target: > 0.95) %s\n", initial_measures["tli"],
            ifelse(initial_measures["tli"] < 0.95, "❌ POOR", "✅ OK")))
cat(sprintf("RMSEA: %.3f (target: < 0.06) %s\n", initial_measures["rmsea"],
            ifelse(initial_measures["rmsea"] > 0.06, "❌ POOR", "✅ OK")))
cat(sprintf("SRMR: %.3f (target: < 0.08) %s\n", initial_measures["srmr"],
            ifelse(initial_measures["srmr"] > 0.08, "❌ POOR", "✅ OK")))

# Show loadings
cat("\n=== FACTOR LOADINGS ===\n")
std_loadings <- standardizedSolution(fit_initial)
loadings_df <- subset(std_loadings, op == "=~")[, c("lhs", "rhs", "est.std")]
loadings_df <- loadings_df[order(abs(loadings_df$est.std)), ]

weak_items <- c()
for (i in 1:nrow(loadings_df)) {
  loading <- loadings_df$est.std[i]
  item <- loadings_df$rhs[i]
  factor <- loadings_df$lhs[i]
  if (abs(loading) < 0.4) {
    status <- "⚠️ WEAK"
    weak_items <- c(weak_items, item)
  } else {
    status <- "✅ OK"
  }
  cat(sprintf("%s -> %s: %.3f %s\n", factor, item, loading, status))
}

cat(sprintf("\nWeak items (< 0.4 loading): %s\n", 
            ifelse(length(weak_items) > 0, paste(weak_items, collapse = ", "), "None")))

# Set up PROPER cleaning with realistic parameters
cat("\n=== SETTING UP PROPER CLEANING ===\n")
config <- triage_rules(
  preset = "balanced",           # Use balanced preset
  loading_min = 0.40             # Standard threshold
)

# Keep validation ENABLED and use realistic improvement thresholds
cat("Validation enabled:", config$gates$use_validation, "\n")
cat("Loading threshold:", config$thresholds$loading_min, "\n")
cat("CFI improvement needed:", config$gates$min_delta$CFI, "\n")

plan <- triage_plan(
  dat = realistic_data,
  model = model_syntax, 
  protected = c("anx1", "dep1", "str1"),  # Protect anchor items
  config = config
)

# Apply cleaning with proper safeguards
cat("\n=== APPLYING CLEANING (WITH VALIDATION) ===\n")
result <- triage_apply(dat = realistic_data, model = model_syntax, plan = plan)

cat("Status:", result$status, "\n")
cat("History steps:", length(result$history), "\n\n")

# Show what happened
removed_items <- c()
if (length(result$history) > 0) {
  cat("=== CLEANING ACTIONS ===\n")
  for (i in seq_along(result$history)) {
    step <- result$history[[i]]
    if (step$type == "item_drop" && isTRUE(step$kept)) {
      removed_items <- c(removed_items, step$item)
      deltas <- step$deltas
      cat(sprintf("✅ REMOVED '%s': CFI %+.3f, TLI %+.3f, RMSEA %+.3f, SRMR %+.3f\n", 
                  step$item, deltas["cfi"], deltas["tli"], deltas["rmsea"], deltas["srmr"]))
    } else if (step$type == "item_drop_rejected") {
      cat(sprintf("❌ REJECTED '%s': %s\n", step$item, step$note))
    }
  }
} else {
  cat("ℹ️  No items were removed.\n")
}

# Final comparison
cat("\n=== RESULTS COMPARISON ===\n")
if (length(result$fit$post) > 0) {
  final_measures <- result$fit$post
  
  cat("Measure     | Before  | After   | Change\n")
  cat("------------|---------|---------|--------\n")
  cat(sprintf("CFI         | %.3f   | %.3f   | %+.3f\n", 
              initial_measures["cfi"], final_measures$cfi, 
              final_measures$cfi - initial_measures["cfi"]))
  cat(sprintf("TLI         | %.3f   | %.3f   | %+.3f\n", 
              initial_measures["tli"], final_measures$tli,
              final_measures$tli - initial_measures["tli"]))
  cat(sprintf("RMSEA       | %.3f   | %.3f   | %+.3f\n", 
              initial_measures["rmsea"], final_measures$rmsea,
              final_measures$rmsea - initial_measures["rmsea"]))
  cat(sprintf("SRMR        | %.3f   | %.3f   | %+.3f\n", 
              initial_measures["srmr"], final_measures$srmr,
              final_measures$srmr - initial_measures["srmr"]))
  
  # Check for overfitting signs
  cat("\n=== OVERFITTING CHECK ===\n")
  overfitting_flags <- c()
  
  if (final_measures$cfi > 0.999) {
    overfitting_flags <- c(overfitting_flags, "CFI too perfect (>0.999)")
  }
  if (final_measures$tli > 1.05) {
    overfitting_flags <- c(overfitting_flags, "TLI above reasonable range (>1.05)")
  }
  if (final_measures$rmsea < 0.001) {
    overfitting_flags <- c(overfitting_flags, "RMSEA impossibly low (<0.001)")
  }
  
  if (length(overfitting_flags) > 0) {
    cat("🚨 OVERFITTING DETECTED:\n")
    for (flag in overfitting_flags) {
      cat("  -", flag, "\n")
    }
    cat("This suggests the model was over-cleaned!\n")
  } else {
    cat("✅ No signs of overfitting detected\n")
    cat("Fit indices are in realistic ranges\n")
  }
  
} else {
  cat("No post-cleaning fit measures available\n")
}

# Assessment
cat("\n=== OVERALL ASSESSMENT ===\n")
if (length(removed_items) > 0) {
  cat(sprintf("Items removed: %s\n", paste(removed_items, collapse = ", ")))
  cat("Expected weak items:", paste(weak_items, collapse = ", "), "\n")
  
  correctly_identified <- intersect(removed_items, weak_items)
  cat("Correctly identified:", ifelse(length(correctly_identified) > 0,
                                      paste(correctly_identified, collapse = ", "), "None"), "\n")
} else {
  cat("No items were removed by semScreenR\n")
  if (length(weak_items) > 0) {
    cat("Possible reasons:\n")
    cat("- Validation prevented overfitting\n")  
    cat("- Improvement thresholds not met\n")
    cat("- Cross-validation failed\n")
    cat("This is GOOD - it shows the safeguards are working!\n")
  }
}

cat("\n=== KEY LESSONS ===\n")
cat("1. Perfect fit indices (CFI=1.0, TLI>1.0, RMSEA=0.0) indicate OVERFITTING\n")
cat("2. semScreenR's validation should catch this when enabled\n")  
cat("3. Realistic cleaning produces modest, sustainable improvements\n")
cat("4. Sometimes NO cleaning is the right answer!\n")

cat("\n📊 This demonstrates semScreenR's PROPER behavior with safeguards enabled\n")
