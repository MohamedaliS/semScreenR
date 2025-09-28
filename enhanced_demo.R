#!/usr/bin/env Rscript
# Enhanced Simulation Demo: semScreenR with Truly Poor Fit Data
# This script creates synthetic data with serious problems and shows how semScreenR cleans it

# Load required libraries
library(semScreenR)
library(lavaan)
library(MASS)

# Suppress ethical reminder for demo
options(semScreenR.ethical_reminder = FALSE)

cat("=== semScreenR Enhanced Simulation Demo ===\n")
cat("Creating synthetic data with truly poor model fit...\n\n")

set.seed(123)  # For reproducibility

# Define a 3-factor CFA model with 5 indicators per factor
model_syntax <- '
  # Factor 1: Anxiety
  anxiety =~ anx1 + anx2 + anx3 + anx4 + anx5
  
  # Factor 2: Depression  
  depression =~ dep1 + dep2 + dep3 + dep4 + dep5
  
  # Factor 3: Stress
  stress =~ str1 + str2 + str3 + str4 + str5
'

# Step 1: Generate synthetic data with serious problems
n_obs <- 300
create_bad_fit_data <- function(n) {
  # Create truly problematic data
  data <- data.frame(matrix(NA, nrow = n, ncol = 15))
  colnames(data) <- c(paste0("anx", 1:5), paste0("dep", 1:5), paste0("str", 1:5))
  
  # Generate three underlying factors with realistic correlations
  factor_cors <- matrix(c(
    1.0, 0.7, 0.6,
    0.7, 1.0, 0.8,
    0.6, 0.8, 1.0
  ), nrow = 3)
  
  factors <- mvrnorm(n, mu = c(0, 0, 0), Sigma = factor_cors)
  
  # PROBLEM 1: Very weak loadings for some items
  loadings <- list(
    anxiety = c(0.75, 0.70, 0.30, 0.25, 0.65),      # anx3, anx4 weak
    depression = c(0.75, 0.20, 0.65, 0.60, 0.70),   # dep2 very weak
    stress = c(0.75, 0.70, 0.65, 0.15, 0.60)        # str4 very weak
  )
  
  # Generate indicators with high error
  error_sd <- 0.8
  
  for (i in 1:5) {
    data[, paste0("anx", i)] <- loadings$anxiety[i] * factors[, 1] + rnorm(n, 0, error_sd)
    data[, paste0("dep", i)] <- loadings$depression[i] * factors[, 2] + rnorm(n, 0, error_sd)
    data[, paste0("str", i)] <- loadings$stress[i] * factors[, 3] + rnorm(n, 0, error_sd)
  }
  
  # PROBLEM 2: Add cross-loadings to create model misspecification
  # anx3 also loads on depression (complex structure)
  data$anx3 <- data$anx3 + 0.4 * factors[, 2] + rnorm(n, 0, 0.3)
  # str4 also loads on anxiety
  data$str4 <- data$str4 + 0.3 * factors[, 1] + rnorm(n, 0, 0.3)
  
  # PROBLEM 3: Add method effects/correlated errors
  # Create systematic response bias in last items of each scale
  method_effect <- rnorm(n, 0, 0.4)
  data$anx5 <- data$anx5 + method_effect
  data$dep5 <- data$dep5 + method_effect  
  data$str5 <- data$str5 + method_effect
  
  # PROBLEM 4: Add some extreme responders and careless responding
  # Extreme response style for 10% of participants
  extreme_idx <- sample(n, floor(0.10 * n))
  for (idx in extreme_idx) {
    # Multiply responses to make them more extreme
    data[idx, ] <- data[idx, ] * 1.5
  }
  
  # Careless responding for 8% of participants  
  careless_idx <- sample(setdiff(1:n, extreme_idx), floor(0.08 * n))
  for (idx in careless_idx) {
    data[idx, ] <- sample(1:7, ncol(data), replace = TRUE)
  }
  
  # Convert to Likert scale (1-7) with appropriate centering
  for (col in 1:ncol(data)) {
    data[, col] <- round(pmax(1, pmin(7, data[, col] + 4)))
  }
  
  data <- as.data.frame(data)
  
  list(
    data = data,
    true_problematic = c("anx3", "anx4", "dep2", "str4"),
    careless_rows = careless_idx,
    extreme_rows = extreme_idx
  )
}

# Generate the problematic dataset
cat("Generating problematic dataset...\n")
sim_result <- create_bad_fit_data(n_obs)
problem_data <- sim_result$data

cat("Generated dataset with", nrow(problem_data), "observations and", ncol(problem_data), "variables\n")
cat("Expected problematic items:", paste(sim_result$true_problematic, collapse = ", "), "\n")
cat("Added", length(sim_result$careless_rows), "careless responders and", 
    length(sim_result$extreme_rows), "extreme responders\n\n")

# Step 2: Check initial model fit
cat("=== INITIAL MODEL FIT (Before Cleaning) ===\n")
fit_initial <- cfa(model_syntax, data = problem_data)
fit_measures_initial <- fitMeasures(fit_initial, c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue"))

cat(sprintf("CFI: %.3f (target: > 0.95)\n", fit_measures_initial["cfi"]))
cat(sprintf("TLI: %.3f (target: > 0.95)\n", fit_measures_initial["tli"]))
cat(sprintf("RMSEA: %.3f (target: < 0.06)\n", fit_measures_initial["rmsea"]))
cat(sprintf("SRMR: %.3f (target: < 0.08)\n", fit_measures_initial["srmr"]))
cat(sprintf("Chi-square: %.2f (df = %.0f, p = %.4f)\n", 
            fit_measures_initial["chisq"], fit_measures_initial["df"], fit_measures_initial["pvalue"]))

# Show standardized loadings
cat("\n=== STANDARDIZED FACTOR LOADINGS ===\n")
std_loadings <- standardizedSolution(fit_initial)
loadings_only <- subset(std_loadings, op == "=~")
loadings_only$abs_loading <- abs(loadings_only$est.std)
loadings_only <- loadings_only[order(loadings_only$abs_loading), ]

for(i in 1:nrow(loadings_only)) {
  loading_val <- loadings_only$est.std[i]
  item <- loadings_only$rhs[i]
  factor <- loadings_only$lhs[i]
  flag <- if(abs(loading_val) < 0.4) " <- WEAK!" else ""
  cat(sprintf("%s -> %s: %.3f%s\n", factor, item, loading_val, flag))
}

# Check if fit is actually poor
initial_poor_fit <- fit_measures_initial["cfi"] < 0.95 || 
                   fit_measures_initial["tli"] < 0.95 || 
                   fit_measures_initial["rmsea"] > 0.06 || 
                   fit_measures_initial["srmr"] > 0.08

if (!initial_poor_fit) {
  cat("\n⚠️  WARNING: Initial model actually has acceptable fit. Adjusting thresholds for demonstration...\n")
}

# Step 3: Set up semScreenR cleaning
cat("\n=== SETTING UP CLEANING PLAN ===\n")

# Use balanced preset but with lower loading threshold for demo
config <- triage_rules(
  preset = "balanced",
  loading_min = 0.35  # Lower threshold to catch more weak items
)

# Override some settings for more aggressive cleaning
config$limits$max_item_drop_frac <- 0.25  # Allow more item removal
config$gates$min_delta$CFI <- 0.002        # Lower improvement threshold
config$gates$min_delta$TLI <- 0.002
config$gates$min_delta$RMSEA <- -0.002
config$gates$min_delta$SRMR <- -0.002

plan <- triage_plan(
  dat = problem_data,
  model = model_syntax,
  protected = c("anx1", "dep1", "str1"),
  config = config
)

cat("Plan created with loading threshold of", config$thresholds$loading_min, "\n")
cat("Protected items:", paste(plan$protected, collapse = ", "), "\n")
cat("Preset:", config$meta$preset, "with modifications\n")
cat("Maximum item drop fraction:", config$limits$max_item_drop_frac, "\n")

# Step 4: Apply cleaning
cat("\n=== APPLYING CLEANING PROCEDURE ===\n")
result <- triage_apply(
  dat = problem_data,
  model = model_syntax,
  plan = plan
)

cat("Cleaning status:", result$status, "\n")
cat("Number of cleaning steps:", length(result$history), "\n\n")

# Show cleaning history
if (length(result$history) > 0) {
  cat("=== CLEANING HISTORY ===\n")
  for (i in seq_along(result$history)) {
    step <- result$history[[i]]
    if (step$type == "item_drop" && isTRUE(step$kept)) {
      deltas <- step$deltas
      cat(sprintf("Step %d: ✅ REMOVED '%s'\n", i, step$item))
      cat(sprintf("  - CFI improvement: %+.3f\n", deltas["cfi"]))
      cat(sprintf("  - TLI improvement: %+.3f\n", deltas["tli"]))
      cat(sprintf("  - RMSEA improvement: %+.3f\n", deltas["rmsea"]))
      cat(sprintf("  - SRMR improvement: %+.3f\n", deltas["srmr"]))
      cat(sprintf("  - Note: %s\n\n", step$note))
    } else if (step$type == "item_drop_rejected") {
      cat(sprintf("Step %d: ❌ REJECTED removal of '%s'\n", i, step$item))
      cat(sprintf("  - Reason: %s\n\n", step$note))
    }
  }
} else {
  cat("ℹ️  No items were removed during cleaning.\n\n")
}

# Step 5: Compare fits
cat("=== MODEL FIT COMPARISON ===\n")
if (length(result$fit$post) > 0) {
  final_fit <- result$fit$post
  
  # Calculate improvements
  cfi_change <- final_fit$cfi - fit_measures_initial["cfi"]
  tli_change <- final_fit$tli - fit_measures_initial["tli"]
  rmsea_change <- final_fit$rmsea - fit_measures_initial["rmsea"]
  srmr_change <- final_fit$srmr - fit_measures_initial["srmr"]
  
  cat("BEFORE -> AFTER (change):\n")
  cat(sprintf("CFI:   %.3f -> %.3f (%+.3f)\n", fit_measures_initial["cfi"], final_fit$cfi, cfi_change))
  cat(sprintf("TLI:   %.3f -> %.3f (%+.3f)\n", fit_measures_initial["tli"], final_fit$tli, tli_change))
  cat(sprintf("RMSEA: %.3f -> %.3f (%+.3f)\n", fit_measures_initial["rmsea"], final_fit$rmsea, rmsea_change))
  cat(sprintf("SRMR:  %.3f -> %.3f (%+.3f)\n", fit_measures_initial["srmr"], final_fit$srmr, srmr_change))
  
  # Check if cleaning improved things
  initial_acceptable <- fit_measures_initial["cfi"] > 0.95 && 
                       fit_measures_initial["tli"] > 0.95 && 
                       fit_measures_initial["rmsea"] < 0.06 && 
                       fit_measures_initial["srmr"] < 0.08
  
  final_acceptable <- final_fit$cfi > 0.95 && 
                     final_fit$tli > 0.95 && 
                     final_fit$rmsea < 0.06 && 
                     final_fit$srmr < 0.08
  
  cat(sprintf("\nFit Assessment:\n"))
  cat(sprintf("Initial fit acceptable: %s\n", ifelse(initial_acceptable, "YES", "NO")))
  cat(sprintf("Final fit acceptable: %s\n", ifelse(final_acceptable, "YES", "NO")))
  
} else {
  cat("Final fit measures not available.\n")
}

# Step 6: Generate report and summary
cat("\n=== GENERATING DETAILED REPORT ===\n")
export_sem_report(
  dat = problem_data,
  model = model_syntax, 
  res = result,
  file = "enhanced_simulation_report.html"
)
cat("Report saved as: enhanced_simulation_report.html\n")

# Generate path diagrams if possible
tryCatch({
  sem_paths_prepost(result, "enhanced_sim_paths", width = 800, height = 600)
  cat("Path diagrams saved as: enhanced_sim_paths_pre.png and enhanced_sim_paths_post.png\n")
}, error = function(e) {
  cat("Path diagrams could not be generated:", e$message, "\n")
})

# Final summary
cat("\n=== SIMULATION SUMMARY ===\n")

# Identify removed items
removed_items <- character()
for (step in result$history) {
  if (step$type == "item_drop" && isTRUE(step$kept)) {
    removed_items <- c(removed_items, step$item)
  }
}

cat("Items successfully removed:", ifelse(length(removed_items) > 0, 
                                          paste(removed_items, collapse = ", "), "None"), "\n")
cat("Expected problematic items:", paste(sim_result$true_problematic, collapse = ", "), "\n")

if (length(removed_items) > 0) {
  correctly_identified <- intersect(removed_items, sim_result$true_problematic)
  missed_items <- setdiff(sim_result$true_problematic, removed_items)
  false_positives <- setdiff(removed_items, sim_result$true_problematic)
  
  cat("✅ Correctly identified:", ifelse(length(correctly_identified) > 0,
                                       paste(correctly_identified, collapse = ", "), "None"), "\n")
  cat("⚠️  Missed problematic items:", ifelse(length(missed_items) > 0,
                                           paste(missed_items, collapse = ", "), "None"), "\n")
  cat("⚡ Unexpected removals:", ifelse(length(false_positives) > 0,
                                      paste(false_positives, collapse = ", "), "None"), "\n")
  
  success_rate <- length(correctly_identified) / length(sim_result$true_problematic)
  cat(sprintf("Success rate: %.1f%% (%d/%d problematic items correctly identified)\n",
              success_rate * 100, length(correctly_identified), length(sim_result$true_problematic)))
}

# Overall assessment
if (length(removed_items) > 0) {
  cat("\n🎉 SUCCESS: semScreenR detected and removed problematic items!\n")
} else {
  cat("\n📊 INFO: No items met the removal criteria in this iteration\n")
  cat("   Consider adjusting thresholds if you expected more cleaning\n")
}

cat("\n=== Demo Complete ===\n")
cat("Generated files:\n")
cat("- enhanced_simulation_report.html (detailed analysis report)\n")
if (file.exists("enhanced_sim_paths_pre.png")) {
  cat("- enhanced_sim_paths_pre.png (model before cleaning)\n")
  cat("- enhanced_sim_paths_post.png (model after cleaning)\n")
}

cat("\nThis demo shows semScreenR's ability to:\n")
cat("✓ Detect items with weak factor loadings\n") 
cat("✓ Assess model fit improvement from item removal\n")
cat("✓ Use cross-validation to avoid overfitting\n")
cat("✓ Apply ethical safeguards and transparent reporting\n")
