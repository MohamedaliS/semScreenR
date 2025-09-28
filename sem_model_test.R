#!/usr/bin/env Rscript
# Testing semScreenR with SEM Model (Regression Paths)
# Using the classic Political Democracy example

library(semScreenR)
library(lavaan)
options(semScreenR.ethical_reminder = FALSE)

cat("=== semScreenR SEM Model Test ===\n")
cat("Testing with structural equation model (not just CFA)...\n\n")

# Load the classic Political Democracy dataset
data(PoliticalDemocracy, package = "lavaan")

# Your SEM model (without residual correlations)
sem_model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
'

# Original model with residual correlations for comparison
full_model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

cat("Dataset: Political Democracy (n =", nrow(PoliticalDemocracy), ")\n")
cat("Variables:", paste(names(PoliticalDemocracy), collapse = ", "), "\n\n")

# Test hierarchy detection first
cat("=== MODEL STRUCTURE ANALYSIS ===\n")
hierarchy_info <- semScreenR:::detect_model_hierarchy(sem_model)
cat("Model type detected:\n")
cat("- Is hierarchical:", hierarchy_info$is_hierarchical, "\n") 
cat("- Is bifactor:", hierarchy_info$is_bifactor, "\n")
cat("- Factors:", paste(hierarchy_info$factors, collapse = ", "), "\n")
cat("- Total indicators:", hierarchy_info$total_indicators, "\n")

# Fit both models to compare
cat("\n=== MODEL FIT COMPARISON ===\n")

# Fit your SEM model (without residual correlations)
cat("Fitting SEM model (without residual correlations)...\n")
fit_sem <- sem(sem_model, data = PoliticalDemocracy)
sem_measures <- fitMeasures(fit_sem, c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue"))

cat("SEM Model Fit (without residual correlations):\n")
cat(sprintf("  CFI: %.3f\n", sem_measures["cfi"]))
cat(sprintf("  TLI: %.3f\n", sem_measures["tli"]))
cat(sprintf("  RMSEA: %.3f\n", sem_measures["rmsea"]))
cat(sprintf("  SRMR: %.3f\n", sem_measures["srmr"]))
cat(sprintf("  Chi-square: %.2f (df = %.0f, p = %.4f)\n", 
            sem_measures["chisq"], sem_measures["df"], sem_measures["pvalue"]))

# Fit full model with residual correlations
cat("\nFitting full model (with residual correlations) for comparison...\n")
fit_full <- sem(full_model, data = PoliticalDemocracy)
full_measures <- fitMeasures(fit_full, c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue"))

cat("Full Model Fit (with residual correlations):\n")
cat(sprintf("  CFI: %.3f\n", full_measures["cfi"]))
cat(sprintf("  TLI: %.3f\n", full_measures["tli"]))
cat(sprintf("  RMSEA: %.3f\n", full_measures["rmsea"]))
cat(sprintf("  SRMR: %.3f\n", full_measures["srmr"]))
cat(sprintf("  Chi-square: %.2f (df = %.0f, p = %.4f)\n", 
            full_measures["chisq"], full_measures["df"], full_measures["pvalue"]))

# Compare fit
improvement <- data.frame(
  Measure = c("CFI", "TLI", "RMSEA", "SRMR"),
  SEM_Model = c(sem_measures["cfi"], sem_measures["tli"], sem_measures["rmsea"], sem_measures["srmr"]),
  Full_Model = c(full_measures["cfi"], full_measures["tli"], full_measures["rmsea"], full_measures["srmr"]),
  Difference = c(full_measures["cfi"] - sem_measures["cfi"],
                full_measures["tli"] - sem_measures["tli"], 
                full_measures["rmsea"] - sem_measures["rmsea"],
                full_measures["srmr"] - sem_measures["srmr"])
)

cat("\nFit Comparison (Full - SEM):\n")
for (i in 1:nrow(improvement)) {
  cat(sprintf("  %s: %.3f vs %.3f (difference: %+.3f)\n",
              improvement$Measure[i], improvement$SEM_Model[i], 
              improvement$Full_Model[i], improvement$Difference[i]))
}

# Check if SEM model needs improvement
sem_acceptable <- sem_measures["cfi"] > 0.95 && sem_measures["tli"] > 0.95 && 
                 sem_measures["rmsea"] < 0.06 && sem_measures["srmr"] < 0.08

cat(sprintf("\nSEM model fit acceptable: %s\n", ifelse(sem_acceptable, "YES", "NO")))

# Now test semScreenR with the SEM model
cat("\n=== TESTING semScreenR WITH SEM MODEL ===\n")

# Set up semScreenR
config <- triage_rules(preset = "balanced", loading_min = 0.40)
plan <- triage_plan(
  dat = PoliticalDemocracy,
  model = sem_model,
  protected = c("x1", "y1", "y5"),  # Protect first indicator of each factor
  config = config
)

cat("semScreenR setup:\n")
cat("- Preset:", config$meta$preset, "\n")
cat("- Loading threshold:", config$thresholds$loading_min, "\n")
cat("- Protected items:", paste(plan$protected, collapse = ", "), "\n")

# Apply semScreenR cleaning
cat("\nApplying semScreenR to SEM model...\n")
result <- triage_apply(
  dat = PoliticalDemocracy,
  model = sem_model,
  plan = plan
)

cat("semScreenR Results:\n")
cat("- Status:", result$status, "\n")
cat("- History steps:", length(result$history), "\n")

# Show any actions taken
if (length(result$history) > 0) {
  cat("\n=== CLEANING HISTORY ===\n")
  for (i in seq_along(result$history)) {
    step <- result$history[[i]]
    
    if (step$type == "hierarchy_detected") {
      cat(sprintf("Step %d: %s - %s\n", i, step$type, step$note))
    } else if (step$type == "item_drop" && isTRUE(step$kept)) {
      deltas <- step$deltas
      cat(sprintf("Step %d: ✅ REMOVED '%s'\n", i, step$item))
      cat(sprintf("  - CFI: %+.3f, TLI: %+.3f, RMSEA: %+.3f, SRMR: %+.3f\n",
                  deltas["cfi"], deltas["tli"], deltas["rmsea"], deltas["srmr"]))
    } else if (step$type == "item_drop_rejected") {
      cat(sprintf("Step %d: ❌ REJECTED '%s' - %s\n", i, step$item, step$note))
    } else {
      cat(sprintf("Step %d: %s - %s\n", i, step$type, step$note %||% "No details"))
    }
  }
}

# Show final fit comparison
if (length(result$fit$post) > 0) {
  final_fit <- result$fit$post
  
  cat("\n=== BEFORE vs AFTER CLEANING ===\n")
  cat("Measure     | Before  | After   | Change\n")
  cat("------------|---------|---------|--------\n")
  cat(sprintf("CFI         | %.3f   | %.3f   | %+.3f\n", 
              sem_measures["cfi"], final_fit$cfi, 
              final_fit$cfi - sem_measures["cfi"]))
  cat(sprintf("TLI         | %.3f   | %.3f   | %+.3f\n", 
              sem_measures["tli"], final_fit$tli,
              final_fit$tli - sem_measures["tli"]))
  cat(sprintf("RMSEA       | %.3f   | %.3f   | %+.3f\n", 
              sem_measures["rmsea"], final_fit$rmsea,
              final_fit$rmsea - sem_measures["rmsea"]))
  cat(sprintf("SRMR        | %.3f   | %.3f   | %+.3f\n", 
              sem_measures["srmr"], final_fit$srmr,
              final_fit$srmr - sem_measures["srmr"]))
}

# Generate report
cat("\n=== GENERATING REPORT ===\n")
export_sem_report(
  dat = PoliticalDemocracy,
  model = sem_model,
  res = result,
  file = "sem_model_test_report.html"
)
cat("Report saved: sem_model_test_report.html\n")

# Final assessment
cat("\n=== ASSESSMENT ===\n")
if (result$status == "completed_with_changes") {
  cat("🎯 semScreenR successfully cleaned the SEM model!\n")
  cat("This shows semScreenR can handle structural equation models,\n")
  cat("not just confirmatory factor analysis models.\n")
} else if (result$status == "completed_no_changes") {
  cat("📊 semScreenR found no items to remove from the SEM model.\n")
  cat("This could mean:\n")
  cat("- All indicators have acceptable loadings\n")
  cat("- Model already has good fit\n")
  cat("- Improvements didn't meet thresholds\n")
  cat("- Validation prevented potentially problematic removals\n")
} else {
  cat("⚠️  semScreenR encountered an issue:", result$status, "\n")
}

cat("\n=== KEY INSIGHTS ===\n")
cat("1. SEM models (with regression paths) can be tested with semScreenR\n")
cat("2. The measurement model portion gets evaluated for weak indicators\n")
cat("3. Structural paths (regressions) are preserved during cleaning\n") 
cat("4. Adding residual correlations improves fit but may mask indicator issues\n")
cat("5. semScreenR focuses on the measurement quality, not structural paths\n")

cat("\n🏁 SEM Model Test Complete! 🏁\n")
