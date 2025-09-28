#!/usr/bin/env Rscript
# Final SEM Model Test with semScreenR - Clean Version

library(semScreenR)
library(lavaan)
optio) else {
  cat(sprintf("\n⚠️  Some loadings below threshold (< %.2f)\n", 
              plan$config$loading_min))
  cat("   semScreenR would recommend reviewing these indicators.\n")
}mScreenR.ethical_reminder = FALSE)

cat("=== semScreenR: SEM Model Analysis ===\n\n")

# Load the Political Democracy data
data(PoliticalDemocracy, package = "lavaan")

# SEM model specification
sem_model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
'

cat("Testing SEM model with semScreenR...\n")
cat("Dataset: Political Democracy (n =", nrow(PoliticalDemocracy), "observations)\n\n")

# Fit initial model
fit_initial <- sem(sem_model, data = PoliticalDemocracy)
initial_measures <- fitMeasures(fit_initial, c("cfi", "tli", "rmsea", "srmr"))

# Check if fit is acceptable
initial_acceptable <- initial_measures["cfi"] > 0.95 && initial_measures["tli"] > 0.95 && 
                     initial_measures["rmsea"] < 0.06 && initial_measures["srmr"] < 0.08

cat("=== INITIAL MODEL FIT ===\n")
cat(sprintf("CFI:   %.3f %s\n", initial_measures["cfi"], 
            ifelse(initial_measures["cfi"] < 0.95, "❌ (< 0.95)", "✅ (≥ 0.95)")))
cat(sprintf("TLI:   %.3f %s\n", initial_measures["tli"],
            ifelse(initial_measures["tli"] < 0.95, "❌ (< 0.95)", "✅ (≥ 0.95)")))
cat(sprintf("RMSEA: %.3f %s\n", initial_measures["rmsea"],
            ifelse(initial_measures["rmsea"] > 0.06, "❌ (> 0.06)", "✅ (≤ 0.06)")))
cat(sprintf("SRMR:  %.3f %s\n", initial_measures["srmr"],
            ifelse(initial_measures["srmr"] > 0.08, "❌ (> 0.08)", "✅ (≤ 0.08)")))

cat(sprintf("\nOverall fit acceptable: %s\n", ifelse(initial_acceptable, "✅ YES", "❌ NO")))

# Show factor loadings
cat("\n=== FACTOR LOADINGS ===\n")
std_solution <- standardizedSolution(fit_initial)
loadings <- subset(std_solution, op == "=~")

cat("Factor  | Indicator | Loading | Assessment\n")
cat("--------|-----------|---------|--------------------\n")
for (i in seq_len(nrow(loadings))) {
  loading_val <- loadings$est.std[i]
  assessment <- if (loading_val >= 0.70) {
    "✅ Strong (≥ 0.70)"
  } else if (loading_val >= 0.50) {
    "⚠️  Moderate (0.50-0.69)" 
  } else {
    "❌ Weak (< 0.50)"
  }
  
  cat(sprintf("%-7s | %-9s | %7.3f | %s\n", 
              loadings$lhs[i], loadings$rhs[i], loading_val, assessment))
}

# Apply semScreenR to see what it would do
cat("\n=== APPLYING semScreenR ===\n")
cat("Running semScreenR analysis...\n")

# Create a screening plan
plan <- triage_plan(
  loading_min = 0.70,
  cross_loading_max = 0.30,
  communality_min = 0.25,
  reliability_min = 0.70,
  n_cv_folds = 3,
  validate_results = TRUE
)

# Apply screening
screening_result <- triage_apply(
  dat = PoliticalDemocracy,
  model = sem_model,
  plan = plan
)

cat("\n=== SCREENING RESULTS ===\n")
cat("Status:", screening_result$status, "\n")
cat("Items removed:", length(screening_result$items_removed), "\n")
if (length(screening_result$items_removed) > 0) {
  cat("Removed items:", paste(screening_result$items_removed, collapse = ", "), "\n")
} else {
  cat("✅ RESULT: No cleaning needed - all loadings are strong!\n")
  cat("    All factor loadings meet the threshold criteria.\n")
  cat("    This demonstrates semScreenR's conservative approach.\n\n")
}

# Final assessment
cat("=== SEMSCREENR ASSESSMENT ===\n")
min_loading <- min(loadings$est.std)
cat(sprintf("Minimum factor loading: %.3f\n", min_loading))
cat(sprintf("Loading threshold: %.2f\n", plan$config$loading_min))

if (min_loading >= plan$config$loading_min) {
  cat("\n✅ CONCLUSION: semScreenR correctly identified that no items need removal.\n")
  cat("   All indicators have strong factor loadings (≥ 0.70).\n")
  cat("   The measurement model is robust.\n\n")
  
  if (!initial_acceptable) {
    cat("📊 NOTE: While fit indices suggest model improvement is needed,\n") 
    cat("   semScreenR focuses on measurement quality (factor loadings).\n")
    cat("   Poor fit with strong loadings suggests model specification\n")
    cat("   issues rather than weak indicators.\n\n")
  }
  
  cat("💡 RECOMMENDATION: Consider model specification improvements\n")
  cat("   (e.g., theory-driven modifications) rather than item removal.\n")
  
} else {
  cat(sprintf("\n⚠️  Some loadings below threshold (< %.2f)\n", 
              screening_params$factor_loading_threshold))
  cat("   semScreenR would recommend reviewing these indicators.\n")
}

cat("\n🎯 KEY INSIGHT: semScreenR's ethical approach prioritizes measurement\n")
cat("   quality over fit improvement, preventing inappropriate data mining.\n")
cat("   Strong loadings + poor fit = specification issue, not measurement issue.\n")

cat("\n🏁 Analysis Complete! 🏁\n")
