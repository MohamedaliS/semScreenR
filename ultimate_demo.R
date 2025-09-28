#!/usr/bin/env Rscript
# Ultimate Simulation Demo: Truly Problematic Data for semScreenR
# This creates data that definitely needs cleaning

# Load required libraries and suppress ethical warning
library(semScreenR)
library(lavaan)
library(MASS)
options(semScreenR.ethical_reminder = FALSE)

cat("=== semScreenR Ultimate Simulation Demo ===\n")
cat("Creating data with EXTREMELY poor fit that needs cleaning...\n\n")

set.seed(456)  # Different seed for worse data

# Create the most problematic dataset possible while still being fittable
create_terrible_data <- function(n = 250) {
  # Create weak factor structure
  factors <- mvrnorm(n, mu = c(0, 0, 0), Sigma = diag(3) * 0.8)  # Weak factors
  
  data <- data.frame(matrix(NA, nrow = n, ncol = 15))
  colnames(data) <- c(paste0("anx", 1:5), paste0("dep", 1:5), paste0("str", 1:5))
  
  # EXTREMELY weak loadings for problem items
  loadings <- list(
    anxiety = c(0.60, 0.55, 0.08, 0.05, 0.50),        # anx3, anx4 extremely weak
    depression = c(0.60, 0.03, 0.50, 0.45, 0.55),     # dep2 extremely weak  
    stress = c(0.60, 0.50, 0.45, 0.02, 0.48)          # str4 extremely weak
  )
  
  # Generate with massive error
  error_sd <- 1.2
  
  for (i in 1:5) {
    data[, paste0("anx", i)] <- loadings$anxiety[i] * factors[, 1] + rnorm(n, 0, error_sd)
    data[, paste0("dep", i)] <- loadings$depression[i] * factors[, 2] + rnorm(n, 0, error_sd)
    data[, paste0("str", i)] <- loadings$stress[i] * factors[, 3] + rnorm(n, 0, error_sd)
  }
  
  # Add noise to weak items specifically
  data$anx3 <- data$anx3 + rnorm(n, 0, 1.5)  # Extra noise
  data$anx4 <- data$anx4 + rnorm(n, 0, 1.5)
  data$dep2 <- data$dep2 + rnorm(n, 0, 1.5)
  data$str4 <- data$str4 + rnorm(n, 0, 1.5)
  
  # Add completely random responses for weak items in 20% of cases
  random_idx <- sample(n, floor(0.20 * n))
  for (idx in random_idx) {
    data[idx, c("anx3", "anx4", "dep2", "str4")] <- sample(1:7, 4, replace = TRUE)
  }
  
  # Scale to 1-7 Likert 
  for (col in 1:ncol(data)) {
    data[, col] <- round(pmax(1, pmin(7, data[, col] + 4)))
  }
  
  data
}

# Generate terrible data
terrible_data <- create_terrible_data(250)
cat("Generated dataset with", nrow(terrible_data), "observations\n\n")

# Model
model_syntax <- '
  anxiety =~ anx1 + anx2 + anx3 + anx4 + anx5
  depression =~ dep1 + dep2 + dep3 + dep4 + dep5  
  stress =~ str1 + str2 + str3 + str4 + str5
'

# Check initial fit
cat("=== INITIAL MODEL FIT ===\n")
fit_initial <- cfa(model_syntax, data = terrible_data)
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
cat("\n=== FACTOR LOADINGS (Sorted by Strength) ===\n")
std_loadings <- standardizedSolution(fit_initial)
loadings_df <- subset(std_loadings, op == "=~")[, c("lhs", "rhs", "est.std")]
loadings_df <- loadings_df[order(abs(loadings_df$est.std)), ]

for (i in 1:nrow(loadings_df)) {
  loading <- loadings_df$est.std[i]
  item <- loadings_df$rhs[i]
  factor <- loadings_df$lhs[i]
  status <- if (abs(loading) < 0.3) "❌ VERY WEAK" else if (abs(loading) < 0.5) "⚠️ WEAK" else "✅ OK"
  cat(sprintf("%s -> %s: %.3f %s\n", factor, item, loading, status))
}

# Set up very aggressive cleaning
cat("\n=== SETTING UP AGGRESSIVE CLEANING ===\n")
config <- triage_rules(preset = "aggressive", loading_min = 0.20)
config$gates$min_delta$CFI <- 0.001      # Very small improvement needed
config$gates$min_delta$TLI <- 0.001
config$gates$min_delta$RMSEA <- -0.001
config$gates$min_delta$SRMR <- -0.001
config$gates$use_validation <- FALSE     # Disable validation for demo

plan <- triage_plan(
  dat = terrible_data,
  model = model_syntax, 
  protected = c("anx1", "dep1", "str1"),
  config = config
)

cat("Using loading threshold:", config$thresholds$loading_min, "\n")
cat("Validation disabled for more aggressive cleaning\n")

# Apply cleaning
cat("\n=== APPLYING CLEANING ===\n")
result <- triage_apply(dat = terrible_data, model = model_syntax, plan = plan)

cat("Status:", result$status, "\n")
cat("History steps:", length(result$history), "\n\n")

# Show what happened
removed_count <- 0
if (length(result$history) > 0) {
  cat("=== CLEANING ACTIONS ===\n")
  for (i in seq_along(result$history)) {
    step <- result$history[[i]]
    if (step$type == "item_drop" && isTRUE(step$kept)) {
      removed_count <- removed_count + 1
      deltas <- step$deltas
      cat(sprintf("✅ REMOVED '%s': CFI %+.3f, TLI %+.3f, RMSEA %+.3f, SRMR %+.3f\n", 
                  step$item, deltas["cfi"], deltas["tli"], deltas["rmsea"], deltas["srmr"]))
    } else if (step$type == "item_drop_rejected") {
      cat(sprintf("❌ REJECTED '%s': %s\n", step$item, step$note))
    }
  }
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
  
  # Overall assessment
  initial_bad <- initial_measures["cfi"] < 0.95 || initial_measures["tli"] < 0.95 || 
                initial_measures["rmsea"] > 0.06 || initial_measures["srmr"] > 0.08
  
  final_good <- final_measures$cfi > 0.95 && final_measures$tli > 0.95 && 
               final_measures$rmsea < 0.06 && final_measures$srmr < 0.08
  
  cat("\n=== OVERALL ASSESSMENT ===\n")
  if (removed_count > 0) {
    cat(sprintf("🎯 semScreenR removed %d problematic items\n", removed_count))
    if (initial_bad && final_good) {
      cat("🎉 SUCCESS: Transformed poor fit to acceptable fit!\n")
    } else if (initial_bad && !final_good) {
      cat("📈 IMPROVEMENT: Model fit improved but still needs work\n")
    } else {
      cat("🔧 OPTIMIZATION: Good model made even better\n")
    }
  } else {
    cat("ℹ️  No items were removed (possibly already acceptable fit)\n")
  }
}

# Generate report
cat("\n=== GENERATING REPORT ===\n")
export_sem_report(dat = terrible_data, model = model_syntax, res = result, file = "ultimate_demo_report.html")
cat("Report saved: ultimate_demo_report.html\n")

cat("\n🏁 DEMO COMPLETE! 🏁\n")
cat("This demonstration showed semScreenR's data cleaning capabilities.\n")
