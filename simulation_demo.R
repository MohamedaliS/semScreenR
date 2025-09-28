#!/usr/bin/env Rscript
# Simulation Demo: semScreenR with Poor Fit Data
# This script creates synthetic data with deliberate problems and shows how semScreenR cleans it

# Load required libraries
library(semScreenR)
library(lavaan)
library(MASS)

cat("=== semScreenR Simulation Demo ===\n")
cat("Creating synthetic data with poor model fit...\n\n")

set.seed(42)  # For reproducibility

# Define a 3-factor CFA model with 5 indicators per factor
model_syntax <- '
  # Factor 1: Anxiety
  anxiety =~ anx1 + anx2 + anx3 + anx4 + anx5
  
  # Factor 2: Depression  
  depression =~ dep1 + dep2 + dep3 + dep4 + dep5
  
  # Factor 3: Stress
  stress =~ str1 + str2 + str3 + str4 + str5
'

# Step 1: Generate "good" underlying factor scores
n_obs <- 400
factor_cors <- matrix(c(
  1.0, 0.6, 0.5,
  0.6, 1.0, 0.7,
  0.5, 0.7, 1.0
), nrow = 3)

true_factors <- mvrnorm(n_obs, mu = c(0, 0, 0), Sigma = factor_cors)
colnames(true_factors) <- c("F_anxiety", "F_depression", "F_stress")

# Step 2: Create observed indicators with deliberate problems
create_problematic_data <- function(factors) {
  n_factors <- nrow(factors)
  data <- as.data.frame(matrix(NA, nrow = n_factors, ncol = 15))
  colnames(data) <- c(paste0("anx", 1:5), paste0("dep", 1:5), paste0("str", 1:5))
  
  # Good loadings for most items
  good_loadings <- c(0.8, 0.75, 0.7, 0.65, 0.6)
  
  # PROBLEM 1: Some items have very weak loadings to create poor fit
  weak_loadings <- c(0.6, 0.55, 0.15, 0.12, 0.45)   # anx3, anx4 are very weak
  dep_loadings <- c(0.6, 0.08, 0.55, 0.45, 0.50)    # dep2 is very weak  
  str_loadings <- c(0.6, 0.55, 0.50, 0.05, 0.45)    # str4 is very weak
  
  # Generate indicators with more error to create poor fit
  error_var <- 1.5  # Even more error variance
  
  # Anxiety items (2 problematic)
  for(i in 1:5) {
    data[, paste0("anx", i)] <- weak_loadings[i] * factors[, "F_anxiety"] + 
                                rnorm(n_factors, 0, sqrt(error_var))
  }
  
  # Depression items (1 problematic)
  for(i in 1:5) {
    data[, paste0("dep", i)] <- dep_loadings[i] * factors[, "F_depression"] + 
                                rnorm(n_factors, 0, sqrt(error_var))
  }
  
  # Stress items (1 problematic)
  for(i in 1:5) {
    data[, paste0("str", i)] <- str_loadings[i] * factors[, "F_stress"] + 
                                rnorm(n_factors, 0, sqrt(error_var))
  }
  
  # PROBLEM 2: Add some careless responding patterns (more aggressive)
  # Random responding for 8% of cases
  careless_idx <- sample(n_factors, floor(0.08 * n_factors))
  for(idx in careless_idx) {
    # Make responses completely random (Likert scale 1-7)
    data[idx, ] <- sample(1:7, ncol(data), replace = TRUE)
  }
  
  # PROBLEM 3: Add extreme outliers for another 5% of cases
  outlier_idx <- sample(setdiff(1:n_factors, careless_idx), floor(0.05 * n_factors))
  for(idx in outlier_idx) {
    # Make some responses extreme
    extreme_cols <- sample(ncol(data), 5)  # More extreme responses
    data[idx, extreme_cols] <- ifelse(runif(5) > 0.5, 7, 1)
  }
  
  # Convert to appropriate scale (1-7 Likert)
  for(col in 1:ncol(data)) {
    data[, col] <- round(pmax(1, pmin(7, data[, col] + 4)))  # Center around 4, bound to 1-7
  }
  
  # Ensure it's a data.frame
  data <- as.data.frame(data)
  
  return(list(
    data = data,
    careless_rows = careless_idx,
    outlier_rows = outlier_idx,
    true_weak_items = c("anx3", "anx4", "dep2", "str4")
  ))
}

# Generate the problematic dataset
sim_result <- create_problematic_data(true_factors)
problematic_data <- sim_result$data
true_weak_items <- sim_result$true_weak_items

cat("Generated dataset with", nrow(problematic_data), "observations and", ncol(problematic_data), "variables\n")
cat("Data class:", class(problematic_data), "\n")
cat("Data structure:\n")
str(problematic_data)
cat("Deliberately included weak items:", paste(true_weak_items, collapse = ", "), "\n")
cat("Added", length(sim_result$careless_rows), "careless responders and", length(sim_result$outlier_rows), "outliers\n\n")

# Step 3: Fit the model to show poor initial fit
cat("=== INITIAL MODEL FIT (Before Cleaning) ===\n")
fit_initial <- cfa(model_syntax, data = problematic_data)
fit_measures_initial <- fitMeasures(fit_initial, c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue"))

cat(sprintf("CFI: %.3f (target: > 0.95)\n", fit_measures_initial["cfi"]))
cat(sprintf("TLI: %.3f (target: > 0.95)\n", fit_measures_initial["tli"]))
cat(sprintf("RMSEA: %.3f (target: < 0.06)\n", fit_measures_initial["rmsea"]))
cat(sprintf("SRMR: %.3f (target: < 0.08)\n", fit_measures_initial["srmr"]))
cat(sprintf("Chi-square: %.2f (df = %.0f, p = %.4f)\n", 
            fit_measures_initial["chisq"], fit_measures_initial["df"], fit_measures_initial["pvalue"]))

# Show standardized loadings to identify weak items
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

# Step 4: Set up semScreenR cleaning plan
cat("\n=== SETTING UP CLEANING PLAN ===\n")

# Create a plan with aggressive cleaning settings
config <- triage_rules(
  preset = "aggressive",
  loading_min = 0.4
)

plan <- triage_plan(
  dat = problematic_data,
  model = model_syntax,
  protected = c("anx1", "dep1", "str1"),  # Protect first item of each factor
  extra_row_proposals = NULL,  # We'll detect careless responding automatically if implemented
  config = config
)

cat("Plan created with loading threshold of", config$thresholds$loading_min, "\n")
cat("Protected items:", paste(plan$protected, collapse = ", "), "\n")
cat("Preset:", config$meta$preset, "\n")
cat("Maximum iterations:", config$limits$max_iterations, "\n")

# Step 5: Apply the cleaning procedure
cat("\n=== APPLYING CLEANING PROCEDURE ===\n")
result <- triage_apply(
  dat = problematic_data,
  model = model_syntax,
  plan = plan
)

cat("Cleaning status:", result$status, "\n")
cat("Number of cleaning steps:", length(result$history), "\n\n")

# Show the cleaning history
if (length(result$history) > 0) {
  cat("=== CLEANING HISTORY ===\n")
  for (i in seq_along(result$history)) {
    step <- result$history[[i]]
    if (step$type == "item_drop" && isTRUE(step$kept)) {
      deltas <- step$deltas
      cat(sprintf("Step %d: Removed '%s'\n", i, step$item))
      cat(sprintf("  - CFI improvement: %+.3f\n", deltas["cfi"]))
      cat(sprintf("  - TLI improvement: %+.3f\n", deltas["tli"]))
      cat(sprintf("  - RMSEA improvement: %+.3f\n", deltas["rmsea"]))
      cat(sprintf("  - SRMR improvement: %+.3f\n", deltas["srmr"]))
      cat(sprintf("  - Note: %s\n\n", step$note))
    } else if (step$type == "item_drop_rejected") {
      cat(sprintf("Step %d: REJECTED removal of '%s'\n", i, step$item))
      cat(sprintf("  - Reason: %s\n\n", step$note))
    }
  }
}

# Step 6: Show final model fit
cat("=== FINAL MODEL FIT (After Cleaning) ===\n")
final_fit_measures <- result$fit$post

cat(sprintf("CFI: %.3f (was %.3f, change: %+.3f)\n", 
            final_fit_measures$cfi, fit_measures_initial["cfi"], 
            final_fit_measures$cfi - fit_measures_initial["cfi"]))
cat(sprintf("TLI: %.3f (was %.3f, change: %+.3f)\n", 
            final_fit_measures$tli, fit_measures_initial["tli"], 
            final_fit_measures$tli - fit_measures_initial["tli"]))
cat(sprintf("RMSEA: %.3f (was %.3f, change: %+.3f)\n", 
            final_fit_measures$rmsea, fit_measures_initial["rmsea"], 
            final_fit_measures$rmsea - fit_measures_initial["rmsea"]))
cat(sprintf("SRMR: %.3f (was %.3f, change: %+.3f)\n", 
            final_fit_measures$srmr, fit_measures_initial["srmr"], 
            final_fit_measures$srmr - fit_measures_initial["srmr"]))

# Step 7: Generate and save a detailed report
cat("\n=== GENERATING DETAILED REPORT ===\n")
export_sem_report(
  dat = problematic_data,
  model = model_syntax, 
  res = result,
  file = "simulation_cleaning_report.html"
)

cat("Detailed HTML report saved as: simulation_cleaning_report.html\n")

# Step 8: Summary and conclusions
cat("\n=== SIMULATION SUMMARY ===\n")
removed_items <- character()
for (step in result$history) {
  if (step$type == "item_drop" && isTRUE(step$kept)) {
    removed_items <- c(removed_items, step$item)
  }
}

cat("Items successfully removed:", ifelse(length(removed_items) > 0, 
                                          paste(removed_items, collapse = ", "), "None"), "\n")
cat("Predicted weak items were:", paste(true_weak_items, collapse = ", "), "\n")

# Check if semScreenR identified the true problematic items
correctly_identified <- intersect(removed_items, true_weak_items)
missed_weak <- setdiff(true_weak_items, removed_items)
false_positives <- setdiff(removed_items, true_weak_items)

cat("Correctly identified weak items:", ifelse(length(correctly_identified) > 0,
                                               paste(correctly_identified, collapse = ", "), "None"), "\n")
cat("Missed weak items:", ifelse(length(missed_weak) > 0,
                                 paste(missed_weak, collapse = ", "), "None"), "\n")
cat("False positive removals:", ifelse(length(false_positives) > 0,
                                       paste(false_positives, collapse = ", "), "None"), "\n")

# Final assessment
initial_acceptable <- fit_measures_initial["cfi"] > 0.95 && 
                     fit_measures_initial["tli"] > 0.95 && 
                     fit_measures_initial["rmsea"] < 0.06 && 
                     fit_measures_initial["srmr"] < 0.08

final_acceptable <- final_fit_measures$cfi > 0.95 && 
                   final_fit_measures$tli > 0.95 && 
                   final_fit_measures$rmsea < 0.06 && 
                   final_fit_measures$srmr < 0.08

cat("\nModel fit assessment:\n")
cat("Initial fit acceptable:", ifelse(initial_acceptable, "YES", "NO"), "\n")
cat("Final fit acceptable:", ifelse(final_acceptable, "YES", "NO"), "\n")

if (!initial_acceptable && final_acceptable) {
  cat("\n🎉 SUCCESS: semScreenR improved a poor-fitting model to acceptable fit!\n")
} else if (!initial_acceptable && !final_acceptable) {
  cat("\n⚠️  PARTIAL: Model fit improved but still needs attention\n")
} else if (initial_acceptable) {
  cat("\n📊 INFO: Initial model already had acceptable fit\n")
}

cat("\n=== Demo Complete ===\n")
cat("Check the generated files:\n")
cat("- simulation_cleaning_report.html (detailed report)\n")
cat("- sem_paths_pre.png (path diagram before cleaning)\n") 
cat("- sem_paths_post.png (path diagram after cleaning)\n")
