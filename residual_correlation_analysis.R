#!/usr/bin/env Rscript
# Exploring Auto-Detection of Residual Correlations

library(semScreenR)
library(lavaan)
options(semScreenR.ethical_reminder = FALSE)

cat("=== Auto-Detection of Residual Correlations ===\n\n")

# Load data and model
data(PoliticalDemocracy, package = "lavaan")

sem_model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
'

cat("=== STEP 1: DETECTING POTENTIAL RESIDUAL CORRELATIONS ===\n")

# Fit initial model
fit_initial <- sem(sem_model, data = PoliticalDemocracy)

# Get modification indices (MI) to detect needed residual correlations
mi_results <- modindices(fit_initial)

# Focus on residual correlations (~~) with high MI values
residual_cors <- subset(mi_results, op == "~~" & lhs != rhs)
residual_cors <- residual_cors[order(-residual_cors$mi), ]

cat("Top modification indices for residual correlations:\n")
cat("Variable 1 | Variable 2 | MI Value | Expected Change\n")
cat("-----------|------------|----------|----------------\n")

# Show top 10 potential residual correlations
top_residuals <- head(residual_cors, 10)
for (i in 1:nrow(top_residuals)) {
  cat(sprintf("%-10s | %-10s | %8.2f | %+.3f\n",
              top_residuals$lhs[i], top_residuals$rhs[i], 
              top_residuals$mi[i], top_residuals$epc[i]))
}

# Common thresholds for MI
cat(sprintf("\nUsing common MI thresholds:\n"))
high_mi <- residual_cors[residual_cors$mi > 10, ]  # Very strong evidence
med_mi <- residual_cors[residual_cors$mi > 6.64 & residual_cors$mi <= 10, ]  # Strong evidence  
low_mi <- residual_cors[residual_cors$mi > 3.84 & residual_cors$mi <= 6.64, ]  # Moderate evidence

cat(sprintf("- High MI (>10): %d correlations\n", nrow(high_mi)))
cat(sprintf("- Medium MI (6.64-10): %d correlations\n", nrow(med_mi)))
cat(sprintf("- Low MI (3.84-6.64): %d correlations\n", nrow(low_mi)))

# Theoretical considerations for each high MI correlation
cat("\n=== STEP 2: THEORETICAL EVALUATION ===\n")
if (nrow(high_mi) > 0) {
  cat("Evaluating high MI residual correlations for theoretical justification:\n\n")
  
  for (i in 1:nrow(high_mi)) {
    var1 <- high_mi$lhs[i]
    var2 <- high_mi$rhs[i]
    mi_val <- high_mi$mi[i]
    
    cat(sprintf("• %s ~~ %s (MI = %.2f)\n", var1, var2, mi_val))
    
    # Theoretical evaluation based on variable patterns
    theoretical_reason <- ""
    
    # Check if both variables are from same factor (method effect)
    if ((grepl("^y[1-4]$", var1) && grepl("^y[1-4]$", var2)) ||
        (grepl("^y[5-8]$", var1) && grepl("^y[5-8]$", var2))) {
      theoretical_reason <- "Same construct - potential method effect"
    }
    
    # Check if variables are temporally related (y1-y5, y2-y6, etc.)
    if ((var1 == "y1" && var2 == "y5") || (var1 == "y5" && var2 == "y1") ||
        (var1 == "y2" && var2 == "y6") || (var1 == "y6" && var2 == "y2") ||
        (var1 == "y3" && var2 == "y7") || (var1 == "y7" && var2 == "y3") ||
        (var1 == "y4" && var2 == "y8") || (var1 == "y8" && var2 == "y4")) {
      theoretical_reason <- "Same item measured at different time points"
    }
    
    # Check for item content similarity (speculative)
    if (grepl("^x", var1) && grepl("^x", var2)) {
      theoretical_reason <- "Same construct (industrialization) - potential shared method"
    }
    
    if (theoretical_reason != "") {
      cat(sprintf("  ✅ Theoretical justification: %s\n", theoretical_reason))
    } else {
      cat(sprintf("  ⚠️  No clear theoretical justification - data driven only\n"))
    }
  }
}

cat("\n=== STEP 3: AUTOMATED INCLUSION PROTOTYPE ===\n")

# Function to automatically add theoretically justified residual correlations
auto_add_residuals <- function(model, data, mi_threshold = 10) {
  # Fit initial model
  fit <- sem(model, data = data)
  mi <- modindices(fit)
  
  # Get high MI residual correlations
  candidates <- subset(mi, op == "~~" & lhs != rhs & mi > mi_threshold)
  candidates <- candidates[order(-candidates$mi), ]
  
  if (nrow(candidates) == 0) {
    return(list(model = model, added = character(), justifications = character()))
  }
  
  added_correlations <- character()
  justifications <- character()
  
  for (i in 1:nrow(candidates)) {
    var1 <- candidates$lhs[i]
    var2 <- candidates$rhs[i]
    mi_val <- candidates$mi[i]
    
    # Apply theoretical filters
    justified <- FALSE
    reason <- ""
    
    # Same time point, different constructs - generally not justified
    # Temporal stability (same item, different times) - justified
    if ((var1 == "y1" && var2 == "y5") || (var1 == "y5" && var2 == "y1")) {
      justified <- TRUE
      reason <- "Temporal stability of same item"
    } else if ((var1 == "y2" && var2 == "y6") || (var1 == "y6" && var2 == "y2")) {
      justified <- TRUE
      reason <- "Temporal stability of same item"
    } else if ((var1 == "y3" && var2 == "y7") || (var1 == "y7" && var2 == "y3")) {
      justified <- TRUE
      reason <- "Temporal stability of same item"
    } else if ((var1 == "y4" && var2 == "y8") || (var1 == "y8" && var2 == "y4")) {
      justified <- TRUE
      reason <- "Temporal stability of same item"
    }
    
    # Method effects within same construct and time
    if (grepl("^y[1-4]$", var1) && grepl("^y[1-4]$", var2) && var1 != var2) {
      if (mi_val > 15) {  # Higher threshold for within-construct
        justified <- TRUE
        reason <- "Strong method effect within dem60"
      }
    }
    if (grepl("^y[5-8]$", var1) && grepl("^y[5-8]$", var2) && var1 != var2) {
      if (mi_val > 15) {  # Higher threshold for within-construct  
        justified <- TRUE
        reason <- "Strong method effect within dem65"
      }
    }
    
    if (justified) {
      correlation_syntax <- sprintf("  %s ~~ %s  # %s (MI=%.1f)", var1, var2, reason, mi_val)
      added_correlations <- c(added_correlations, correlation_syntax)
      justifications <- c(justifications, sprintf("%s ~~ %s: %s", var1, var2, reason))
    }
  }
  
  if (length(added_correlations) > 0) {
    # Add residual correlations section to model
    enhanced_model <- paste(model, 
                           "\n  # Auto-detected residual correlations",
                           paste(added_correlations, collapse = "\n"),
                           sep = "\n")
  } else {
    enhanced_model <- model
  }
  
  return(list(
    model = enhanced_model,
    added = added_correlations, 
    justifications = justifications
  ))
}

# Test the function
result <- auto_add_residuals(sem_model, PoliticalDemocracy, mi_threshold = 6.64)

if (length(result$added) > 0) {
  cat("Auto-detection found", length(result$added), "justified residual correlations:\n")
  for (i in seq_along(result$justifications)) {
    cat(sprintf("  %d. %s\n", i, result$justifications[i]))
  }
  
  cat("\nEnhanced model syntax:\n")
  cat(result$model)
  
  # Test the enhanced model
  cat("\n\n=== TESTING ENHANCED MODEL ===\n")
  fit_enhanced <- sem(result$model, data = PoliticalDemocracy)
  enhanced_measures <- fitMeasures(fit_enhanced, c("cfi", "tli", "rmsea", "srmr"))
  original_measures <- fitMeasures(fit_initial, c("cfi", "tli", "rmsea", "srmr"))
  
  cat("Fit comparison:\n")
  cat("Measure | Original | Enhanced | Improvement\n")
  cat("--------|----------|----------|------------\n")
  for (measure in c("cfi", "tli", "rmsea", "srmr")) {
    improvement <- enhanced_measures[measure] - original_measures[measure]
    cat(sprintf("%-7s | %.3f    | %.3f    | %+.3f\n", 
                toupper(measure), original_measures[measure], 
                enhanced_measures[measure], improvement))
  }
} else {
  cat("No theoretically justified residual correlations found.\n")
}

cat("\n=== STEP 4: ETHICAL & PRACTICAL CONSIDERATIONS ===\n")

cat("🤔 **Is it POSSIBLE?** ✅ YES\n")
cat("   - Modification indices can identify needed correlations\n") 
cat("   - Theoretical rules can filter statistically-driven suggestions\n")
cat("   - Automated implementation is technically feasible\n\n")

cat("🧠 **Is it LOGICAL?** ⚠️  COMPLEX\n")
cat("   **Arguments FOR automation:**\n")
cat("   ✅ Reduces manual specification effort\n")
cat("   ✅ Applies consistent theoretical rules\n")
cat("   ✅ Can improve model fit when justified\n")
cat("   ✅ Makes modeling accessible to non-experts\n\n")

cat("   **Arguments AGAINST automation:**\n")
cat("   ❌ Theoretical justification requires domain expertise\n")
cat("   ❌ Risk of overfitting to sample-specific patterns\n") 
cat("   ❌ May mask deeper model specification issues\n")
cat("   ❌ Reduces researcher engagement with model assumptions\n")
cat("   ❌ Could lead to 'black box' modeling\n\n")

cat("🎯 **RECOMMENDATION: Hybrid Approach**\n")
cat("   1. **Auto-DETECT** potential residual correlations\n")
cat("   2. **Flag** them with theoretical justifications\n")
cat("   3. **Require user approval** before inclusion\n")
cat("   4. **Document** all decisions transparently\n")
cat("   5. **Warn** about overfitting risks\n\n")

cat("💡 **Best Practice Implementation:**\n")
cat("   • Use modification indices as **suggestions**, not **commands**\n")
cat("   • Apply strict theoretical filters (temporal stability, method effects)\n") 
cat("   • Require explicit user consent for each correlation\n")
cat("   • Cross-validate results on independent samples when possible\n")
cat("   • Report all specification searches transparently\n\n")

cat("🚨 **semScreenR Philosophy:**\n")
cat("   Current focus on measurement quality (item removal) is more defensible\n")
cat("   because weak loadings have clear statistical/theoretical meaning.\n")
cat("   Residual correlations are often post-hoc fixes that need careful\n")
cat("   theoretical justification to avoid capitalizing on chance.\n")

cat("\n🏁 Analysis Complete! 🏁\n")
