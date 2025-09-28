#!/usr/bin/env Rscript
# Test the package improvements

library(semScreenR)
options(semScreenR.ethical_reminder = FALSE)

# Test hierarchy detection
model <- '
  # First-order factors
  anxiety =~ anx1 + anx2 + anx3
  depression =~ dep1 + dep2 + dep3
  stress =~ str1 + str2 + str3
  
  # Second-order factor
  distress =~ anxiety + depression + stress
'

hierarchy_info <- semScreenR:::detect_model_hierarchy(model)
cat("=== Hierarchy Detection Test ===\n")
cat("Is hierarchical:", hierarchy_info$is_hierarchical, "\n")
cat("Levels:", hierarchy_info$levels, "\n") 
cat("Higher-order factors:", paste(hierarchy_info$higher_order, collapse = ", "), "\n")
cat("First-order factors:", paste(hierarchy_info$first_order, collapse = ", "), "\n")

# Test formatting functions
cat("\n=== Formatting Test ===\n")
test_values <- list(cfi = 0.934, tli = 0.912, rmsea = 0.087, srmr = 0.045)
for (measure in names(test_values)) {
  formatted <- semScreenR:::format_fit_measure(test_values[[measure]], measure)
  cat(sprintf("%s: %.3f -> %s\n", toupper(measure), test_values[[measure]], formatted))
}

# Test change formatting  
cat("\n=== Change Formatting Test ===\n")
changes <- list(cfi = 0.045, tli = -0.023, rmsea = -0.012, srmr = 0.001)
for (measure in names(changes)) {
  formatted <- semScreenR:::format_change(changes[[measure]], measure) 
  cat(sprintf("%s change: %.3f -> %s\n", toupper(measure), changes[[measure]], formatted))
}

cat("\n=== Test Complete ===\n")
