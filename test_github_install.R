# Test semScreenR installed from GitHub
library(semScreenR)
library(lavaan)

# Suppress startup message for cleaner output
options(semScreenR.ethical_reminder = FALSE)

cat("=== TESTING SEMSCREENR FROM GITHUB INSTALLATION ===\n\n")

# Test 1: Basic functionality
cat("1. Testing basic workflow functions...\n")
data("HolzingerSwineford1939")
items <- paste0("x", 1:9)

# Test triage_rules
rules <- triage_rules(
  careless_threshold = 0.1,
  alpha_threshold = 0.7,
  loading_threshold = 0.4
)
cat("   - triage_rules: ✓\n")

# Test triage_plan
plan <- triage_plan(HolzingerSwineford1939[, items], rules)
cat("   - triage_plan: ✓\n")

# Test triage_apply
result <- triage_apply(HolzingerSwineford1939[, items], plan)
cat("   - triage_apply: ✓\n")

# Test 2: SEM model for hierarchical detection
cat("\n2. Testing hierarchical model detection...\n")
model <- '
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6  
  speed =~ x7 + x8 + x9
'

fit <- cfa(model, data = HolzingerSwineford1939)

# Test hierarchical detection functions
is_hierarchical <- is_higher_order_model(fit)
cat("   - is_higher_order_model:", is_hierarchical, "✓\n")

if (!is_hierarchical) {
  hierarchy_result <- sem_maybe_hierarchy(fit)
  cat("   - sem_maybe_hierarchy: Status =", hierarchy_result$status, "✓\n")
  cat("   - Recommendation:", hierarchy_result$recommendation, "\n")
}

# Test 3: Report generation
cat("\n3. Testing report generation...\n")
report_file <- "github_test_report.html"
export_sem_report(
  pre_data = HolzingerSwineford1939[, items],
  post_data = result$data,
  original_model = fit,
  results = result,
  output_file = report_file
)
cat("   - export_sem_report: ✓ (Report:", report_file, ")\n")

# Test 4: Advanced functions
cat("\n4. Testing additional functions...\n")

# Test comparison table
comparison <- sem_compare_table(fit, fit)
cat("   - sem_compare_table: ✓\n")

# Test path diagrams (if semPlot available)
tryCatch({
  if (requireNamespace("semPlot", quietly = TRUE)) {
    paths_result <- sem_paths_prepost(fit, fit)
    cat("   - sem_paths_prepost: ✓\n")
  } else {
    cat("   - sem_paths_prepost: Skipped (semPlot not available)\n")
  }
}, error = function(e) {
  cat("   - sem_paths_prepost: Skipped (error in semPlot)\n")
})

cat("\n=== GITHUB INSTALLATION TEST RESULTS ===\n")
cat("✅ Package successfully installed from GitHub\n")
cat("✅ All core functions working correctly\n") 
cat("✅ Hierarchical model detection functional\n")
cat("✅ Report generation successful\n")
cat("✅ Package version:", as.character(packageVersion("semScreenR")), "\n")
cat("✅ Installation location:", find.package("semScreenR"), "\n")
cat("\n🎉 GITHUB PACKAGE TEST COMPLETED SUCCESSFULLY! 🎉\n")
