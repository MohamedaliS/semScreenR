# tests/testthat/test-utils.R

test_that("NULL coalescing operator works", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(NA %||% "default", NA)
  expect_equal(0 %||% "default", 0)
})

test_that("package availability check works", {
  expect_true(semScreenR:::.pkg_available("stats"))
  expect_false(semScreenR:::.pkg_available("nonexistent_package_xyz"))
})

test_that("safe fit handles errors gracefully", {
  # Invalid model syntax
  result <- semScreenR:::.safe_fit("invalid syntax", data.frame(x = 1:10))
  expect_null(result)
  
  # Empty data
  skip_if_not_installed("lavaan")
  result <- semScreenR:::.safe_fit("f =~ x + y", data.frame())
  expect_null(result)
})

test_that("fit measures extraction works", {
  expect_equal(semScreenR:::.fit_measures(NULL), list())
  
  skip_if_not_installed("lavaan")
  library(lavaan)
  data("HolzingerSwineford1939", package = "lavaan")
  model <- "visual =~ x1 + x2 + x3"
  fit <- semScreenR:::.safe_fit(model, HolzingerSwineford1939)
  
  if (!is.null(fit)) {
    measures <- semScreenR:::.fit_measures(fit)
    expect_true(is.list(measures))
    expect_true("cfi" %in% names(measures))
    expect_true("rmsea" %in% names(measures))
  }
})

# tests/testthat/test-careless.R

test_that("careless signals handles various data types", {
  set.seed(42)
  
  # Normal data
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    rt = runif(100, 10, 100)
  )
  
  signals <- careless_signals(df, items = c("x1", "x2", "x3"), rt_col = "rt")
  expect_equal(nrow(signals), 100)
  expect_true(all(c("longstring", "low_irv", "mahal", "too_fast") %in% names(signals)))
  
  # Test with some straight-lining
  df_straight <- df
  df_straight[1:5, c("x1", "x2", "x3")] <- 1  # Same response
  
  signals_straight <- careless_signals(df_straight, items = c("x1", "x2", "x3"))
  expect_true(any(signals_straight$longstring))
  expect_true(any(signals_straight$low_irv))
})

test_that("careless signals handles missing RT column gracefully", {
  df <- data.frame(x1 = rnorm(50), x2 = rnorm(50), x3 = rnorm(50))
  
  # Without RT column
  signals <- careless_signals(df, items = c("x1", "x2", "x3"))
  expect_true(all(!signals$too_fast))  # Should all be FALSE
  
  # With non-existent RT column
  signals2 <- careless_signals(df, items = c("x1", "x2", "x3"), rt_col = "nonexistent")
  expect_true(all(!signals2$too_fast))
})

test_that("careless signals parameter validation", {
  df <- data.frame(x1 = rnorm(10), x2 = rnorm(10))
  
  # Missing items
  expect_error(careless_signals(df, items = c("x1", "missing_var")))
  
  # Empty items
  expect_error(careless_signals(df, items = character(0)))
})

# tests/testthat/test-integration.R

test_that("full workflow integration test", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  
  # Create synthetic data with known issues
  set.seed(123)
  n <- 200
  
  # Good indicators
  f1 <- rnorm(n)
  f2 <- rnorm(n)
  
  data_synth <- data.frame(
    id = 1:n,
    # Strong indicators
    x1 = f1 + rnorm(n, 0, 0.3),
    x2 = f1 + rnorm(n, 0, 0.3),
    x3 = f1 + rnorm(n, 0, 0.3),
    # Weak indicator (should be removed)
    x4 = f1 * 0.1 + rnorm(n, 0, 1),
    # Second factor
    y1 = f2 + rnorm(n, 0, 0.3),
    y2 = f2 + rnorm(n, 0, 0.3),
    y3 = f2 + rnorm(n, 0, 0.3)
  )
  
  model <- '
    factor1 =~ x1 + x2 + x3 + x4
    factor2 =~ y1 + y2 + y3
  '
  
  cfg <- triage_rules("balanced")
  cfg$thresholds$loading_min <- 0.3  # Make it likely x4 will be removed
  
  plan <- triage_plan(data_synth, model, id_cols = "id", config = cfg)
  result <- triage_apply(data_synth, model, plan)
  
  expect_s3_class(result, "semScreen_result")
  expect_true(result$status %in% c("completed_no_changes", "completed_with_changes"))
  expect_true(nrow(result$data_final) <= nrow(data_synth))
  
  # Check that fit measures are present
  expect_true(length(result$fit$pre) > 0)
  expect_true(length(result$fit$post) > 0)
})

test_that("workflow handles edge cases gracefully", {
  skip_if_not_installed("lavaan")
  
  # Test with minimum viable data
  df_min <- data.frame(
    x1 = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1),
    x2 = c(2, 3, 1, 2, 3, 1, 2, 3, 1, 2),
    x3 = c(3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
  )
  
  model_min <- "f1 =~ x1 + x2 + x3"
  cfg <- triage_rules("aggressive")  # Most permissive
  cfg$limits$min_n <- 5  # Lower minimum
  
  plan <- triage_plan(df_min, model_min, config = cfg)
  
  expect_no_error({
    result <- triage_apply(df_min, model_min, plan)
  })
  
  expect_s3_class(result, "semScreen_result")
})