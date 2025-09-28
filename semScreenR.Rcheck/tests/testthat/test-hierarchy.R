test_that("hierarchical model detection works correctly", {
  skip_if_not_installed("lavaan")
  
  # Test regular (non-hierarchical) model
  regular_model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
  "
  expect_false(is_higher_order_model(regular_model))
  
  # Test higher-order model
  hierarchical_model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
    g =~ visual + textual + speed
  "
  expect_true(is_higher_order_model(hierarchical_model))
  
  # Test invalid syntax
  expect_false(is_higher_order_model("invalid syntax"))
})

test_that("lower-order equivalent generation works correctly", {
  skip_if_not_installed("lavaan")
  
  hierarchical_model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
    g =~ visual + textual + speed
  "
  
  lower_order <- lower_order_equivalent(hierarchical_model)
  
  # Lower-order model should not be hierarchical
  expect_false(is_higher_order_model(lower_order))
  
  # Should contain the first-order factors
  expect_true(grepl("visual =~ x1 \\+ x2 \\+ x3", lower_order))
  expect_true(grepl("textual =~ x4 \\+ x5 \\+ x6", lower_order))
  expect_true(grepl("speed =~ x7 \\+ x8 \\+ x9", lower_order))
  
  # Should not contain the higher-order factor
  expect_false(grepl("g =~", lower_order))
})

test_that("sem_maybe_hierarchy returns NULL for non-hierarchical models", {
  skip_if_not_installed("lavaan")
  
  # Use HolzingerSwineford1939 data for testing
  data(HolzingerSwineford1939, package = "lavaan")
  dat <- HolzingerSwineford1939
  
  regular_model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
  "
  
  result <- sem_maybe_hierarchy(dat, regular_model, 
                               config = triage_rules("balanced"), 
                               estimator = "MLR", 
                               validate = FALSE)
  
  expect_null(result)
})

test_that("sem_maybe_hierarchy works with hierarchical models", {
  skip_if_not_installed("lavaan")
  
  # Use HolzingerSwineford1939 data for testing
  data(HolzingerSwineford1939, package = "lavaan")
  dat <- HolzingerSwineford1939
  
  hierarchical_model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
    g =~ visual + textual + speed
  "
  
  result <- sem_maybe_hierarchy(dat, hierarchical_model,
                               config = triage_rules("balanced"),
                               estimator = "MLR",
                               validate = FALSE)
  
  expect_false(is.null(result))
  expect_true("status" %in% names(result))
  expect_true("recommendation" %in% names(result))
  expect_true("comparison_table" %in% names(result))
  
  # If comparison completes successfully
  if (result$status == "comparison_complete") {
    expect_true("preferred_model" %in% names(result))
    expect_true(result$preferred_model %in% c("lower_order", "higher_order"))
    expect_true(is.character(result$recommendation))
  }
})

test_that("hierarchical functions handle edge cases gracefully", {
  skip_if_not_installed("lavaan")
  
  # Test with invalid syntax
  expect_error(lower_order_equivalent("invalid syntax"))
  
  # Test with minimal data
  minimal_dat <- data.frame(
    x1 = c(1, 2, 3), x2 = c(2, 3, 1), x3 = c(3, 1, 2),
    x4 = c(1, 3, 2), x5 = c(2, 1, 3), x6 = c(3, 2, 1)
  )
  
  hierarchical_model <- "
    f1 =~ x1 + x2 + x3
    f2 =~ x4 + x5 + x6
    g =~ f1 + f2
  "
  
  # Should handle gracefully even if models don't converge
  result <- sem_maybe_hierarchy(minimal_dat, hierarchical_model,
                               config = triage_rules("balanced"),
                               estimator = "ML",
                               validate = FALSE)
  
  # Should return a result (even if it indicates failure)
  expect_false(is.null(result))
  expect_true("status" %in% names(result))
})
