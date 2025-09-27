test_that("triage_rules presets are valid", {
  cfg <- triage_rules("balanced")
  expect_true(is.list(cfg$limits))
  expect_true(is.list(cfg$gates))
  expect_true(is.list(cfg$careless))
  expect_equal(cfg$limits$min_items_per_factor, 3L)
})

test_that("careless_signals returns expected columns", {
  set.seed(123)
  df <- data.frame(x1 = rnorm(30), x2 = rnorm(30), x3 = rnorm(30))
  sig <- careless_signals(df, items = c("x1","x2","x3"))
  expect_true(all(c("row_id","longstring","low_irv","mahal","too_fast") %in% names(sig)))
  expect_equal(nrow(sig), nrow(df))
})

test_that("triage_plan creates valid object", {
  df <- data.frame(id = 1:10, x1 = rnorm(10), x2 = rnorm(10))
  model <- "f1 =~ x1 + x2"
  cfg <- triage_rules("balanced")
  plan <- triage_plan(df, model, id_cols = "id", protected = "x1", config = cfg)
  expect_s3_class(plan, "semScreen_plan")
  expect_equal(plan$ids, "id")
  expect_equal(plan$protected, "x1")
  expect_equal(plan$model, model)
  expect_equal(plan$config, cfg)
})

test_that("triage_apply returns expected structure", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  data("HolzingerSwineford1939", package = "lavaan")
  dat <- HolzingerSwineford1939
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  cfg  <- triage_rules("balanced")
  plan <- triage_plan(dat, model, id_cols = "id", protected = character(), config = cfg)
  res  <- triage_apply(dat, model, plan)

  expect_s3_class(res, "semScreen_result")
  expect_true(is.list(res$fit))
  expect_true(all(c("pre","post") %in% names(res$fit)))
  expect_true(is.data.frame(res$data_final))
})

test_that("input validation works correctly", {
  df <- data.frame(x1 = rnorm(10), x2 = rnorm(10))
  model <- "f1 =~ x1 + x2"
  cfg <- triage_rules("balanced")
  plan <- triage_plan(df, model, config = cfg)
  # Test invalid data
  expect_error(triage_apply("not a dataframe", model, plan), "dat must be a data.frame")
  # Test invalid model
  expect_error(triage_apply(df, c("model1", "model2"), plan), "model must be a character string")
  # Test invalid plan
  expect_error(triage_apply(df, model, "not a plan"), "plan must be a semScreen_plan object")
  # Test empty data
  expect_error(triage_apply(df[0,], model, plan), "dat cannot be empty")
})

test_that("missing variables are detected", {
  df <- data.frame(x1 = rnorm(10), x2 = rnorm(10))
  model <- "f1 =~ x1 + x2 + x3"  # x3 doesn't exist
  cfg <- triage_rules("balanced")
  plan <- triage_plan(df, model, config = cfg)
  expect_error(triage_apply(df, model, plan), "Variables not found in data: x3")
})

test_that("model syntax modification works correctly", {
  # Test normal case
  model <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6"
  result <- semScreenR:::modify_model_syntax(model, "x2")
  expect_true(grepl("visual =~ x1 \\+ x3", result))
  expect_true(grepl("textual =~ x4 \\+ x5 \\+ x6", result))
  # Test edge case - removing last indicator from factor
  model_single <- "factor1 =~ x1"
  result_null <- semScreenR:::modify_model_syntax(model_single, "x1")
  expect_null(result_null)
  # Test with coefficient specifications
  model_coef <- "visual =~ 0.5*x1 + x2 + 1.2*x3"
  result_coef <- semScreenR:::modify_model_syntax(model_coef, "x2")
  expect_true(grepl("visual =~ 0.5\\*x1 \\+ 1.2\\*x3", result_coef))
})

test_that("variable extraction works correctly", {
  model <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nvisual ~ textual"
  vars <- semScreenR:::extract_model_variables(model)
  expected <- c("x1", "x2", "x3", "x4", "x5", "x6", "visual", "textual")
  expect_true(all(expected %in% vars))
  # Test with coefficient specifications
  model_coef <- "visual =~ 0.5*x1 + x2 + 1.2*x3"
  vars_coef <- semScreenR:::extract_model_variables(model_coef)
  expect_true(all(c("x1", "x2", "x3") %in% vars_coef))
  expect_false("0.5" %in% vars_coef)
})

test_that("k-fold validation handles edge cases", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  # Small dataset
  set.seed(123)
  small_dat <- data.frame(
    x1 = rnorm(20), x2 = rnorm(20), x3 = rnorm(20),
    x4 = rnorm(20), x5 = rnorm(20), x6 = rnorm(20)
  )
  model <- "f1 =~ x1 + x2 + x3\nf2 =~ x4 + x5 + x6"
  cfg <- triage_rules("balanced")
  plan <- triage_plan(small_dat, model, config = cfg)
  # Should handle small sample size gracefully
  expect_no_error({
    res <- triage_apply(small_dat, model, plan)
  })
})

test_that("perfect fit guard and other safeguards work", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  # Test with minimum N enforcement
  set.seed(123)
  tiny_dat <- data.frame(
    x1 = rnorm(50), x2 = rnorm(50), x3 = rnorm(50)
  )
  model <- "f1 =~ x1 + x2 + x3"
  cfg <- triage_rules("conservative")  # Has min_n = 300
  plan <- triage_plan(tiny_dat, model, config = cfg)
  res <- triage_apply(tiny_dat, model, plan)
  expect_true(grepl("insufficient_n", res$status))
})