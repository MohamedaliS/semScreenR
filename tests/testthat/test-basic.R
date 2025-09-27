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
