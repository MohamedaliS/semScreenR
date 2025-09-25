test_that('triage_rules presets set caps and validation', {
  cfg <- triage_rules('balanced')
  expect_true(is.list(cfg$limits))
  expect_true(cfg$gates$use_validation)
  expect_equal(cfg$limits$min_items_per_factor, 3)
})

test_that('careless_signals returns required columns', {
  set.seed(1)
  df <- data.frame(x1=rnorm(20), x2=rnorm(20), x3=rnorm(20))
  sig <- careless_signals(df, items=c('x1','x2','x3'))
  expect_true(all(c('row_id','longstring','low_irv','mahal','too_fast') %in% names(sig)))
})
