# tests/testthat/test-indicators.R — 信号生成测试

build = \(syms, n = 80) {
  bars = synthetic_bars(syms, n_days = n)
  data = prepare_data(bars)
  data$return = make_return(data$close)
  data
}

test_that("make_signals 维度与取值", {
  data = build(c("A", "B", "C"), 60)
  sig = make_signals(data$close, data$return, 5, 8, 6, 0.5)
  expect_equal(dim(sig$entry), dim(data$close))
  expect_equal(dim(sig$exit), dim(data$close))
  expect_equal(dim(sig$favor), dim(data$close))
  expect_true(all(as.numeric(sig$entry)[!is.na(as.numeric(sig$entry))] %in% c(0, 1)))
  expect_true(all(as.numeric(sig$exit)[!is.na(as.numeric(sig$exit))] %in% c(0, 1)))
})

test_that("A3 回归：exit 存在非 0 主动出场信号", {
  data = build(c("A", "B", "C", "D"), 80)
  sig = make_signals(data$close, data$return, 5, 8, 6, 0.5)
  expect_true(sum(sig$entry > 0, na.rm = TRUE) > 0)
  expect_true(sum(sig$exit > 0, na.rm = TRUE) > 0)
})

test_that("latest_signals 按 favor 降序且含 entry/favor", {
  data = build(c("A", "B", "C", "D"), 80)
  ls = latest_signals(data, 5, 8, 6, 0.5)
  expect_named(ls, c("symbol", "entry", "favor"))
  expect_equal(nrow(ls), ncol(data$close))
  f = ls$favor[!is.na(ls$favor)]
  expect_false(is.unsorted(-f))
})
