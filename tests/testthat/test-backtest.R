# tests/testthat/test-backtest.R — 回测引擎测试（simulate 不变量）

build = \(syms, n = 60) {
  bars = synthetic_bars(syms, n_days = n)
  data = prepare_data(bars)
  data$return = make_return(data$close)
  data
}

test_that("prepare_data 宽矩阵维度", {
  data = build(c("A", "B", "C"), 40)
  expect_equal(nrow(data$close), 40)
  expect_equal(ncol(data$close), 3)
  expect_equal(names(data$close), c("A", "B", "C"))
})

test_that("make_return 首行全 NA、行数一致", {
  data = build(c("A", "B"), 40)
  expect_true(all(is.na(data$return[1, ])))
  expect_equal(nrow(data$return), nrow(data$close))
})

test_that("simulate 基本不变量：无负持仓/现金、equity 有限且非负", {
  data = build("A", 40)
  sig = make_signals(data$close, data$return, 3, 5, 4, 0.3)
  res = simulate(
    open_mat = data$open,
    close_mat = data$close,
    entry = sig$entry,
    exit = sig$exit,
    favor = sig$favor,
    max_lookback = 6,
    max_assets = 2,
    starting_cash = 100000,
    slip_factor = 0,
    spread_adjust = 0,
    flat_commission = 0,
    per_share_commission = 0
  )
  qty = as.numeric(res$pos_qty)
  cash = as.numeric(res$cash)
  eq = res$equity[!is.na(res$equity)]
  expect_true(length(eq) > 0)
  expect_true(all(qty >= 0), info = "不应出现做空（entry 仅 0/1）")
  expect_true(all(cash >= 0))
  expect_true(all(is.finite(eq)))
  expect_true(all(eq > 0))
})

test_that("simulate 收到信号会产生交易", {
  data = build(c("A", "B", "C"), 80)
  sig = make_signals(data$close, data$return, 5, 8, 6, 0.5)
  res = simulate(
    open_mat = data$open,
    close_mat = data$close,
    entry = sig$entry,
    exit = sig$exit,
    favor = sig$favor,
    max_lookback = 9,
    max_assets = 3,
    starting_cash = 100000,
    slip_factor = 0.001,
    spread_adjust = 0.01,
    flat_commission = 3.5,
    per_share_commission = 0
  )
  expect_true(sum(as.numeric(res$pos_qty) > 0) > 0, info = "应发生过建仓")
})

test_that("calc_metrics 已知单调上行序列", {
  v = 100 * (1.001)^(1:100)
  m = calc_metrics(v)
  expect_true(m$total_return > 0)
  expect_true(m$annualized > 0)
  expect_true(m$max_drawdown <= 0)
  expect_equal(m$n_days, 99)
})

test_that("evaluate 返回标量（transform=FALSE）", {
  data = build(c("A", "B", "C"), 60)
  out = evaluate(
    data,
    c(n1 = 5, n_fact = 1.6, n_sharpe = 6, sh_thresh = 0.5),
    year = 2023,
    transform = FALSE
  )
  expect_true(is.numeric(out) && length(out) == 1)
})
