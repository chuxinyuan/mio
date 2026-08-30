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
    flat_commission = 0,
    commission_rate = 0,
    stamp_duty = 0
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
    flat_commission = 3.5,
    commission_rate = 0.0003,
    stamp_duty = 0.0005
  )
  expect_true(sum(as.numeric(res$pos_qty) > 0) > 0, info = "应发生过建仓")
})

test_that("simulate 费用确定性手算（固定佣金+万3费率+卖出印花税，预留费率现金非负）", {
  n = 6
  dates = as.Date("2023-01-01") + 0:(n - 1)
  close_mat = zoo(matrix(10, nrow = n, ncol = 1), order.by = dates)
  colnames(close_mat) = "A"
  open_mat = close_mat
  entry = zoo(matrix(c(0, 1, 0, 0, 0, 0), nrow = n, ncol = 1), order.by = dates)
  exit = zoo(matrix(c(0, 0, 0, 0, 1, 0), nrow = n, ncol = 1), order.by = dates)
  favor = zoo(matrix(0.5, nrow = n, ncol = 1), order.by = dates)
  colnames(entry) = colnames(exit) = colnames(favor) = "A"
  res = simulate(
    open_mat, close_mat, entry, exit, favor,
    max_lookback = 2, max_assets = 1, starting_cash = 100000,
    slip_factor = 0, flat_commission = 3.5, commission_rate = 0.0003, stamp_duty = 0.0005
  )
  # 买入（预留费率后）：qty = floor((100000 - 3.5) / (1 * 10 * 1.0003)) = 9996
  qty = as.numeric(res$pos_qty[3, 1])
  expect_equal(qty, 9996)
  expect_true(all(as.numeric(res$cash) >= 0), info = "持仓期现金应非负（预留了费率佣金）")
  # 期末现金精确手算：买入额 9996*10=99960，佣金 3.5+99960*0.0003，卖出印花税 99960*0.0005
  amount = 99960
  expected = 100000 - amount - (3.5 + amount * 0.0003) + amount - (3.5 + amount * 0.0003) - amount * 0.0005
  expect_equal(as.numeric(tail(res$cash, 1)), expected, tolerance = 1e-6)
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

test_that("prepare_data 停牌缺口 locf 填充、未上市保留", {
  b = synthetic_bars(c("A", "B"), n_days = 40)
  dates = unique(b$date)
  gap = dates[15:20]
  pre = dates[1:5]
  b = b[!(symbol == "B" & date %in% c(gap, pre))]   # B 前 5 日未上市、中间停牌 6 日
  d = prepare_data(b)
  close = d$close[, "B"]
  idx = index(close)
  gap_idx = which(idx %in% gap)
  pre_idx = which(idx %in% pre)
  expect_true(all(is.na(close[pre_idx])), info = "未上市期应保持 NA")
  expect_true(all(!is.na(close[gap_idx])), info = "停牌缺口应被填充")
  expect_equal(as.numeric(close[gap_idx[1]]), as.numeric(close[gap_idx[1] - 1]), info = "填前值")
})

test_that("evaluate 含停牌 NA 的年份不报错且日期对齐", {
  b = synthetic_bars(c("A", "B", "C"), n_days = 80, start = "2022-01-01")
  gap = unique(b$date)[20:30]
  b = b[!(symbol == "A" & date %in% gap)]            # A 停牌一段
  d = prepare_data(b)
  d$return = make_return(d$close)
  res = evaluate(
    d,
    c(n1 = 5, n_fact = 1.6, n_sharpe = 6, sh_thresh = 0.5),
    year = 2022,
    transform = FALSE,
    return_data = TRUE
  )
  expect_equal(length(res$equity), length(res$dates), info = "equity 与日期等长")
  expect_false(all(is.na(res$equity)), info = "不应整体为 NA")
})
