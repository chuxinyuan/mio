# tests/testthat/test-views.R — 视图层组装函数测试（离线）

# 代码→名称映射（names 与 codes 等长，默认 2 只）
mk_map = \(codes = c("600000", "600519"), names = c("浦发银行", "贵州茅台")) {
  data.table(code = codes, name = names)
}

test_that("sym_map 代码→名称映射", {
  m = sym_map(mk_map())
  expect_equal(m$symbol, c("600000", "600519"))
  expect_equal(m$name, c("浦发银行", "贵州茅台"))
})

test_that("orders_view 合并名称、方向/状态中文化，空表保留列", {
  con = new_test_con()
  save_order(con, "2023-01-01 09:00:00", "600000", "buy", 100, 10)
  d = orders_view(con, sym_map(mk_map()))
  expect_equal(d$name, "浦发银行")
  expect_equal(d$side, "买入")
  expect_equal(d$status, "未成交")
  econ = new_test_con()
  e = orders_view(econ, sym_map(mk_map()))
  expect_equal(nrow(e), 0)
  expect_true("name" %in% names(e))
  dbDisconnect(econ)
  dbDisconnect(con)
})

test_that("fills_view 计算现价与回报（先舍入价格）", {
  con = new_test_con()
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  px = last_close(con, "600000")
  id = save_order(con, "2023-01-01 09:00:00", "600000", "buy", 100, px)
  save_fill(con, id, "2023-01-02 09:00:00", px, 100)
  set_order_status(con, id, "filled")
  d = fills_view(con, sym_map(mk_map()))
  expect_equal(d$side, "买入")
  expect_equal(d$price, round2(px, 2))
  expect_equal(d$cur_price, round2(px, 2))
  expect_equal(d$ret, 0)
  expect_equal(d$name, "浦发银行")
  dbDisconnect(con)
})

test_that("positions_view 四项两位小数", {
  con = new_test_con()
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  px = last_close(con, "600000")
  upsert_position(con, "600000", 100, 0, px)
  d = positions_view(con, sym_map(mk_map()))
  expect_equal(d$name, "浦发银行")
  expect_equal(d$avg_cost, round2(px, 2))
  expect_equal(d$price, round2(px, 2))
  expect_equal(d$market_value, round2(100 * px, 2))
  expect_equal(d$pnl, 0)
  dbDisconnect(con)
})

test_that("视图 price_map 实时价驱动 现价/市值/浮盈亏", {
  con = new_test_con()
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  px = last_close(con, "600000")
  upsert_position(con, "600000", 100, 0, px)
  price_hi = data.table(symbol = "600000", price = round2(px * 1.05, 2))
  d = positions_view(con, sym_map(mk_map()), price_map = price_hi)
  expect_equal(d$price, price_hi$price)
  expect_equal(d$market_value, round2(100 * price_hi$price, 2))
  expect_equal(d$pnl, round2((price_hi$price - round2(px, 2)) * 100, 2))
  k = account_kpis_view(con, price_map = price_hi)
  expect_equal(k$equity, 100000 + 100 * price_hi$price)   # upsert 不扣现金，权益 = 现金 + 市值
  # fills_view 现价也随 price_map
  oid = save_order(con, "2023-01-01 09:00:00", "600000", "buy", 100, px)
  save_fill(con, oid, "2023-01-02 09:00:00", px, 100)
  set_order_status(con, oid, "filled")
  f = fills_view(con, sym_map(mk_map()), price_map = price_hi)
  expect_equal(f$cur_price, price_hi$price)
  dbDisconnect(con)
})

test_that("account_kpis_view 返回 KPI 字段", {
  con = new_test_con()
  k = account_kpis_view(con)
  expect_equal(k$cash, load_settings(con)$starting_cash)
  expect_equal(k$equity, k$cash)
  expect_equal(k$n_pos, 0)
  expect_equal(k$pnl, 0)
  dbDisconnect(con)
})

test_that("equity_view 快照不足 2 行时补齐可渲染", {
  con = new_test_con()
  s = equity_view(con)
  expect_gte(nrow(s), 2)
  dbDisconnect(con)
})

test_that("log_view 级别/模块中文化", {
  con = new_test_con()
  write_log(con, "测试", "info", "fetch")
  d = log_view(con)
  expect_equal(d$level, "信息")
  expect_equal(d$module, "数据获取")
  dbDisconnect(con)
})

test_that("signals_view 按 favor 降序、评分两位小数并截取 max_assets", {
  bars = synthetic_bars(c("600000", "600519", "601318"), n_days = 80)
  d = prepare_data(bars)
  d$return = make_return(d$close)
  s = signals_view(
    d,
    sym_map(mk_map(c("600000", "600519", "601318"), c("浦发银行", "贵州茅台", "中国平安"))),
    5, 8, 6, 0.5, 2
  )
  expect_lte(nrow(s), 2)
  expect_true(all(abs(s$favor - round(s$favor, 2)) < 1e-9, na.rm = TRUE))
  if (nrow(s) > 1) {
    expect_true(all(diff(s$favor) <= 0), info = "favor 应降序")
  }
})

test_that("kline_view 含均线且按日期筛选", {
  con = new_test_con()
  b = synthetic_bars("600000", 40)
  replace_bars(con, "600000", b)
  d = kline_view(con, "600000")
  expect_true(all(c("ma5", "ma10", "ma20") %in% names(d)))
  rng = unique(b$date)
  d2 = kline_view(con, "600000", rng = c(rng[1], rng[10]))
  expect_lte(nrow(d2), nrow(d))
  dbDisconnect(con)
})

test_that("realtime_view 单位换算与两位小数", {
  d = data.table(
    code = "600000", name = "浦发银行",
    price = 10.5, open = 10.3, high = 10.8, low = 10.2,
    volume = 1e6, amount = 2e9, change = 1.234, pct = 3.456
  )
  r = realtime_view(d)
  expect_equal(r$volume, 100)       # 1e6 手 → 100 万手
  expect_equal(r$amount, 20)        # 2e9 元 → 20 亿
  expect_equal(r$change, 1.23)
  expect_equal(r$pct, 3.46)
  expect_equal(nrow(realtime_view(data.table())), 0)
})
