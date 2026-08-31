# tests/testthat/test-account.R — 模拟账户与撮合测试

test_that("place_order 数量校验：非 100 整数倍 / 非正", {
  con = new_test_con()
  expect_false(place_order(con, "600000", "buy", 50, 10)$ok)
  expect_false(place_order(con, "600000", "buy", 0, 10)$ok)
  expect_false(place_order(con, "600000", "buy", -100, 10)$ok)
  dbDisconnect(con)
})

test_that("A1 回归：卖出校验不被其它持仓干扰", {
  con = new_test_con()
  upsert_position(con, "600000", 100, 0, 10)    # 目标可卖 0
  upsert_position(con, "600519", 100, 100, 1500) # 其它持仓可卖充足
  r = place_order(con, "600000", "sell", 100, NA_real_)
  expect_false(r$ok)
  expect_match(r$msg, "可卖")
  r2 = place_order(con, "600519", "sell", 100, NA_real_)
  expect_true(r2$ok)
  dbDisconnect(con)
})

test_that("place_order 现金不足拒绝", {
  con = new_test_con()
  r = place_order(con, "600000", "buy", 100, 10000)   # 约 100 万 > 初始 10 万
  expect_false(r$ok)
  expect_match(r$msg, "现金")
  dbDisconnect(con)
})

test_that("place_order 涨跌停校验", {
  con = new_test_con()
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  prev = last_close(con, "600000")
  up = round(prev * 1.10, 2)
  expect_false(place_order(con, "600000", "buy", 100, up + 0.01)$ok)
  dn = round(prev * 0.90, 2)
  upsert_position(con, "600000", 100, 100, prev)
  r = place_order(con, "600000", "sell", 100, dn - 0.01)
  expect_false(r$ok)
  expect_match(r$msg, "跌停")
  dbDisconnect(con)
})

test_that("match_orders 单笔买入手算断言", {
  con = new_test_con()
  settings = load_settings(con)
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  px = last_close(con, "600000")
  qty = 100
  id = save_order(con, "2023-01-01 09:00:00", "600000", "buy", qty, px)
  match_orders(con, setNames(c(px), "600000"), ts = "2023-01-02 09:00:00")

  expect_equal(get_orders(con)$status, "filled")
  fills = get_fills(con)
  expect_equal(nrow(fills), 1)
  expect_equal(fills$price, px)
  expect_equal(fills$qty, qty)
  expect_equal(fills$symbol, "600000")
  expect_equal(fills$side, "buy")

  pos = get_positions(con)[symbol == "600000"]
  expect_equal(pos$qty, qty)
  expect_equal(pos$avail_qty, 0)          # T+1 当日买入不可卖
  expect_equal(pos$avg_cost, px)

  amount = qty * px
  expect_equal(get_cash(con), STARTING_CASH - amount - commission(amount, settings))
  dbDisconnect(con)
})

test_that("match_orders 单笔卖出手算断言（含印花税）", {
  con = new_test_con()
  settings = load_settings(con)
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  px = last_close(con, "600000")
  upsert_position(con, "600000", 200, 200, px)
  id = save_order(con, "2023-01-01 09:00:00", "600000", "sell", 100, NA_real_)
  match_orders(con, setNames(c(px), "600000"), ts = "2023-01-02 09:00:00")

  pos = get_positions(con)[symbol == "600000"]
  expect_equal(pos$qty, 100)
  expect_equal(pos$avail_qty, 100)
  expect_equal(get_orders(con)$status, "filled")

  amount = 100 * px
  fee = commission(amount, settings) + stamp_duty(amount, settings)
  expect_equal(get_cash(con), STARTING_CASH + amount - fee)
  dbDisconnect(con)
})

test_that("rollover_positions 解锁 T+1", {
  con = new_test_con()
  upsert_position(con, "600000", 100, 0, 10)
  rollover_positions(con, as.Date("2023-01-02"))
  expect_equal(get_positions(con)[symbol == "600000"]$avail_qty, 100)
  dbDisconnect(con)
})

test_that("reset_account 清空订单/成交/持仓/快照并恢复初始资金", {
  con = new_test_con()
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  px = last_close(con, "600000")
  id = save_order(con, "2023-01-01 09:00:00", "600000", "buy", 100, px)
  match_orders(con, setNames(c(px), "600000"), ts = "2023-01-02 09:00:00")
  set_meta(con, "rollover_date", "2023-01-02")
  set_meta(con, "auto_cancel_date", "2023-01-02")

  expect_equal(nrow(get_orders(con)), 1)
  expect_equal(nrow(get_fills(con)), 1)
  expect_equal(nrow(get_positions(con)), 1)

  reset_account(con)
  expect_equal(nrow(get_orders(con)), 0)
  expect_equal(nrow(get_fills(con)), 0)
  expect_equal(nrow(get_positions(con)), 0)
  expect_equal(nrow(get_snapshots(con)), 1)
  expect_equal(get_cash(con), load_settings(con)$starting_cash)
  expect_equal(get_meta(con, "rollover_date", default = ""), "")
  expect_equal(get_meta(con, "auto_cancel_date", default = ""), "")
  dbDisconnect(con)
})

test_that("place_order 市价单价格 NA 入库、日志显示参考价、订单表显示参考价", {
  con = new_test_con()
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  px = last_close(con, "600000")
  oid = place_order(con, "600000", "buy", 100)$id
  o = get_orders(con)
  expect_true(is.na(o[o$id == oid]$price), info = "市价单价格存 NA（匹配时必成交）")
  msg = get_logs(con)$message[1]
  expect_match(msg, sprintf("价格 %.2f", px))
  expect_false(grepl("市价", msg))
  d = orders_view(con, sym_map(get_symbols(con)))
  expect_equal(d[id == oid]$price, px, info = "订单表显示参考价")
  dbDisconnect(con)
})

test_that("match_orders 限价语义：市价未达限价不成交，达到才成交", {
  con = new_test_con()
  b = synthetic_bars("600000", 5)
  replace_bars(con, "600000", b, table = "daily_bar_real")
  px = last_close(con, "600000")
  px_map = setNames(c(px), "600000")
  # 买限价高于市价 → 立即成交
  oid1 = place_order(con, "600000", "buy", 100, px)$id
  match_orders(con, px_map)
  expect_equal(get_orders(con)[id == oid1]$status, "filled")
  # 买限价低于市价 → 不成交，保持 open
  oid2 = place_order(con, "600000", "buy", 100, px - 1)$id
  match_orders(con, px_map)
  expect_equal(get_orders(con)[id == oid2]$status, "open")
  # 市价降到 ≤ 限价 → 成交
  px_low = setNames(c(px - 1), "600000")
  match_orders(con, px_low)
  expect_equal(get_orders(con)[id == oid2]$status, "filled")
  dbDisconnect(con)
})
