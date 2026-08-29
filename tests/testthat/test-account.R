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
  replace_bars(con, "600000", b)
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
  replace_bars(con, "600000", b)
  px = last_close(con, "600000")
  qty = 100
  id = save_order(con, "2023-01-01 09:00:00", "600000", "buy", qty, px)
  match_orders(con, setNames(c(px), "600000"), ts = "2023-01-02 09:00:00")

  expect_equal(get_orders(con)$status, "filled")
  fills = get_fills(con)
  expect_equal(nrow(fills), 1)
  expect_equal(fills$price, px)
  expect_equal(fills$qty, qty)

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
  replace_bars(con, "600000", b)
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
