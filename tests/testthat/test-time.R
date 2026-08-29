# tests/testthat/test-time.R — 交易时段判定（离线，纯函数）

as_market_time = \(ymd_hm) as.POSIXlt(as.POSIXct(ymd_hm, tz = ""))

test_that("盘中时段返回 TRUE", {
  expect_true(in_trading_hours(as_market_time("2026-08-27 09:30:00")))   # 周四开盘
  expect_true(in_trading_hours(as_market_time("2026-08-27 10:00:00")))   # 上午盘中
  expect_true(in_trading_hours(as_market_time("2026-08-27 11:30:00")))   # 上午收盘边界
  expect_true(in_trading_hours(as_market_time("2026-08-27 13:00:00")))   # 下午开盘
  expect_true(in_trading_hours(as_market_time("2026-08-27 14:59:00")))   # 下午收盘前
})

test_that("盘前/午休/收盘/周末返回 FALSE", {
  expect_false(in_trading_hours(as_market_time("2026-08-27 09:00:00")))  # 盘前
  expect_false(in_trading_hours(as_market_time("2026-08-27 11:31:00")))  # 午休
  expect_false(in_trading_hours(as_market_time("2026-08-27 12:30:00")))  # 午休
  expect_false(in_trading_hours(as_market_time("2026-08-27 15:00:00")))  # 收盘（含边界）
  expect_false(in_trading_hours(as_market_time("2026-08-27 18:00:00")))  # 盘后
  expect_false(in_trading_hours(as_market_time("2026-08-29 10:00:00")))  # 周六
  expect_false(in_trading_hours(as_market_time("2026-08-30 10:00:00")))  # 周日
})
