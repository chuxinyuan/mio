# tests/testthat/test-fetch.R — 数据获取纯函数测试（不触网）

test_that("%||% 逻辑", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_equal(NULL %||% NULL, NULL)
})

test_that("secid 上交所前缀", {
  expect_equal(secid("600000"), "1.600000")
  expect_equal(secid("688981"), "1.688981")
})

test_that("parse_realtime_batch 字段映射与单位（clist fltt=2）", {
  diff = data.frame(
    f12 = c("600000", "600519"),
    f14 = c("浦发银行", "贵州茅台"),
    f2 = c(10.5, 1500.25),
    f3 = c(1.2, -0.5),
    f4 = c(0.12, -7.5),
    f5 = c(100000, 20000),
    f6 = c(1e6, 3e7),
    f15 = c(10.8, 1510),
    f16 = c(10.2, 1490),
    f17 = c(10.3, 1505),
    stringsAsFactors = FALSE
  )
  dt = parse_realtime_batch(diff)
  expect_named(
    dt,
    c("code", "name", "price", "open", "high", "low",
      "volume", "amount", "change", "pct")
  )
  expect_equal(dt$code, c("600000", "600519"))
  expect_equal(dt$price, c(10.5, 1500.25))
  expect_equal(dt$open, c(10.3, 1505))
  expect_equal(dt$high, c(10.8, 1510))
  expect_equal(dt$low, c(10.2, 1490))
  expect_equal(dt$change, c(0.12, -7.5))
  expect_equal(dt$pct, c(1.2, -0.5))
})

test_that("parse_realtime_batch 空输入安全", {
  expect_equal(nrow(parse_realtime_batch(NULL)), 0)
  expect_equal(nrow(parse_realtime_batch(data.frame())), 0)
})

test_that("is_valid_bars 有效性判定", {
  good = data.table(date = "2023-01-01", open = 10, high = 11, low = 9, close = 10.5, volume = 100)
  bad_neg = copy(good)[, close := -1]
  bad_empty = good[0]
  expect_true(is_valid_bars(good))
  expect_false(is_valid_bars(bad_neg))
  expect_false(is_valid_bars(bad_empty))
  expect_false(is_valid_bars(NULL))
})
