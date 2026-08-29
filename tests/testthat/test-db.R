# tests/testthat/test-db.R — db.R 存储层测试

test_that("init_db 幂等且建全表", {
  con = connect_db(tempfile(fileext = ".db"))
  init_db(con)
  expect_no_error(init_db(con))
  tables = dbListTables(con)
  expect_true(all(c("symbol", "daily_bar", "order_ticket", "fill",
                    "position", "meta", "settings", "account_snapshot",
                    "sys_log") %in% tables))
  dbDisconnect(con)
})

test_that("symbol upsert + get", {
  con = new_test_con()
  upsert_symbols(con, data.table(code = c("600000", "600519"),
                                 name = c("浦发银行", "贵州茅台"), board = "SSE"))
  got = get_symbols(con)
  expect_equal(nrow(got), 2)
  expect_equal(got[code == "600519"]$name, "贵州茅台")
  upsert_symbols(con, data.table(code = "600000", name = "浦发银行(新)", board = "SSE"))
  expect_equal(get_symbols(con)[code == "600000"]$name, "浦发银行(新)")
  expect_equal(nrow(get_symbols(con)), 2)
  dbDisconnect(con)
})

test_that("replace_bars 删旧插新（不追加）", {
  con = new_test_con()
  b = synthetic_bars("600000", 10)
  replace_bars(con, "600000", b)
  expect_equal(nrow(load_bars(con, symbols = "600000")), 10)
  b2 = b[1:5]
  replace_bars(con, "600000", b2)
  got = load_bars(con, symbols = "600000")
  expect_equal(nrow(got), 5)
  expect_equal(sort(got$date), sort(b2$date))
  dbDisconnect(con)
})

test_that("upsert_bars 更新已存在行", {
  con = new_test_con()
  b = synthetic_bars("600000", 5)
  upsert_bars(con, b)
  b[1, close := close + 1]
  upsert_bars(con, b[1])
  got = load_bars(con, symbols = "600000")
  expect_equal(nrow(got), 5)
  expect_equal(got[date == b$date[1]]$close, b$close[1])
  dbDisconnect(con)
})

test_that("settings 默认/覆盖/回退", {
  con = new_test_con()
  expect_equal(load_settings(con)$starting_cash, STARTING_CASH)
  set_setting(con, "max_assets", 5)
  expect_equal(load_settings(con)$max_assets, 5)
  expect_equal(load_settings(con)$starting_cash, STARTING_CASH)
  dbExecute(con, "DELETE FROM settings")
  expect_equal(load_settings(con)$max_assets, MAX_ASSETS)
  dbDisconnect(con)
})

test_that("save_order / set_order_status / get_orders", {
  con = new_test_con()
  id = save_order(con, "2023-01-01 09:00:00", "600000", "buy", 100, 10.0)
  expect_true(id > 0)
  expect_equal(get_orders(con, status = "open")$id, id)
  set_order_status(con, id, "filled")
  expect_equal(get_orders(con, status = "filled")$id, id)
  expect_equal(nrow(get_orders(con, status = "open")), 0)
  dbDisconnect(con)
})

test_that("latest_prices 返回每个标的最新收盘", {
  con = new_test_con()
  b = synthetic_bars(c("600000", "600519"), 20)
  replace_bars(con, "600000", b[symbol == "600000"])
  replace_bars(con, "600519", b[symbol == "600519"])
  lp = latest_prices(con)
  expect_equal(sort(lp$symbol), c("600000", "600519"))
  expect_equal(lp[symbol == "600000"]$price,
               max(b[symbol == "600000"]$close))
  dbDisconnect(con)
})
