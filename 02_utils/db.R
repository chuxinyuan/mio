# R/db.R — SQLite 存储层封装（DBI + RSQLite）
# 数据：行情 + 交易 + 账户 + 日志，统一存于 DB_PATH

library(DBI)
library(RSQLite)
library(data.table)

connect_db = \(path = DB_PATH) {
  dbConnect(RSQLite::SQLite(), path)
}

# 建表（幂等）
init_db = \(con = NULL) {
  own = is.null(con)
  if (own) con = connect_db()
  on.exit(if (own) dbDisconnect(con), add = TRUE)

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS symbol (
      code     TEXT PRIMARY KEY,
      name     TEXT,
      board    TEXT,
      is_valid INTEGER NOT NULL DEFAULT 1
    );"
  )

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS daily_bar (
      symbol TEXT NOT NULL,
      date   TEXT NOT NULL,
      open   REAL,
      high   REAL,
      low    REAL,
      close  REAL,
      volume REAL,
      PRIMARY KEY (symbol, date)
    );"
  )

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS daily_bar_real (
      symbol TEXT NOT NULL,
      date   TEXT NOT NULL,
      open   REAL,
      high   REAL,
      low    REAL,
      close  REAL,
      volume REAL,
      PRIMARY KEY (symbol, date)
    );"
  )

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS order_ticket (
      id     INTEGER PRIMARY KEY AUTOINCREMENT,
      ts     TEXT NOT NULL,
      symbol TEXT NOT NULL,
      side   TEXT NOT NULL,            -- buy / sell
      qty    INTEGER NOT NULL,
      price  REAL,
      status TEXT NOT NULL DEFAULT 'open'   -- open / filled / cancelled
    );"
  )

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS fill (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      order_id INTEGER NOT NULL,
      ts       TEXT NOT NULL,
      price    REAL NOT NULL,
      qty      INTEGER NOT NULL
    );"
  )

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS position (
      symbol    TEXT PRIMARY KEY,
      qty       INTEGER NOT NULL DEFAULT 0,
      avail_qty INTEGER NOT NULL DEFAULT 0,
      avg_cost  REAL NOT NULL DEFAULT 0
    );"
  )

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS meta (
      key   TEXT PRIMARY KEY,
      value TEXT
    );"
  )

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS settings (
      key   TEXT PRIMARY KEY,
      value TEXT
    );"
  )

  # 迁移：老库 position 缺 avail_qty 列则补上（T+1 可卖数量）
  pos_cols = dbGetQuery(con, "PRAGMA table_info(position)")$name
  if (!"avail_qty" %in% pos_cols) {
    dbExecute(con, "ALTER TABLE position ADD COLUMN avail_qty INTEGER NOT NULL DEFAULT 0")
  }

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS account_snapshot (
      ts     TEXT NOT NULL,
      cash   REAL NOT NULL,
      equity REAL NOT NULL
    );"
  )

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS sys_log (
      ts      TEXT NOT NULL,
      level   TEXT NOT NULL,
      module  TEXT NOT NULL,
      message TEXT NOT NULL
    );"
  )

  invisible(con)
}

# ---- symbol ----
upsert_symbols = \(con, dt) {
  dbExecute(
    con,
    "INSERT INTO symbol (code, name, board, is_valid)
     VALUES (:code, :name, :board, 1)
     ON CONFLICT(code) DO UPDATE SET name = excluded.name,
                                     board = excluded.board",
    dt[, .(code, name, board)]
  )
  invisible(dt)
}

get_symbols = \(con, valid_only = TRUE) {
  q = "SELECT code, name, board, is_valid FROM symbol"
  if (valid_only) q = paste(q, "WHERE is_valid = 1")
  as.data.table(dbGetQuery(con, q))
}

# 单标的名称查询（找不到返回空串）
symbol_name = \(con, code) {
  d = dbGetQuery(con, "SELECT name FROM symbol WHERE code = :code", list(code = code))
  if (nrow(d) == 0) "" else d$name[1]
}

# ---- daily_bar ----
# 增量 upsert：日期已存在则更新（table: daily_bar hfq 回测 / daily_bar_real 真实价交易）
upsert_bars = \(con, dt, table = "daily_bar") {
  dbExecute(
    con,
    sprintf(
      "INSERT INTO %s (symbol, date, open, high, low, close, volume)
       VALUES (:symbol, :date, :open, :high, :low, :close, :volume)
       ON CONFLICT(symbol, date) DO UPDATE SET
         open = excluded.open, high = excluded.high, low = excluded.low,
         close = excluded.close, volume = excluded.volume",
      table
    ),
    dt[, .(symbol, date, open, high, low, close, volume)]
  )
  invisible(dt)
}

# 全量替换某只股票（复权数据会漂移，更新时删旧插新）
replace_bars = \(con, symbol, dt, table = "daily_bar") {
  dbExecute(
    con,
    sprintf("DELETE FROM %s WHERE symbol = :symbol", table),
    list(symbol = symbol)
  )
  if (nrow(dt) > 0) upsert_bars(con, dt[, symbol := symbol], table = table)
  invisible(dt)
}

load_bars = \(con, symbols = NULL, from = NULL, to = NULL, real = FALSE) {
  tbl = if (real) "daily_bar_real" else "daily_bar"
  q = sprintf("SELECT symbol, date, open, high, low, close, volume FROM %s", tbl)
  cond = character(0)
  params = list()
  if (!is.null(symbols)) {
    cond = c(cond, paste0("symbol IN (", paste0(rep("?", length(symbols)), collapse = ","), ")"))
    params = c(params, as.list(symbols))
  }
  if (!is.null(from)) {
    cond = c(cond, "date >= ?")
    params = c(params, as.character(from))
  }
  if (!is.null(to)) {
    cond = c(cond, "date <= ?")
    params = c(params, as.character(to))
  }
  if (length(cond)) q = paste(q, "WHERE", paste(cond, collapse = " AND "))
  if (length(params) == 0) params = NULL
  as.data.table(dbGetQuery(con, q, params = params))
}

last_date = \(con, symbol) {
  r = dbGetQuery(
    con,
    "SELECT MAX(date) AS d FROM daily_bar WHERE symbol = :symbol",
    list(symbol = symbol)
  )
  r$d[[1]]
}

last_close = \(con, symbol) {
  r = dbGetQuery(
    con,
    "SELECT close FROM daily_bar_real WHERE symbol = :symbol
     ORDER BY date DESC LIMIT 1",
    list(symbol = symbol)
  )
  if (nrow(r) == 0) NA_real_ else r$close[[1]]
}

# 每个标的最新一条真实收盘价（交易/账户层：成交价/持仓现价/估值；data.table: symbol, price）
latest_prices = \(con) {
  as.data.table(dbGetQuery(
    con,
    "SELECT d.symbol, d.close AS price
     FROM daily_bar_real d
     JOIN (SELECT symbol, MAX(date) AS mdate FROM daily_bar_real GROUP BY symbol) m
       ON d.symbol = m.symbol AND d.date = m.mdate"
  ))
}

# ---- 交易 ----
save_order = \(con, ts, symbol, side, qty, price = NA_real_) {
  dbExecute(
    con,
    "INSERT INTO order_ticket (ts, symbol, side, qty, price, status)
     VALUES (:ts, :symbol, :side, :qty, :price, 'open')",
    list(ts = ts, symbol = symbol, side = side, qty = qty, price = price)
  )
  as.integer(dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1])
}

set_order_status = \(con, id, status) {
  dbExecute(
    con,
    "UPDATE order_ticket SET status = :status WHERE id = :id",
    list(id = id, status = status)
  )
  invisible(id)
}

save_fill = \(con, order_id, ts, price, qty) {
  dbExecute(
    con,
    "INSERT INTO fill (order_id, ts, price, qty)
     VALUES (:order_id, :ts, :price, :qty)",
    list(order_id = order_id, ts = ts, price = price, qty = qty)
  )
  invisible(TRUE)
}

get_orders = \(con, status = NULL) {
  q = "SELECT * FROM order_ticket"
  if (!is.null(status)) q = paste(q, "WHERE status = :status")
  as.data.table(dbGetQuery(
    con,
    q,
    params = if (is.null(status)) NULL else list(status = status)
  ))
}

get_fills = \(con) {
  as.data.table(dbGetQuery(
    con,
    "SELECT f.id, f.order_id, f.ts, f.price, f.qty, o.symbol, o.side
     FROM fill f
     JOIN order_ticket o ON f.order_id = o.id
     ORDER BY f.id DESC"
  ))
}

# ---- position ----
upsert_position = \(con, symbol, qty, avail_qty, avg_cost) {
  dbExecute(
    con,
    "INSERT INTO position (symbol, qty, avail_qty, avg_cost)
     VALUES (:symbol, :qty, :avail_qty, :avg_cost)
     ON CONFLICT(symbol) DO UPDATE SET
       qty = :qty, avail_qty = :avail_qty, avg_cost = :avg_cost",
    list(symbol = symbol, qty = qty, avail_qty = avail_qty, avg_cost = avg_cost)
  )
  invisible(TRUE)
}

get_positions = \(con) as.data.table(dbGetQuery(con, "SELECT * FROM position"))

# ---- meta ----
set_meta = \(con, key, value) {
  dbExecute(
    con,
    "INSERT INTO meta (key, value) VALUES (:key, :value)
     ON CONFLICT(key) DO UPDATE SET value = :value",
    list(key = key, value = as.character(value))
  )
  invisible(value)
}

get_meta = \(con, key, default = NA_character_) {
  r = dbGetQuery(con, "SELECT value FROM meta WHERE key = :key", list(key = key))
  if (nrow(r) == 0) default else r$value[[1]]
}

# ---- settings（交易/账户参数，可在 App 设置页覆盖） ----

# 默认参数（取自 config.R 全局常量）
default_settings = \() {
  list(
    max_assets = MAX_ASSETS,
    starting_cash = STARTING_CASH,
    slip_factor = SLIP_FACTOR,
    flat_commission = FLAT_COMMISSION,
    commission_rate = COMMISSION_RATE,
    stamp_duty = STAMP_DUTY
  )
}

set_setting = \(con, key, value) {
  dbExecute(
    con,
    "INSERT INTO settings (key, value) VALUES (:key, :value)
     ON CONFLICT(key) DO UPDATE SET value = :value",
    list(key = key, value = as.character(value))
  )
  invisible(value)
}

get_setting = \(con, key, default) {
  r = dbGetQuery(con, "SELECT value FROM settings WHERE key = :key", list(key = key))
  if (nrow(r) == 0) default else type.convert(r$value[[1]], as.is = TRUE)
}

# 清空全部自定义设置（恢复默认值）
clear_settings = \(con) {
  dbExecute(con, "DELETE FROM settings")
  invisible(TRUE)
}

# 载入全部参数，DB 缺失时回退到默认值
load_settings = \(con) {
  s = default_settings()
  rows = as.data.table(dbGetQuery(con, "SELECT key, value FROM settings"))
  for (i in seq_len(nrow(rows))) {
    k = rows$key[i]
    if (k %in% names(s)) s[[k]] = type.convert(rows$value[i], as.is = TRUE)
  }
  s
}

# ---- account ----
save_snapshot = \(con, ts, cash, equity) {
  dbExecute(
    con,
    "INSERT INTO account_snapshot (ts, cash, equity) VALUES (:ts, :cash, :equity)",
    list(ts = ts, cash = cash, equity = equity)
  )
  invisible(TRUE)
}

get_snapshots = \(con) as.data.table(dbGetQuery(con, "SELECT * FROM account_snapshot ORDER BY ts"))

# ---- log ----
write_log = \(
  con,
  message,
  level = "info",
  module = "system",
  ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
) {
  dbExecute(
    con,
    "INSERT INTO sys_log (ts, level, module, message) VALUES (:ts, :level, :module, :message)",
    list(ts = ts, level = level, module = module, message = message)
  )
  invisible(TRUE)
}

get_logs = \(con, limit = 500) {
  as.data.table(dbGetQuery(
    con,
    paste0("SELECT * FROM sys_log ORDER BY ts DESC LIMIT ", as.integer(limit))
  ))
}

# 清空系统日志
clear_logs = \(con) {
  dbExecute(con, "DELETE FROM sys_log")
  invisible(TRUE)
}
