# tests/testthat/helper-setup.R — 测试公共环境
# source 项目源码，提供临时 DB 与合成数据工具（全部离线，不触网）

find_proj_root = function(start = getwd()) {
  d = normalizePath(start, winslash = "/")
  repeat {
    if (file.exists(file.path(d, "global.R"))) return(d)
    parent = dirname(d)
    if (parent == d) stop("未找到项目根目录（应包含 global.R）")
    d = parent
  }
}

PROJ_ROOT = find_proj_root()
setwd(PROJ_ROOT)

suppressMessages({
  source("config.R")
  source("01_settings/path.R")
  source("01_settings/color.R")
  source("00_fetch/data_eastmoney.R")
  source("02_utils/db.R")
  source("02_utils/indicators.R")
  source("02_utils/backtest.R")
  source("02_utils/account.R")
})

# 临时 SQLite 连接（已 init_db + init_account，初始现金 = STARTING_CASH）
new_test_con = function() {
  con = connect_db(tempfile(fileext = ".db"))
  init_db(con)
  init_account(con)
  con
}

# 确定性合成行情：n_days 个工作日 × syms 个标的
synthetic_bars = function(
  syms = c("600000", "600519", "601318", "600036"),
  n_days = 60,
  start = "2023-01-03",
  seed = 1
) {
  set.seed(seed)
  n = length(syms)
  drift = seq(0.0005, 0.003, length.out = n)
  cal = seq(as.Date(start), by = "day", length.out = ceiling(n_days * 1.5))
  dates = cal[!format(cal, "%u") %in% c("6", "7")][seq_len(n_days)]
  rows = vector("list", n)
  for (i in seq_len(n)) {
    base = 10 + 5 * i
    px = base * exp(cumsum(drift[i] + rnorm(n_days, 0, 0.015)))
    rows[[i]] = data.table(
      symbol = syms[i],
      date   = as.character(dates),
      open   = px * (1 + rnorm(n_days, 0, 0.005)),
      high   = px * (1 + abs(rnorm(n_days, 0, 0.01))),
      low    = px * (1 - abs(rnorm(n_days, 0, 0.01))),
      close  = px,
      volume = round(runif(n_days, 1e5, 1e6))
    )
  }
  rbindlist(rows)
}
