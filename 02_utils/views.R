# R/views.R — 视图层：页面显示数据组装（纯函数，可离线单测）
# 页面 server 只做「取数 → 渲染」，业务组装（合并名称/计算/中文化/格式化）收敛于此

# 四舍五入（远离零，避免 R round() 的银行家舍入）
round2 = \(x, digits) {
  posneg = sign(x)
  z = abs(x) * 10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z / 10^digits
  z * posneg
}

# 标的代码→名称映射（from rv$symbols 或任意 code/name 表）
sym_map = \(symbols_dt) symbols_dt[, .(symbol = code, name)]

# 校验回测准备数据可用（供页面 server 使用）
req_data = \(rv) {
  req(!is.null(rv$data))
  invisible(rv$data)
}

# 订单表显示数据（名称/方向/状态中文化）
orders_view = \(con, sym_map_dt) {
  o = get_orders(con)
  if (nrow(o) == 0) {
    o[, name := character()]
  } else {
    o = merge(o, sym_map_dt, by = "symbol", all.x = TRUE)
    o[, side := fifelse(side == "buy", "买入", "卖出")]
    o[, status := fcase(
      status == "open", "未成交",
      status == "filled", "已成交",
      status == "cancelled", "已撤销",
      default = status
    )]
  }
  o[, .(id, ts, symbol, name, side, qty, price, status)]
}

# 成交回报显示数据（名称/现价/回报，先舍入价格再算回报保证口径一致）
fills_view = \(con, sym_map_dt) {
  f = get_fills(con)
  if (nrow(f) == 0) return(data.table())
  f = merge(f, sym_map_dt, by = "symbol", all.x = TRUE)
  px = latest_prices(con)
  px_map = setNames(px$price, px$symbol)
  f[, price := round2(price, 2)]
  f[, cur_price := round2(px_map[symbol], 2)]
  f[, ret := fifelse(
    side == "buy",
    (cur_price - price) * qty,
    (price - cur_price) * qty
  )]
  f[, ret := round2(ret, 2)]
  f[, side := fifelse(side == "buy", "买入", "卖出")]
  f[, .(id, order_id, ts, symbol, name, side, price, qty, cur_price, ret)]
}

# 账户 KPI（现金/总资产/持仓数/浮盈亏/权益变化/近 30 快照）
account_kpis_view = \(con) {
  a = get_account(con, prices = latest_prices(con))
  pos = a$positions
  pnl = sum((pos$price - pos$avg_cost) * pos$qty, na.rm = TRUE)
  pnl = ifelse(is.na(pnl), 0, pnl)
  snaps = get_snapshots(con)$equity
  equity_change = if (length(snaps) >= 2) snaps[length(snaps)] - snaps[length(snaps) - 1] else 0
  list(
    cash = a$cash,
    equity = a$equity,
    n_pos = nrow(pos),
    pnl = pnl,
    equity_change = equity_change,
    spark = tail(snaps, 30)
  )
}

# 当前持仓显示数据（成本/现价/市值/浮盈亏均两位小数）
positions_view = \(con, sym_map_dt) {
  pos = get_account(con, prices = latest_prices(con))$positions
  if (nrow(pos) == 0) return(data.table())
  pos = merge(pos, sym_map_dt, by = "symbol", all.x = TRUE)
  pos[, avg_cost := round2(avg_cost, 2)]
  pos[, price := round2(price, 2)]
  pos[, market_value := round2(market_value, 2)]
  pos[, pnl := round2((price - avg_cost) * qty, 2)]
  pos[, .(symbol, name, qty, avail_qty, avg_cost, price, market_value, pnl)]
}

# 权益曲线显示数据（快照不足时补一行，保证图表可渲染）
equity_view = \(con) {
  s = get_snapshots(con)
  if (nrow(s) < 2) s = rbind(s, s)
  s[, .(ts, equity)]
}

# 系统日志显示数据（级别/模块中文化）
log_view = \(con, limit = 300) {
  d = get_logs(con, limit = limit)
  if (nrow(d) > 0) {
    d[, level := fcase(
      level == "info", "信息",
      level == "warn", "警告",
      level == "error", "错误",
      default = level
    )]
    d[, module := fcase(
      module == "fetch", "数据获取",
      module == "account", "账户",
      module == "system", "系统",
      module == "order", "订单",
      default = module
    )]
  }
  d
}

# 今日信号显示数据（按 favor 降序、评分两位小数、按 max_assets 截取）
signals_view = \(data, sym_map_dt, n1, n2, n_sharpe, sh_thresh, max_assets) {
  sig = latest_signals(data, n1, n2, n_sharpe, sh_thresh)
  sig = merge(sig, sym_map_dt, by = "symbol", all.x = TRUE, sort = FALSE)
  sig = sig[entry == 1][order(-favor)]
  sig[seq_len(min(max_assets, .N)), .(symbol, name, favor = round2(favor, 2))]
}

# K线显示数据（MA5/10/20 + 日期筛选）
kline_view = \(con, symbol, rng = NULL) {
  b = load_bars(con, symbols = symbol)
  if (nrow(b) == 0) return(b)
  b = b[order(date)]
  b[, date := as.Date(date)]
  b[, `:=`(
    ma5 = frollmean(close, 5),
    ma10 = frollmean(close, 10),
    ma20 = frollmean(close, 20)
  )]
  if (!is.null(rng) && !is.na(rng[1]) && !is.na(rng[2])) {
    b = b[date >= rng[1] & date <= rng[2]]
  }
  b[, .(date, open, close, low, high, volume, ma5, ma10, ma20)]
}

# 实时行情格式化（成交量→万手、成交额→亿、涨跌/涨跌幅两位小数）
# 复制入参再就地修改，保持纯函数（不改动调用方持有的对象）
realtime_view = \(d) {
  if (nrow(d) == 0) return(d)
  d = copy(d)
  d[, volume := round2(volume / 1e4, 2)]
  d[, amount := round2(amount / 1e8, 2)]
  d[, change := round2(change, 2)]
  d[, pct := round2(pct, 2)]
  d
}
