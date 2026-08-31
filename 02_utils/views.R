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

# 订单表显示数据（名称/方向/状态中文化；市价单价格 NA 显示最新真实收盘参考价）
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
    if (any(is.na(o$price))) {
      px = latest_prices(con)
      px_map = setNames(px$price, px$symbol)
      o[is.na(price), price := px_map[symbol]]
    }
  }
  o[, .(id, ts, symbol, name, side, qty, price, status)]
}

# 成交回报显示数据（名称/现价/回报，先舍入价格再算回报保证口径一致）
# price_map: data.table(symbol, price) 实时价，NULL 回退最新日收盘
fills_view = \(con, sym_map_dt, price_map = NULL) {
  f = get_fills(con)
  if (nrow(f) == 0) return(data.table())
  f = merge(f, sym_map_dt, by = "symbol", all.x = TRUE)
  px = price_map %||% latest_prices(con)
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

# 估值价格：实时价缺失的持仓/成交/未成交订单回退最新日收盘，保证现价/市值/权益不遗漏
valuation_prices = \(con, rt_dt) {
  pm = if (nrow(rt_dt) > 0) {
    rt_dt[, .(symbol = code, price)]
  } else {
    data.table(symbol = character(), price = numeric())
  }
  need = unique(c(
    get_positions(con)[qty > 0]$symbol,
    get_fills(con)$symbol,
    get_orders(con, status = "open")$symbol
  ))
  missing = setdiff(need, pm$symbol)
  if (length(missing) > 0) {
    lp = latest_prices(con)[symbol %in% missing]
    pm = rbindlist(list(pm, lp), fill = TRUE)
  }
  pm
}

# 估值实时行情：交易时段用实时价（盘中刷新），非交易时段置空 → 走 valuation_prices 回退最新日收盘（权威，避免陈旧实时价）
valuation_quote = \(con, rt_dt) {
  if (in_trading_hours()) rt_dt else data.table()
}

# 昨收价（前一个交易日真实收盘）：若最新 bar 是今天则取前一交易日，否则取最新
prev_close_map = \(con, symbols) {
  d = load_bars(con, symbols = symbols, real = TRUE)
  if (nrow(d) == 0) return(data.table(symbol = character(), prev_close = numeric()))
  d = d[order(symbol, date)]
  today = format(Sys.Date(), "%Y-%m-%d")
  out = d[, {
    n = .N
    if (date[n] == today) {
      pc = if (n >= 2) close[n - 1] else NA_real_
    } else {
      pc = close[n]
    }
    .(prev_close = pc)
  }, by = symbol]
  out
}

# 账户 KPI（现金/总资产/持仓数/累计浮盈/当日盈亏/当天浮盈/近 30 快照）
# price_map: data.table(symbol, price) 估值价（交易实时/收盘日收盘）
# prev_close: data.table(symbol, prev_close) 昨收（日数据）
# 当天浮盈（券商口径）：昨日已有持仓 → (现价-昨收)×数量；今日买入且仍持有 → (现价-今日买入均价)×数量
account_kpis_view = \(con, price_map = NULL, prev_close = NULL) {
  a = get_account(con, prices = price_map %||% latest_prices(con))
  pos = a$positions
  pnl = sum((pos$price - pos$avg_cost) * pos$qty, na.rm = TRUE)
  pnl = ifelse(is.na(pnl), 0, pnl)

  day_pnl = 0
  if (nrow(pos) > 0 && !is.null(prev_close) && nrow(prev_close) > 0) {
    today_str = format(Sys.Date(), "%Y-%m-%d")
    fills = get_fills(con)
    if (nrow(fills) > 0) fills = fills[startsWith(ts, today_str)]
    buys  = if (nrow(fills)) fills[side == "buy"] else data.table()
    sells = if (nrow(fills)) fills[side == "sell"] else data.table()
    bsum = if (nrow(buys)) {
      buys[, .(bqty = sum(qty), bamt = sum(qty * price)), by = symbol]
    } else {
      data.table(symbol = character(), bqty = numeric(), bamt = numeric())
    }
    ssum = if (nrow(sells)) {
      sells[, .(sqty = sum(qty)), by = symbol]
    } else {
      data.table(symbol = character(), sqty = numeric())
    }
    p2 = merge(pos, prev_close, by = "symbol", all.x = TRUE)
    p2 = merge(p2, bsum, by = "symbol", all.x = TRUE)
    p2 = merge(p2, ssum, by = "symbol", all.x = TRUE)
    p2[, `:=`(
      bqty = fifelse(is.na(bqty), 0, bqty),
      sqty = fifelse(is.na(sqty), 0, sqty)
    )]
    p2[, net_buy := pmin(qty, pmax(0, bqty - sqty))]     # 今日净买入且仍持有
    p2[, held_before := qty - net_buy]                    # 昨日持有到今日的数量
    p2[, avg_buy := fifelse(net_buy > 0 & bqty > 0, bamt / bqty, NA_real_)]
    day_pnl = sum((p2$price - p2$prev_close) * p2$held_before, na.rm = TRUE) +
      sum((p2$price - p2$avg_buy) * p2$net_buy, na.rm = TRUE)
  }

  snaps = get_snapshots(con)   # ts, cash, equity
  if (nrow(snaps) > 0) {
    today = format(Sys.Date(), "%Y-%m-%d")
    prev = snaps[ts < paste0(today, " 00:00:00")]      # 今日之前的快照（昨收/初始）
    base = if (nrow(prev) > 0) tail(prev, 1)$equity else snaps$equity[1]
    equity_change = a$equity - base                     # 当日盈亏 = 当前权益 - 昨收/初始（含费用）
  } else {
    equity_change = 0
  }
  list(
    cash = a$cash,
    equity = a$equity,
    n_pos = nrow(pos),
    pnl = pnl,
    day_pnl = day_pnl,
    equity_change = equity_change,
    spark = tail(snaps$equity, 30)
  )
}

# 当前持仓显示数据（成本/现价/市值/浮盈亏均两位小数）
positions_view = \(con, sym_map_dt, price_map = NULL) {
  pos = get_account(con, prices = price_map %||% latest_prices(con))$positions
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
