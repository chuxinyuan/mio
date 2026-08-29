# R/account.R — 模拟账户与订单撮合（paper trading）
# 规则：T+1（当日买入次日可卖）、100 股整数倍、涨跌停（主板 ±10% / 科创板688 ±20%）
# 状态存 SQLite；撮合为次日开盘价成交，卖出扣印花税；交易参数从 DB settings 读取

library(data.table)

commission = function(amount, settings) settings$flat_commission + amount * settings$commission_rate
stamp_duty = function(amount, settings) amount * settings$stamp_duty

# 板块与涨跌停比例（上交所：688 科创板 ±20%，其余主板 ±10%）
limit_rate = function(code) ifelse(substr(code, 1, 3) == "688", 0.20, 0.10)

price_limits = function(code, prev_close) {
  r = limit_rate(code)
  list(up = round(prev_close * (1 + r), 2), down = round(prev_close * (1 - r), 2))
}

# 初始化账户（首次写入初始现金）
init_account = function(con) {
  s = get_snapshots(con)
  if (nrow(s) == 0) {
    cash = load_settings(con)$starting_cash
    save_snapshot(con, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), cash, cash)
  }
  invisible(get_cash(con))
}

get_cash = function(con) {
  s = get_snapshots(con)
  if (nrow(s) == 0) return(load_settings(con)$starting_cash)
  tail(s, 1)$cash
}

# 当前账户状态（需当日收盘价做市值）
get_account = function(con, prices = NULL) {
  cash = get_cash(con)
  pos = get_positions(con)
  if (!is.null(prices)) {
    pos = merge(pos, prices, by = "symbol", all.x = TRUE)
    pos[, market_value := qty * price]
    equity = cash + sum(pos$market_value, na.rm = TRUE)
  } else {
    pos[, market_value := NA_real_]
    equity = cash
  }
  list(cash = cash, positions = pos, equity = equity)
}

# 新交易日结算：T+1 解锁（全部持仓可卖）
rollover_positions = function(con, date = Sys.Date()) {
  last = get_meta(con, "rollover_date", default = "")
  if (as.character(date) <= last) return(invisible(FALSE))
  dbExecute(con, "UPDATE position SET avail_qty = qty")
  set_meta(con, "rollover_date", as.character(date))
  invisible(TRUE)
}

# 下单（校验：100 股整数倍、现金/可卖数量充足、涨跌停价）
place_order = function(
  con,
  symbol,
  side,
  qty,
  price = NA_real_,
  ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
) {
  settings = load_settings(con)
  qty = as.integer(qty)
  if (is.na(qty) || qty <= 0) return(list(ok = FALSE, msg = "数量无效"))
  if (qty %% 100 != 0) return(list(ok = FALSE, msg = "数量需为 100 股整数倍"))

  prev = last_close(con, symbol)
  if (!is.na(price) && !is.na(prev)) {
    lim = price_limits(symbol, prev)
    if (side == "buy"  && price > lim$up)   return(list(ok = FALSE, msg = "限价超过涨停价"))
    if (side == "sell" && price < lim$down) return(list(ok = FALSE, msg = "限价低于跌停价"))
  }

  if (side == "buy" && !is.na(price)) {
    cost = qty * price + commission(qty * price, settings)
    if (cost > get_cash(con)) return(list(ok = FALSE, msg = "现金不足"))
  }
  if (side == "sell") {
    sym = symbol
    pos = get_positions(con)[symbol == sym]
    if (nrow(pos) == 0 || pos$avail_qty < qty) return(list(ok = FALSE, msg = "可卖数量不足（T+1）"))
  }

  id = save_order(con, ts, symbol, side, qty, price)
  write_log(
    con,
    sprintf(
      "下单 %s %s %d 股 @ %s",
      side, symbol, qty,
      ifelse(is.na(price), "市价", as.character(price))
    ),
    "info",
    "account",
    ts
  )
  list(ok = TRUE, id = id)
}

cancel_order = function(con, id) {
  set_order_status(con, id, "cancelled")
  write_log(con, sprintf("撤单 #%d", id), "info", "account")
  invisible(TRUE)
}

# 撮合所有 open 订单（price_map: named vector，symbol -> 成交价）
match_orders = function(
  con,
  price_map,
  ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
) {
  settings = load_settings(con)
  orders = get_orders(con, status = "open")
  if (nrow(orders) == 0) return(invisible(NULL))

  cash = get_cash(con)
  for (i in seq_len(nrow(orders))) {
    o = orders[i]
    px = as.numeric(price_map[o$symbol])
    if (is.na(px)) next

    # 涨跌停约束
    prev = last_close(con, o$symbol)
    if (!is.na(prev)) {
      lim = price_limits(o$symbol, prev)
      if (o$side == "buy"  && px > lim$up)   next   # 涨停买不进
      if (o$side == "sell" && px < lim$down) next   # 跌停卖不出
    }

    pos = get_positions(con)[symbol == o$symbol]

    if (o$side == "buy") {
      amount = o$qty * px
      fee = commission(amount, settings)
      if (amount + fee > cash) next           # 资金不足，跳过（订单仍 open）
      cash = cash - amount - fee
      n_qty  = if (nrow(pos)) pos$qty + o$qty else o$qty
      n_avail = if (nrow(pos)) pos$avail_qty else 0      # 当日买入次日方可卖
      n_avg  = if (nrow(pos) && pos$qty > 0)
        (pos$qty * pos$avg_cost + amount) / n_qty else px
      upsert_position(con, o$symbol, n_qty, n_avail, n_avg)
    } else {
      if (nrow(pos) == 0 || pos$avail_qty < o$qty) next  # T+1 可卖数量不足
      amount = o$qty * px
      fee = commission(amount, settings) + stamp_duty(amount, settings)
      cash = cash + amount - fee
      n_qty = pos$qty - o$qty
      n_avail = pos$avail_qty - o$qty
      if (n_qty <= 0) {
        dbExecute(
          con,
          "DELETE FROM position WHERE symbol = :symbol",
          list(symbol = o$symbol)
        )
      } else {
        upsert_position(con, o$symbol, n_qty, n_avail, pos$avg_cost)
      }
    }

    save_fill(con, o$id, ts, px, o$qty)
    set_order_status(con, o$id, "filled")
    write_log(
      con,
      sprintf("成交 %s %s %d 股 @ %.2f", o$side, o$symbol, o$qty, px),
      "info",
      "account",
      ts
    )
  }

  px_all = price_map[get_positions(con)$symbol]
  equity = cash + sum(get_positions(con)$qty * as.numeric(px_all), na.rm = TRUE)
  save_snapshot(con, ts, cash, equity)
  invisible(cash)
}

# ------------------------------
# 信号 -> 交易建议
# ------------------------------

# 纯决策逻辑：由当前持仓与信号向量计算要买卖的标的（沿袭 simulate 的 Step 5-7，仅做多）
decide_trades = function(syms, entry, exit, favor, held, max_assets) {
  names(entry) = names(exit) = names(favor) = syms

  long_pos = intersect(held, syms)
  n_pos = length(long_pos)

  trigger = setdiff(syms[which(entry == 1)], long_pos)
  if (length(trigger) > max_assets) {
    trigger = trigger[order(favor[trigger], decreasing = TRUE)][1:max_assets]
  }

  exit_trigger = long_pos[long_pos %in% syms[which(exit == 1 | exit == 999)]]
  need_to_exit = max((length(trigger) - length(exit_trigger)) - (max_assets - n_pos), 0)

  to_exit = character(0)
  if (need_to_exit > 0) {
    cand = setdiff(long_pos, exit_trigger)
    ord = cand[order(favor[cand])]          # favor 最低者先出
    to_exit = ord[seq_len(min(need_to_exit, length(ord)))]
  }

  list(enter = trigger, exit = unique(c(exit_trigger, to_exit)), favor = favor)
}

# 由当前持仓与最新信号计算要买卖的标的
compute_trades = function(data, n1, n2, n_sharpe, sh_thresh, held, max_assets) {
  sig = make_signals(data$close, data$return, n1, n2, n_sharpe, sh_thresh)
  syms = names(data$close)
  entry = as.numeric(sig$entry[nrow(sig$entry), ])
  exit  = as.numeric(sig$exit[nrow(sig$exit), ])
  favor = as.numeric(sig$favor[nrow(sig$favor), ])
  decide_trades(syms, entry, exit, favor, held, max_assets)
}

# 按信号自动下单：卖出持仓中应退出的，买入信号中应进场的（等权分配现金）
auto_trade = function(
  con,
  data,
  n1,
  n2,
  n_sharpe,
  sh_thresh,
  max_assets,
  prices,
  ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
) {
  held = get_positions(con)[qty > 0]$symbol
  tr = compute_trades(data, n1, n2, n_sharpe, sh_thresh, held, max_assets)

  results = list()
  for (s in tr$exit) {
    pos = get_positions(con)[symbol == s]
    if (nrow(pos) && pos$qty > 0)
      results[[length(results) + 1]] = place_order(con, s, "sell", pos$qty, NA_real_, ts)
  }

  if (length(tr$enter) > 0) {
    cash = get_cash(con)
    budget = cash / length(tr$enter)
    for (s in tr$enter) {
      px = as.numeric(prices[s])
      if (is.na(px) || px <= 0) next
      qty = floor(budget / px / 100) * 100
      if (qty >= 100)
        results[[length(results) + 1]] = place_order(con, s, "buy", qty, NA_real_, ts)
    }
  }

  results
}
