# server.R — 服务端

server = \(input, output, session) {
  last_quote = reactiveVal(NULL)

  # 共享实时行情（全应用单一轮询源）：交易时段每 10s 刷新，首次加载拉一次，非交易时段冻结
  cur_rt = reactiveVal(data.table())
  rt_init = reactiveVal(FALSE)
  last_snap = reactiveVal(0)

  observe({
    tick(10000, session)
    if ((in_trading_hours() || isFALSE(rt_init())) && nrow(rv$symbols) > 0) {
      px = tryCatch(fetch_realtime(rv$symbols$code), error = \(e) data.table())
      if (nrow(px) > 0) {
        cur_rt(px)
        rt_init(TRUE)
        # 参照券商：盘中定期写权益快照（约每 60s），供账户权益曲线实时更新
        now = as.numeric(Sys.time())
        if (in_trading_hours() && now - last_snap() >= 60) {
          a = get_account(con, prices = px[, .(symbol = code, price)])
          save_snapshot(con, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), a$cash, a$equity)
          last_snap(now)
        }
      }
    }
  })

  observe({
    tick(15000, session)
    if (in_trading_hours() || is.null(last_quote())) {
      q = tryCatch(fetch_index(), error = \(e) NULL)
      if (!is.null(q) && nrow(q) > 0) last_quote(q)
    }
  })

  output$nav_quotes = renderUI({
    q = last_quote()
    date_txt = format(Sys.Date(), "%Y-%m-%d")
    if (is.null(q) || nrow(q) == 0 || is.na(q$change)) {
      return(
        HTML(sprintf('<div class="nav-quotes"><span class="nav-q-date">%s</span></div>', date_txt))
      )
    }
    col = if (q$change >= 0) RED_UP else GREEN_DOWN
    sign = if (q$change >= 0) "+" else ""
    HTML(
      sprintf(
        '<div class="nav-quotes">
           <span class="nav-q-label">上证指数</span>
           <span class="nav-q-price" style="color:%s">%.2f</span>
           <span class="nav-q-change" style="color:%s">%s%.2f</span>
           <span class="nav-q-pct" style="color:%s">%s%.2f%%</span>
           <span class="nav-q-date">%s</span>
         </div>',
        col, q$price, col, sign, q$change, col, sign, q$pct, date_txt
      )
    )
  })

  market_server("market", con, rv, cur_rt)
  strategy_server("strategy", con, rv)
  order_server("order", con, rv, cur_rt)
  account_server("account", con, rv, cur_rt)
  log_server("log", con, rv)
  settings_server("settings", con, rv)
}
