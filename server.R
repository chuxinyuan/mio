# server.R — 服务端

server = \(input, output, session) {
  last_quote = reactiveVal(NULL)

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

  market_server("market", con, rv)
  strategy_server("strategy", con, rv)
  order_server("order", con, rv)
  account_server("account", con, rv)
  log_server("log", con, rv)
  settings_server("settings", con, rv)
}
