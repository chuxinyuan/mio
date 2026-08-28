# 04_pages/overview.server.R — 总览首页

overview_server = function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    output$kpis = renderUI({
      invalidateLater(10000, session)
      a = get_account(con, prices = latest_prices(con))
      pos = a$positions
      pos_value = sum(pos$market_value, na.rm = TRUE)
      pnl = sum((pos$price - pos$avg_cost) * pos$qty, na.rm = TRUE)
      pnl = ifelse(is.na(pnl), 0, pnl)
      snaps = get_snapshots(con)$equity
      equity_change = if (length(snaps) >= 2) snaps[length(snaps)] - snaps[length(snaps) - 1] else 0
      fluidRow(
        column(3, value_box(fmt_money(a$equity), "总资产", CYAN,
                            change = equity_change, sparkline = tail(snaps, 30))),
        column(3, value_box(fmt_money(a$cash), "可用资金", MAGENTA)),
        column(3, value_box(fmt_money(pos_value), "持仓市值", PURPLE)),
        column(3, value_box(fmt_signed(pnl), "浮盈亏", ifelse(pnl >= 0, RED_UP, GREEN_DOWN),
                            change = pnl))
      )
    })

    output$equity = renderEcharts4r({
      invalidateLater(10000, session)
      s = get_snapshots(con)
      if (nrow(s) < 2) s = rbind(s, s)
      equity_chart(s[, .(ts, equity)])
    })

    output$signals = renderDT({
      invalidateLater(30000, session)
      if (is.null(rv$data)) return(datatable(data.table()))
      sig = latest_signals(rv$data, 12, 26, 20, 0.5)
      nm = rv$symbols[, .(symbol = code, name)]
      sig = merge(sig, nm, by = "symbol", all.x = TRUE)
      top = sig[entry == 1, .(symbol, name, favor = round(favor, 3))][1:min(rv$settings$max_assets, .N)]
      datatable(top, rownames = FALSE, colnames = c("代码", "名称", "评分"),
                options = list(dom = "t", pageLength = 10))
    })

    output$logs = renderDT({
      invalidateLater(10000, session)
      datatable(get_logs(con, limit = 20), rownames = FALSE,
                colnames = c("时间", "级别", "模块", "信息"),
                options = list(dom = "t", pageLength = 10))
    })
  })
}
