# 04_pages/account.server.R — 账户监控

account_server = function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    observe({
      invalidateLater(5000, session)
      req(nrow(rv$bars) > 0)
      rollover_positions(con, max(rv$bars$date))
    })

    acct = reactive({
      get_account(con, prices = latest_prices(con))
    })

    output$kpis = renderUI({
      invalidateLater(5000, session)
      a = acct()
      pos = a$positions
      pos_value = sum(pos$market_value, na.rm = TRUE)
      pnl = sum((pos$price - pos$avg_cost) * pos$qty, na.rm = TRUE)
      pnl = ifelse(is.na(pnl), 0, pnl)
      snaps = get_snapshots(con)$equity
      equity_change = if (length(snaps) >= 2) snaps[length(snaps)] - snaps[length(snaps) - 1] else 0
      fluidRow(
        column(3, value_box(fmt_money(a$cash), "可用资金", MAGENTA)),
        column(3, value_box(fmt_money(a$equity), "总资产", CYAN,
                            change = equity_change, sparkline = tail(snaps, 30))),
        column(3, value_box(as.character(nrow(pos)), "持仓数", PURPLE)),
        column(3, value_box(fmt_signed(pnl), "浮盈亏", ifelse(pnl >= 0, RED_UP, GREEN_DOWN),
                            change = pnl))
      )
    })

    output$equity_curve = renderEcharts4r({
      invalidateLater(5000, session)
      s = get_snapshots(con)
      if (nrow(s) < 2) s = rbind(s, s)
      equity_chart(s[, .(ts, equity)])
    })

    output$positions = renderDT({
      invalidateLater(5000, session)
      pos = acct()$positions
      if (nrow(pos) == 0) return(datatable(data.table()))
      nm = rv$symbols[, .(symbol = code, name)]
      pos = merge(pos, nm, by = "symbol", all.x = TRUE)
      pos[, pnl := round((price - avg_cost) * qty, 2)]
      datatable(pos[, .(symbol, name, qty, avail_qty, avg_cost = round(avg_cost, 2),
                        price, market_value = round(market_value, 2), pnl)],
                colnames = c("代码", "名称", "持仓", "可卖", "成本", "现价", "市值", "浮盈亏"),
                rownames = FALSE, options = list(dom = "t", pageLength = 10)) |>
        formatStyle("pnl", color = styleInterval(0, c(GREEN_DOWN, RED_UP)))
    })
  })
}
