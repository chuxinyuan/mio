# 04_pages/account.server.R — 账户监控

account_server = \(id, con, rv) {
  moduleServer(id, \(input, output, session) {

    observe({
      tick(5000, session)
      req(nrow(rv$bars) > 0)
      rollover_positions(con, max(rv$bars$date))
    })

    output$kpis = renderUI({
      tick(5000, session)
      k = account_kpis_view(con)
      fluidRow(
        column(3, value_box(fmt_money(k$cash), "可用资金", MAGENTA)),
        column(
          3,
          value_box(
            fmt_money(k$equity),
            "总资产",
            CYAN,
            change = k$equity_change,
            sparkline = k$spark
          )
        ),
        column(3, value_box(as.character(k$n_pos), "持仓数", PURPLE)),
        column(
          3,
          value_box(
            fmt_signed(k$pnl),
            "浮盈亏",
            ifelse(k$pnl >= 0, RED_UP, GREEN_DOWN),
            change = k$pnl
          )
        )
      )
    })

    output$equity_curve = renderEcharts4r({
      tick(5000, session)
      equity_chart(equity_view(con))
    })

    output$positions = renderDT({
      tick(5000, session)
      d = positions_view(con, sym_map(rv$symbols))
      if (nrow(d) == 0) return(datatable(data.table()))
      datatable(
        d,
        colnames = c(
          "代码", "名称", "持仓", "可卖",
          "成本", "现价", "市值", "浮盈亏"
        ),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 10)
      ) |>
        formatRound(columns = c("avg_cost", "price", "market_value", "pnl"), digits = 2) |>
        formatStyle("pnl", color = styleInterval(0, c(GREEN_DOWN, RED_UP)))
    })
  })
}
