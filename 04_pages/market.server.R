# 04_pages/market.server.R — 行情展示

market_server = \(id, con, rv) {
  moduleServer(id, \(input, output, session) {
    ns = session$ns

    observe({
      req(nrow(rv$symbols) > 0)
      cur = isolate(input$sym)
      choices = setNames(rv$symbols$code, paste0(rv$symbols$code, " ", rv$symbols$name))
      selected = if (is.null(cur) || !cur %in% choices) choices[1] else cur
      updateSelectInput(session, "sym", choices = choices, selected = selected)
    })

    observe({
      req(input$sym)
      rng = load_bars(con, symbols = input$sym)$date
      if (length(rng) > 0) {
        updateDateRangeInput(session, "date_range", start = min(rng), end = max(rng))
      }
    })

    kline = reactive({
      req(input$sym)
      b = load_bars(con, symbols = input$sym)
      req(nrow(b) > 0)
      b = b[order(date)]
      b[, date := as.Date(date)]
      b[, `:=`(
        ma5 = frollmean(close, 5),
        ma10 = frollmean(close, 10),
        ma20 = frollmean(close, 20)
      )]
      rng = input$date_range
      if (!is.null(rng) && !is.na(rng[1]) && !is.na(rng[2])) {
        b = b[date >= rng[1] & date <= rng[2]]
      }
      req(nrow(b) > 0)
      b[, .(date, open, close, low, high, volume, ma5, ma10, ma20)]
    })

    output$kline = renderEcharts4r({
      kline_chart(kline())
    })

    rt = reactiveVal(data.table())
    rt_init = reactiveVal(FALSE)
    observe({
      tick(10000, session)
      if ((in_trading_hours() || isFALSE(rt_init())) && nrow(rv$symbols) > 0) {
        rt(tryCatch(fetch_realtime(rv$symbols$code), error = \(e) data.table()))
        rt_init(TRUE)
      }
    })

    output$rt_table = renderDT({
      d = rt()
      if (nrow(d) == 0) return(datatable(data.table()))
      d[, volume := round2(volume / 1e4, 2)]    # 成交量 → 万手（两位小数）
      d[, amount := round2(amount / 1e8, 2)]    # 成交额 → 亿元
      d[, change := round2(change, 2)]          # 涨跌额 → 两位小数
      d[, pct    := round2(pct, 2)]             # 涨跌幅 → 两位小数
      datatable(
        d,
        rownames = FALSE,
        colnames = c(
          "代码", "名称", "现价", "今开",
          "最高", "最低", "成交量（万手）", "成交额（亿）",
          "涨跌额", "涨跌幅（%）"
        ),
        options = list(dom = "t", pageLength = 50)
      ) |>
        formatRound(
          columns = c("price", "open", "high", "low", "volume", "amount", "change", "pct"),
          digits = 2
        ) |>
        formatStyle("pct", color = styleInterval(0, c(GREEN_DOWN, RED_UP))) |>
        formatStyle("change", color = styleInterval(0, c(GREEN_DOWN, RED_UP)))
    })
  })
}
