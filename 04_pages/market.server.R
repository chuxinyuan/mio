# 04_pages/market.server.R — 行情展示

market_server = \(id, con, rv) {
  moduleServer(id, \(input, output, session) {
    ns = session$ns

    observe({
      req(nrow(rv$symbols) > 0)
      cur = isolate(input$sym)
      choices = rv$symbols$code
      selected = if (is.null(cur) || !cur %in% choices) choices[1] else cur
      updateSelectInput(session, "sym", choices = choices, selected = selected)
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
      b[, .(date, open, close, low, high, volume, ma5, ma10, ma20)]
    })

    output$kline = renderEcharts4r({
      kline_chart(kline())
    })

    rt = reactivePoll(
      intervalMillis = 10000,
      session,
      checkFunc = \() Sys.time(),
      valueFunc = \() {
        req(nrow(rv$symbols) > 0)
        tryCatch(fetch_realtime(rv$symbols$code), error = \(e) data.table())
      }
    )

    output$rt_table = renderDT({
      d = rt()
      if (nrow(d) == 0) return(datatable(data.table()))
      datatable(
        d,
        rownames = FALSE,
        colnames = c(
          "代码", "名称", "现价", "今开",
          "最高", "最低", "成交量", "成交额",
          "涨跌额", "涨跌幅"
        ),
        options = list(dom = "t", pageLength = 8)
      ) |>
        formatRound(columns = c("price", "open", "high", "low"), digits = 2) |>
        formatStyle("pct", color = styleInterval(0, c(GREEN_DOWN, RED_UP))) |>
        formatStyle("change", color = styleInterval(0, c(GREEN_DOWN, RED_UP)))
    })
  })
}
