# 04_pages/market.server.R — 行情展示

market_server = \(id, con, rv, cur_rt) {
  moduleServer(id, \(input, output, session) {

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
      b = kline_view(con, input$sym, input$date_range)
      req(nrow(b) > 0)
      b
    })

    output$kline = renderEcharts4r({
      kline_chart(kline())
    })

    output$rt_table = renderDT({
      d = realtime_view(cur_rt())
      if (nrow(d) == 0) return(datatable(data.table()))
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
