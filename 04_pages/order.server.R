# 04_pages/order.server.R — 订单管理

order_server = \(id, con, rv, cur_rt) {
  moduleServer(id, \(input, output, session) {

    observe({
      req(nrow(rv$symbols) > 0)
      cur = isolate(input$sym)
      choices = setNames(rv$symbols$code, paste0(rv$symbols$code, " ", rv$symbols$name))
      selected = if (is.null(cur) || !cur %in% choices) choices[1] else cur
      updateSelectInput(session, "sym", choices = choices, selected = selected)
    })

    observe({
      tick(5000, session)
      open_ids = get_orders(con, status = "open")$id
      updateSelectInput(
        session,
        "cancel_id",
        choices = if (length(open_ids)) open_ids else character(0)
      )

      # 非交易时段：锁死交易按钮，未成交订单自动撤单（每日一次）
      if (in_trading_hours()) {
        shinyjs::enable("submit")
        shinyjs::enable("match")
        shinyjs::enable("cancel")
      } else {
        shinyjs::disable("submit")
        shinyjs::disable("match")
        shinyjs::disable("cancel")
        today = format(Sys.Date(), "%Y-%m-%d")
        last = get_meta(con, "auto_cancel_date", default = "")
        if (last != today) {
          if (length(open_ids) > 0) {
            for (id in open_ids) cancel_order(con, id)
            write_log(con, sprintf("收盘自动撤单 %d 笔未成交订单", length(open_ids)), "info", "account")
          }
          set_meta(con, "auto_cancel_date", today)
        }
      }
    })

    # 价格默认显示当前价（实时失败回退昨收），供用户修改
    observeEvent(input$sym, {
      req(input$sym)
      px = tryCatch(
        fetch_realtime(input$sym)$price[1],
        error = \(e) NA_real_
      )
      if (is.na(px)) {
        lp = latest_prices(con)[symbol == input$sym]
        px = if (nrow(lp) > 0) lp$price[1] else NA_real_
      }
      if (!is.na(px)) {
        updateTextInput(session, "price", value = format(round2(px, 2), nsmall = 2))
      }
    })

    # 实时限价监控：交易时段有未成交单时，按实时市价撮合，市价达到限价即自动成交
    observe({
      tick(15000, session)
      open_ids = get_orders(con, status = "open")$id
      if (in_trading_hours() && length(open_ids) > 0) {
        codes = unique(get_orders(con, status = "open")$symbol)
        px = tryCatch(fetch_realtime(codes), error = \(e) data.table())
        if (nrow(px) > 0) match_orders(con, setNames(px$price, px$symbol))
      }
    })

    observeEvent(input$submit, {
      req(input$sym)
      px = suppressWarnings(as.numeric(input$price))
      if (is.na(px) || px <= 0) px = NA_real_
      r = place_order(con, input$sym, input$side, input$qty, px)
      if (!r$ok) {
        showNotification(r$msg, type = "error")
        return()
      }
      # 提交即按实时市价撮合：限价未达则保持未成交，实时监控达到限价自动成交
      rt = tryCatch(fetch_realtime(input$sym), error = \(e) data.table())
      if (nrow(rt) > 0) match_orders(con, setNames(rt$price, rt$symbol))
      st = get_orders(con)[id == r$id]$status
      if (isTRUE(st == "filled")) {
        px_fill = get_fills(con)[order_id == r$id]$price[1]
        showNotification(sprintf("下单成功，已按 %.2f 成交", px_fill), type = "message")
      } else {
        showNotification("已下单，实时市价达到限价将自动成交", type = "warning")
      }
    })

    observeEvent(input$cancel, {
      req(!is.null(input$cancel_id), input$cancel_id != "")
      cancel_order(con, as.integer(input$cancel_id))
      showNotification(paste("已撤单 #", input$cancel_id), type = "message")
    })

    observeEvent(input$match, {
      px = tryCatch(fetch_realtime(rv$symbols$code), error = \(e) data.table())
      if (nrow(px) > 0) match_orders(con, setNames(px$price, px$symbol))
      showNotification("撮合完成", type = "message")
    })

    output$orders = renderDT({
      tick(5000, session)
      d = orders_view(con, sym_map(rv$symbols))
      datatable(
        d,
        rownames = FALSE,
        colnames = c("单号", "时间", "标的", "名称", "方向", "数量", "价格", "状态"),
        options = list(pageLength = 10)
      )
    })

    output$fills = renderDT({
      tick(5000, session)
      d = fills_view(con, sym_map(rv$symbols), price_map = cur_rt()[, .(symbol = code, price)])
      if (nrow(d) == 0) return(datatable(data.table()))
      datatable(
        d,
        rownames = FALSE,
        colnames = c(
          "成交号", "订单号", "时间", "代码", "名称",
          "方向", "成交价", "数量", "现价", "回报"
        ),
        options = list(pageLength = 10)
      ) |>
        formatRound(columns = c("price", "cur_price", "ret"), digits = 2) |>
        formatStyle("ret", color = styleInterval(0, c(GREEN_DOWN, RED_UP)))
    })
  })
}
