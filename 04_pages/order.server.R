# 04_pages/order.server.R — 订单管理

order_server = \(id, con, rv) {
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

    observeEvent(input$submit, {
      req(input$sym)
      px = suppressWarnings(as.numeric(input$price))
      if (is.na(px) || px <= 0) px = NA_real_
      r = place_order(con, input$sym, input$side, input$qty, px)
      showNotification(r$msg, type = if (r$ok) "message" else "error")
    })

    observeEvent(input$cancel, {
      req(!is.null(input$cancel_id), input$cancel_id != "")
      cancel_order(con, as.integer(input$cancel_id))
      showNotification(paste("已撤单 #", input$cancel_id), type = "message")
    })

    observeEvent(input$match, {
      px = latest_prices(con)
      price_map = setNames(px$price, px$symbol)
      match_orders(con, price_map)
      showNotification("撮合完成", type = "message")
    })

    output$orders = renderDT({
      tick(5000, session)
      o = get_orders(con)
      if (nrow(o) > 0) {
        nm = rv$symbols[, .(symbol = code, name)]
        o = merge(o, nm, by = "symbol", all.x = TRUE)
        o[, side := fifelse(side == "buy", "买入", "卖出")]
        o[, status := fcase(
          status == "open", "未成交",
          status == "filled", "已成交",
          status == "cancelled", "已撤销",
          default = status
        )]
      } else {
        o[, name := character()]
      }
      datatable(
        o[, .(id, ts, symbol, name, side, qty, price, status)],
        rownames = FALSE,
        colnames = c("单号", "时间", "标的", "名称", "方向", "数量", "价格", "状态"),
        options = list(pageLength = 10)
      )
    })

    output$fills = renderDT({
      tick(5000, session)
      f = get_fills(con)
      if (nrow(f) == 0) return(datatable(data.table()))
      nm = rv$symbols[, .(symbol = code, name)]
      f = merge(f, nm, by = "symbol", all.x = TRUE)
      px = latest_prices(con)
      px_map = setNames(px$price, px$symbol)
      f[, price := round2(price, 2)]
      f[, cur_price := round2(px_map[symbol], 2)]
      f[, ret := fifelse(
        side == "buy",
        (cur_price - price) * qty,
        (price - cur_price) * qty
      )]
      f[, ret := round2(ret, 2)]
      f[, side := fifelse(side == "buy", "买入", "卖出")]
      datatable(
        f[, .(id, order_id, ts, symbol, name, side, price, qty, cur_price, ret)],
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
