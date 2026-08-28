# 04_pages/order.server.R — 订单管理

order_server = function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    observe({
      req(nrow(rv$symbols) > 0)
      updateSelectInput(session, "sym", choices = rv$symbols$code)
    })

    observe({
      invalidateLater(5000, session)
      open_ids = get_orders(con, status = "open")$id
      updateSelectInput(session, "cancel_id",
                        choices = if (length(open_ids)) open_ids else character(0))
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
      invalidateLater(5000, session)
      o = get_orders(con)
      if (nrow(o) > 0) {
        o[, side := fifelse(side == "buy", "买入", "卖出")]
        o[, status := fcase(status == "open", "未成交",
                            status == "filled", "已成交",
                            status == "cancelled", "已撤销",
                            default = status)]
      }
      datatable(o, rownames = FALSE,
                colnames = c("单号", "时间", "标的", "方向", "数量", "价格", "状态"),
                options = list(pageLength = 10))
    })

    output$fills = renderDT({
      invalidateLater(5000, session)
      datatable(get_fills(con), rownames = FALSE,
                colnames = c("成交号", "订单号", "时间", "价格", "数量"),
                options = list(pageLength = 10))
    })
  })
}
