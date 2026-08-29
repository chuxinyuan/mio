# 04_pages/strategy.server.R — 策略控制

strategy_server = function(id, con, rv) {
  moduleServer(id, \(input, output, session) {
    ns = session$ns

    params = reactive({
      list(
        param = c(
          n1 = input$n1,
          n_fact = input$n2 / input$n1,
          n_sharpe = input$n_sharpe,
          sh_thresh = input$sh_thresh
        ),
        year = input$year
      )
    })

    bt = eventReactive(input$run_bt, {
      req(!is.null(rv$data))
      p = params()
      res = evaluate(
        rv$data,
        p$param,
        year = p$year,
        settings = rv$settings,
        transform = FALSE,
        return_data = TRUE
      )
      list(
        equity = res$equity,
        dates = index(rv$data$close),
        param = p$param
      )
    })

    output$equity = renderEcharts4r({
      req(bt())
      keep = !is.na(bt()$equity)
      d = data.table(ts = bt()$dates[keep], equity = as.numeric(bt()$equity[keep]))
      equity_chart(d)
    })

    output$metrics = renderPrint({
      req(bt())
      m = calc_metrics(bt()$equity)
      cat("训练年份:", params()$year, "\n")
      cat("总收益:", round(m$total_return * 100, 2), "%\n")
      cat("年化收益:", round(m$annualized * 100, 2), "%\n")
      cat("夏普比率:", round(m$sharpe, 2), "\n")
      cat("最大回撤:", round(m$max_drawdown * 100, 2), "%\n")
      cat("回测天数:", m$n_days, "\n")
    })

    observeEvent(input$optimize, {
      req(!is.null(rv$data))
      showNotification("开始参数优化（可能较慢）...", type = "message")
      best = tryCatch(
        optimize_params(rv$data, year = input$year),
        error = \(e) {
          showNotification(paste("优化失败:", e$message), type = "error")
          NULL
        }
      )
      req(!is.null(best))
      updateNumericInput(session, "n1", value = round(best[["n1"]]))
      updateNumericInput(session, "n2", value = round(best[["n_fact"]] * best[["n1"]]))
      updateNumericInput(session, "n_sharpe", value = round(best[["n_sharpe"]]))
      updateNumericInput(session, "sh_thresh", value = round(best[["sh_thresh"]], 3))
      showNotification("优化完成，参数已回填", type = "message")
    })

    observeEvent(input$refresh, {
      showNotification("开始刷新数据...", type = "message")
      sse = tryCatch(
        refresh_universe(con, verbose = FALSE),
        error = \(e) {
          showNotification(paste("刷新失败:", e$message), type = "error")
          NULL
        }
      )
      reload()
      showNotification(
        if (is.null(sse)) "刷新失败" else paste("刷新完成", nrow(sse), "只"),
        type = if (is.null(sse)) "error" else "message"
      )
    })

    observeEvent(input$auto_trade, {
      req(!is.null(rv$data), nrow(rv$bars) > 0)
      px = latest_prices(con)
      price_map = setNames(px$price, px$symbol)
      res = tryCatch(
        auto_trade(
          con,
          rv$data,
          input$n1,
          input$n2,
          input$n_sharpe,
          input$sh_thresh,
          rv$settings$max_assets,
          price_map
        ),
        error = \(e) {
          showNotification(paste("自动下单失败:", e$message), type = "error")
          NULL
        }
      )
      if (!is.null(res)) {
        n_ok = sum(vapply(res, \(r) isTRUE(r$ok), logical(1)))
        showNotification(
          paste0("已按信号下单（成功 ", n_ok, " / ", length(res), " 笔）"),
          type = if (n_ok == length(res)) "message" else "warning"
        )
      }
    })

    output$signals = renderDT({
      req(!is.null(rv$data))
      sig = latest_signals(
        rv$data,
        input$n1,
        input$n2,
        input$n_sharpe,
        input$sh_thresh
      )
      names = rv$symbols[, .(symbol = code, name)]
      sig = merge(sig, names, by = "symbol", all.x = TRUE)
      top = sig[entry == 1, .(symbol, name, favor = round(favor, 3))][1:min(rv$settings$max_assets, .N)]
      datatable(
        top,
        rownames = FALSE,
        colnames = c("代码", "名称", "评分"),
        options = list(dom = "t", pageLength = 10)
      )
    })
  })
}
