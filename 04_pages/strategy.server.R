# 04_pages/strategy.server.R — 策略控制

strategy_server = \(id, con, rv) {
  moduleServer(id, \(input, output, session) {
    ns = session$ns
    optimizing = reactiveVal(FALSE)

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
        dates = res$dates,
        param = p$param
      )
    })

    output$equity = renderEcharts4r({
      req(bt())
      keep = !is.na(bt()$equity)
      d = data.table(ts = bt()$dates[keep], equity = as.numeric(bt()$equity[keep]))
      equity_chart(d)
    })

    output$metrics = renderUI({
      req(bt())
      m = calc_metrics(bt()$equity)
      if (is.null(m)) return(fluidRow(column(12, p("回测数据不足", style = "color:var(--mio-dim);"))))
      pct_col = \(x) ifelse(x >= 0, RED_UP, GREEN_DOWN)
      fluidRow(
        column(2, value_box(as.character(params()$year), "训练年份", PURPLE)),
        column(2, value_box(paste0(round2(m$total_return * 100, 2), "%"), "总收益", pct_col(m$total_return))),
        column(2, value_box(paste0(round2(m$annualized * 100, 2), "%"), "年化收益", pct_col(m$annualized))),
        column(2, value_box(round2(m$sharpe, 2), "夏普比率", CYAN)),
        column(2, value_box(paste0(round2(m$max_drawdown * 100, 2), "%"), "最大回撤", GREEN_DOWN)),
        column(2, value_box(m$n_days, "回测天数", TEXT_CYAN))
      )
    })

    observeEvent(input$optimize, {
      req(!is.null(rv$data))
      if (optimizing()) {
        showNotification("参数优化进行中，请稍候", type = "warning")
        return()
      }
      optimizing(TRUE)
      shinyjs::disable("optimize")
      showNotification("开始参数优化（后台运行，可继续操作）...", type = "message")

      data = isolate(rv$data)
      year = isolate(input$year)
      pr = future::future(
        {
          library(zoo)
          library(caTools)
          library(data.table)
          optimize_params(data, year = year)
        },
        seed = TRUE
      )

      promises::then(
        pr,
        onFulfilled = \(best) {
          req(!is.null(best))
          updateNumericInput(session, "n1", value = round(best[["n1"]]))
          updateNumericInput(session, "n2", value = round(best[["n_fact"]] * best[["n1"]]))
          updateNumericInput(session, "n_sharpe", value = round(best[["n_sharpe"]]))
          updateNumericInput(session, "sh_thresh", value = round(best[["sh_thresh"]], 3))
          showNotification("优化完成，参数已回填", type = "message")
        },
        onRejected = \(e) {
          showNotification(paste("优化失败:", conditionMessage(e)), type = "error")
        }
      )
      promises::finally(
        pr,
        onFinally = \() {
          optimizing(FALSE)
          shinyjs::enable("optimize")
        }
      )
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
      symbols_map = rv$symbols[, .(symbol = code, name)]
      sig = merge(sig, symbols_map, by = "symbol", all.x = TRUE)
      top = sig[entry == 1, .(symbol, name, favor = round2(favor, 2))][1:min(rv$settings$max_assets, .N)]
      datatable(
        top,
        rownames = FALSE,
        colnames = c("代码", "名称", "评分"),
        options = list(dom = "t", pageLength = 10)
      )
    })
  })
}
