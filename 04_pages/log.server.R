# 04_pages/log.server.R — 系统日志

log_server = \(id, con, rv) {
  moduleServer(id, \(input, output, session) {
    ns = session$ns

    clear_tick = reactiveVal(0L)

    observeEvent(input$clear, {
      dbExecute(con, "DELETE FROM sys_log")
      clear_tick(clear_tick() + 1)
      showNotification("日志已清空", type = "message")
    })

    output$log_table = renderDT({
      tick(5000, session)
      clear_tick()                    # 清空后立即刷新表格
      d = get_logs(con, limit = 300)
      if (nrow(d) > 0) {
        d[, level := fcase(
          level == "info", "信息",
          level == "warn", "警告",
          level == "error", "错误",
          default = level
        )]
        d[, module := fcase(
          module == "fetch", "数据获取",
          module == "account", "账户",
          module == "system", "系统",
          module == "order", "订单",
          default = module
        )]
      }
      datatable(
        d,
        rownames = FALSE,
        colnames = c("时间", "级别", "模块", "信息"),
        options = list(pageLength = 15)
      )
    })
  })
}
