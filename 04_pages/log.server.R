# 04_pages/log.server.R — 系统日志

log_server = \(id, con, rv) {
  moduleServer(id, \(input, output, session) {

    clear_tick = reactiveVal(0L)

    observeEvent(input$clear, {
      clear_logs(con)
      clear_tick(clear_tick() + 1)
      showNotification("日志已清空", type = "message")
    })

    output$log_table = renderDT({
      tick(5000, session)
      clear_tick()                    # 清空后立即刷新表格
      d = log_view(con, limit = 300)
      datatable(
        d,
        rownames = FALSE,
        colnames = c("时间", "级别", "模块", "信息"),
        options = list(pageLength = 15)
      )
    })
  })
}
