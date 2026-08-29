# 04_pages/log.server.R — 系统日志

log_server = \(id, con, rv) {
  moduleServer(id, \(input, output, session) {
    ns = session$ns

    observeEvent(input$clear, {
      dbExecute(con, "DELETE FROM sys_log")
    })

    output$log_table = renderDT({
      invalidateLater(5000, session)
      datatable(
        get_logs(con, limit = 300),
        rownames = FALSE,
        colnames = c("时间", "级别", "模块", "信息"),
        options = list(pageLength = 15)
      )
    })
  })
}
