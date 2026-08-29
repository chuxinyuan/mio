# 04_pages/log.ui.R — 系统日志

log_ui = \(id) {
  ns = NS(id)
  tabItem(
    tabName = "log",
    box(
      title = "系统日志",
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      DTOutput(ns("log_table")),
      br(),
      div(
        class = "log-clear-wrap",
        actionButton(
          ns("clear"),
          "清空日志",
          icon = icon("broom"),
          status = "danger",
          width = "200px"
        )
      )
    )
  )
}
