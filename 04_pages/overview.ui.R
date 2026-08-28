# 04_pages/overview.ui.R — 总览首页

overview_ui = function(id) {
  ns = NS(id)
  tabItem(
    tabName = "overview",
    fluidRow(uiOutput(ns("kpis"))),
    fluidRow(
      box(title = "账户权益", width = 8, status = "primary", solidHeader = TRUE,
          echarts4rOutput(ns("equity"), height = "320px")),
      box(title = "今日信号（建议买入）", width = 4, status = "info", solidHeader = TRUE,
          DTOutput(ns("signals")))
    ),
    fluidRow(
      box(title = "最近日志", width = 12, status = "secondary", solidHeader = TRUE,
          DTOutput(ns("logs")))
    )
  )
}
