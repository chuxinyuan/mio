# 04_pages/account.ui.R — 账户监控

account_ui = \(id) {
  ns = NS(id)
  tabItem(
    tabName = "account",
    uiOutput(ns("kpis")),
    fluidRow(
      box(
        title = "权益曲线",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        echarts4rOutput(ns("equity_curve"), height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "当前持仓",
        width = 12,
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("positions"))
      )
    )
  )
}
