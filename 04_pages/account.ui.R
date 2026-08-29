# 04_pages/account.ui.R — 账户监控

account_ui = function(id) {
  ns = NS(id)
  tabItem(
    tabName = "account",
    uiOutput(ns("kpis")),
    fluidRow(
      box(
        title = "权益曲线",
        width = 8,
        status = "primary",
        solidHeader = TRUE,
        echarts4rOutput(ns("equity_curve"), height = "340px")
      ),
      box(
        title = "当前持仓",
        width = 4,
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("positions"))
      )
    )
  )
}
