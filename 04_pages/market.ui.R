# 04_pages/market.ui.R — 行情展示

market_ui = \(id) {
  ns = NS(id)
  tabItem(
    tabName = "market",
    fluidRow(
      box(
        title = "个股 K 线（后复权）",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        selectInput(ns("sym"), label = NULL, choices = NULL, width = "320px"),
        echarts4rOutput(ns("kline"), height = "460px")
      )
    ),
    fluidRow(
      box(
        title = "实时行情（10 秒刷新）",
        width = 12,
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("rt_table"))
      )
    )
  )
}
