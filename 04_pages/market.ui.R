# 04_pages/market.ui.R — 行情展示

market_ui = \(id) {
  ns = NS(id)
  tabItem(
    tabName = "market",
    fluidRow(
      box(
        title = "个股 K 线（前复权）",
        width = 8,
        status = "primary",
        solidHeader = TRUE,
        selectInput(ns("sym"), "标的", choices = NULL),
        echarts4rOutput(ns("kline"), height = "460px")
      ),
      box(
        title = "实时行情（10 秒刷新）",
        width = 4,
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("rt_table"))
      )
    )
  )
}
