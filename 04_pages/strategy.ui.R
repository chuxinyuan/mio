# 04_pages/strategy.ui.R — 策略控制

strategy_ui = \(id) {
  ns = NS(id)
  tabItem(
    tabName = "strategy",
    fluidRow(
      box(
        title = "策略参数",
        width = 4,
        status = "warning",
        solidHeader = TRUE,
        numericInput(
          ns("n1"),
          "快线周期 n1",
          value = N1_DEFAULT,
          min = 2,
          max = 150
        ),
        numericInput(
          ns("n2"),
          "慢线周期 n2",
          value = N2_DEFAULT,
          min = 3,
          max = 300
        ),
        numericInput(
          ns("n_sharpe"),
          "Sharpe 排名窗口",
          value = N_SHARPE_DEFAULT,
          min = 2,
          max = 200
        ),
        numericInput(
          ns("sh_thresh"),
          "入场分位 sh_thresh",
          value = SH_THRESH_DEFAULT,
          min = 0,
          max = 0.99,
          step = 0.05
        ),
        numericInput(
          ns("year"),
          "训练年份",
          value = YEAR,
          min = 2016,
          max = 2024
        ),
        br(),
        actionButton(
          ns("run_bt"),
          "运行回测",
          icon = icon("play"),
          status = "primary",
          width = "100%"
        ),
        br(), br(),
        actionButton(
          ns("optimize"),
          "参数优化",
          icon = icon("cogs"),
          status = "info",
          width = "100%"
        ),
        br(), br(),
        actionButton(
          ns("auto_trade"),
          "自动下单",
          icon = icon("paper-plane"),
          status = "success",
          width = "100%"
        ),
        br(), br(),
        actionButton(
          ns("refresh"),
          "刷新数据",
          icon = icon("sync"),
          status = "secondary",
          width = "100%"
        )
      ),
      box(
        title = "回测净值曲线",
        width = 8,
        status = "primary",
        solidHeader = TRUE,
        echarts4rOutput(ns("equity"), height = "320px"),
        verbatimTextOutput(ns("metrics"))
      )
    ),
    fluidRow(
      box(
        title = "今日信号（建议买入，按 favor 降序）",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        DTOutput(ns("signals"))
      )
    )
  )
}
