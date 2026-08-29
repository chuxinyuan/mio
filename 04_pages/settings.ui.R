# 04_pages/settings.ui.R — 设置

settings_ui = \(id) {
  ns = NS(id)
  tabItem(
    tabName = "settings",
    box(
      title = "交易与账户参数（保存后对回测与模拟盘生效）",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      numericInput(ns("starting_cash"), "初始资金（元）", value = NULL),
      numericInput(
        ns("max_assets"),
        "最大持仓数",
        value = NULL,
        min = 1,
        max = 50
      ),
      numericInput(
        ns("commission_rate"),
        "佣金费率",
        value = NULL,
        step = 0.0001
      ),
      numericInput(
        ns("stamp_duty"),
        "印花税（卖出）",
        value = NULL,
        step = 0.0001
      ),
      numericInput(
        ns("slip_factor"),
        "滑点",
        value = NULL,
        step = 0.0005
      ),
      numericInput(
        ns("spread_adjust"),
        "价差",
        value = NULL,
        step = 0.001
      ),
      numericInput(
        ns("flat_commission"),
        "固定佣金",
        value = NULL,
        step = 0.1
      ),
      numericInput(
        ns("per_share_commission"),
        "每手佣金",
        value = NULL,
        step = 0.001
      ),
      br(),
      div(
        class = "cyber-btns",
        actionButton(ns("save"), "保存设置", status = "info", width = "100%"),
        actionButton(ns("reset"), "恢复默认", status = "info", width = "100%")
      )
    )
  )
}
