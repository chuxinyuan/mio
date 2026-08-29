# 04_pages/order.ui.R — 订单管理

order_ui = \(id) {
  ns = NS(id)
  tabItem(
    tabName = "order",
    fluidRow(
      box(
        title = "下单",
        width = 4,
        status = "danger",
        solidHeader = TRUE,
        div(
          class = "cyber-btns",
          selectInput(ns("sym"), "标的", choices = NULL),
          selectInput(
            ns("side"),
            "方向",
            choices = c("买入" = "buy", "卖出" = "sell")
          ),
          numericInput(
            ns("qty"),
            "数量（股）",
            value = 100,
            min = 100,
            step = 100
          ),
          textInput(ns("price"), "价格", value = ""),
          actionButton(
            ns("submit"),
            "提交订单",
            status = "info",
            width = "100%"
          ),
          actionButton(
            ns("match"),
            "撮合订单",
            status = "info",
            width = "100%"
          ),
          br(),
          br(),
          selectInput(ns("cancel_id"), "撤单订单", choices = NULL),
          actionButton(
            ns("cancel"),
            "撤销订单",
            status = "info",
            width = "100%"
          )
        )
      ),
      box(
        title = "当前订单",
        width = 8,
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("orders"))
      )
    ),
    fluidRow(
      box(
        title = "成交回报",
        width = 12,
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("fills"))
      )
    )
  )
}
