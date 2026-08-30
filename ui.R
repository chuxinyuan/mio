# ui.R — 赛博朋克深色交易终端界面

css_vars = sprintf(
  ":root { --cyan: %s; --magenta: %s; --purple: %s; --yellow: %s; --red-up: %s; --green-down: %s; --text-main: %s; --text-dim: %s; --text-cyan: %s; --bg: %s; --panel: %s; --nav: %s; }",
  CYAN, MAGENTA, PURPLE, YELLOW, RED_UP, GREEN_DOWN,
  TEXT_MAIN, TEXT_DIM, TEXT_CYAN, BG_MAIN, BG_PANEL, BG_NAV
)

ui = dashboardPage(
  dark = TRUE,
  help = FALSE,
  fullscreen = FALSE,
  scrollToTop = FALSE,
  header = dashboardHeader(
    rightUi = tags$li(class = "dropdown", uiOutput("nav_quotes"))
  ),
  sidebar = dashboardSidebar(
    dashboardBrand(
      title = "量化交易系统",
      color = "info",
      image = "logo.png"
    ),
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("行情展示", tabName = "market", icon = icon("chart-line")),
      menuItem("策略控制", tabName = "strategy", icon = icon("cogs")),
      menuItem("订单管理", tabName = "order", icon = icon("exchange-alt")),
      menuItem("账户监控", tabName = "account", icon = icon("wallet")),
      menuItem("系统日志", tabName = "log", icon = icon("file-alt")),
      menuItem("系统设置", tabName = "settings", icon = icon("cog"))
    )
  ),
  body = dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$script("document.title = '量化交易系统';"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$meta(property = "og:title", content = "量化交易系统"),
      tags$meta(property = "og:description", content = "基于 R 语言的量化交易系统，用于模拟交易和教学演示"),
      tags$meta(property = "og:image", content = paste0(APP_URL, "logo.png")),
      tags$meta(property = "og:url", content = APP_URL),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(name = "wechat-share-title", content = "量化交易系统"),
      tags$meta(name = "wechat-share-desc", content = "基于 R 语言的量化交易系统，用于模拟交易和教学演示"),
      tags$meta(name = "wechat-share-image", content = paste0(APP_URL, "logo.png")),
      tags$meta(itemprop = "name", content = "量化交易系统"),
      tags$meta(itemprop = "description", content = "基于 R 语言的量化交易系统，用于模拟交易和教学演示"),
      tags$meta(itemprop = "image", content = paste0(APP_URL, "logo.png")),
      tags$link(rel = "stylesheet", href = "style.css")
    ),
    tags$style(HTML(css_vars)),
    tabItems(
      market_ui("market"),
      strategy_ui("strategy"),
      order_ui("order"),
      account_ui("account"),
      log_ui("log"),
      settings_ui("settings")
    )
  )
)
