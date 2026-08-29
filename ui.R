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
  header = dashboardHeader(),
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
    tags$style(
      HTML(
        paste0(
          "@import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@500;700&family=JetBrains+Mono:wght@400;600&display=swap');\n",
          css_vars, "
      body, body.dark-mode {
        background: var(--bg) !important;
        font-family: 'JetBrains Mono', 'Cascadia Code', 'Courier New', monospace !important;
        color: var(--text-main) !important;
      }

      ::-webkit-scrollbar { width: 8px; height: 8px; }
      ::-webkit-scrollbar-track { background: var(--bg); }
      ::-webkit-scrollbar-thumb { background: rgba(0,240,255,0.35); border-radius: 4px; }
      ::-webkit-scrollbar-thumb:hover { background: rgba(0,240,255,0.6); }

      .content-wrapper, .right-side, .content {
        background:
          linear-gradient(rgba(0,240,255,0.04) 1px, transparent 1px),
          linear-gradient(90deg, rgba(0,240,255,0.04) 1px, transparent 1px),
          radial-gradient(ellipse at top, #0b1120 0%, var(--bg) 60%) !important;
        background-size: 42px 42px, 42px 42px, 100% 100%;
      }

      .main-header.navbar, .navbar-white {
        background: rgba(6,9,16,0.92) !important;
        border-bottom: 1px solid rgba(0,240,255,0.3);
      }
      .navbar .custom-switch { display: none !important; }
      .navbar-brand, .brand-link .brand-text {
        font-family: 'Orbitron', 'JetBrains Mono', sans-serif !important;
        color: var(--cyan) !important;
        letter-spacing: 3px;
        text-shadow: 0 0 12px rgba(0,240,255,0.9), 0 0 30px rgba(0,240,255,0.4);
      }

      .main-sidebar {
        background: var(--nav) !important;
        border-right: 1px solid rgba(0,240,255,0.25);
      }
      .nav-sidebar .nav-link, .nav-sidebar .nav-item > .nav-link {
        color: var(--text-cyan) !important;
        letter-spacing: 1px;
      }
      .nav-sidebar .nav-link:hover { color: var(--cyan) !important; text-shadow: 0 0 8px var(--cyan); }
      .nav-sidebar .nav-link.active {
        background: rgba(0,240,255,0.10) !important;
        color: var(--cyan) !important;
        border-left: 3px solid var(--cyan);
        box-shadow: inset 0 0 14px rgba(0,240,255,0.15);
      }

      .card {
        background: rgba(9,13,22,0.92) !important;
        border: 1px solid rgba(0,240,255,0.28) !important;
        border-radius: 4px;
        box-shadow: 0 0 16px rgba(0,240,255,0.12), inset 0 0 24px rgba(0,240,255,0.02);
      }
      .card-header {
        background: rgba(0,240,255,0.06) !important;
        border-bottom: 1px solid rgba(0,240,255,0.25) !important;
      }
      .card-title {
        color: var(--cyan) !important;
        letter-spacing: 2px;
        text-transform: uppercase;
        text-shadow: 0 0 8px rgba(0,240,255,0.7);
      }

      /* 赛博价值卡 */
      .cyber-box {
        position: relative;
        background: rgba(9,13,22,0.94);
        border: 1px solid var(--accent);
        border-radius: 4px;
        padding: 14px 16px 12px 16px;
        box-shadow: 0 0 16px rgba(0,240,255,0.10), inset 0 0 20px rgba(0,0,0,0.35);
        overflow: hidden;
        animation: cyberpulse 4s ease-in-out infinite;
        min-height: 104px;
        margin-bottom: 18px;
        display: flex;
        flex-direction: column;
      }
      .cyber-box::before {
        content: ''; position: absolute; top: -1px; left: -1px;
        width: 14px; height: 14px;
        border-top: 2px solid var(--accent); border-left: 2px solid var(--accent);
        filter: drop-shadow(0 0 4px var(--accent));
      }
      .cyber-box::after {
        content: ''; position: absolute; bottom: -1px; right: -1px;
        width: 14px; height: 14px;
        border-bottom: 2px solid var(--accent); border-right: 2px solid var(--accent);
        filter: drop-shadow(0 0 4px var(--accent));
      }
      .cyber-box-value {
        font-size: 24px; font-weight: 600; color: var(--accent);
        text-shadow: 0 0 12px var(--accent);
        font-variant-numeric: tabular-nums; white-space: nowrap;
      }
      .cyber-box-label {
        color: var(--text-dim); font-size: 11px; letter-spacing: 2px;
        text-transform: uppercase; margin-top: 4px;
        flex: 1 1 auto;
      }
      .cyber-box-foot {
        margin-top: 8px;
        display: flex;
        align-items: flex-end;
        justify-content: space-between;
        min-height: 24px;
      }
      .cyber-box-change { font-size: 14px; font-variant-numeric: tabular-nums; }
      .cyber-box-spark { line-height: 0; }
      @keyframes cyberpulse {
        0%, 100% { box-shadow: 0 0 16px rgba(0,240,255,0.10), inset 0 0 20px rgba(0,0,0,0.35); }
        50% { box-shadow: 0 0 24px rgba(0,240,255,0.20), inset 0 0 20px rgba(0,0,0,0.35); }
      }

      /* 表格 */
      table.dataTable { background: transparent !important; color: var(--text-main); }
      table.dataTable thead th {
        background: #0c1220 !important; color: var(--cyan) !important;
        border-bottom: 1px solid rgba(0,240,255,0.35) !important;
        letter-spacing: 1px;
      }
      table.dataTable tbody td { border-top: 1px solid rgba(0,240,255,0.10); }
      table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd { background: #0a0f1a !important; }
      table.dataTable.display tbody tr.even { background: #070a12 !important; }
      table.dataTable.hover tbody tr:hover { background: rgba(0,240,255,0.08) !important; }
      .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_paginate { color: var(--text-dim); }
      .dataTables_wrapper .dataTables_paginate .paginate_button { color: var(--text-cyan) !important; }

      /* 输入控件 */
      .form-control { background: #0a0f1a !important; color: var(--text-main) !important; border: 1px solid rgba(0,240,255,0.25) !important; }
      .form-control:focus { border-color: var(--cyan) !important; box-shadow: 0 0 8px rgba(0,240,255,0.4) !important; }

      .btn { letter-spacing: 1px; }
      .echarts4r { width: 100%; }
    "
        )
      )
    ),
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
