# global.R — 全局初始化：加载库与核心函数、连接数据库
# 约定：从项目根目录运行（runApp(".")），路径用相对路径

library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(echarts4r)
library(DT)
library(zoo)
library(data.table)

source("config.R")
source("01_settings/path.R")
source("01_settings/color.R")
source("00_fetch/data_eastmoney.R")
suppressMessages(source("02_utils/db.R"))
suppressMessages(source("02_utils/indicators.R"))
suppressMessages(source("02_utils/backtest.R"))
suppressMessages(source("02_utils/optimize.R"))
suppressMessages(source("02_utils/account.R"))
source("03_components/charts.R")
source("03_components/value_box.R")

# 页面模块（进全局环境，供 ui/server 共用）
source("04_pages/market.ui.R")
source("04_pages/market.server.R")
source("04_pages/strategy.ui.R")
source("04_pages/strategy.server.R")
source("04_pages/order.ui.R")
source("04_pages/order.server.R")
source("04_pages/account.ui.R")
source("04_pages/account.server.R")
source("04_pages/log.ui.R")
source("04_pages/log.server.R")
source("04_pages/settings.ui.R")
source("04_pages/settings.server.R")

# 数据库连接（会话级单连接）
con = connect_db()
init_db(con)
init_account(con)
onStop(\() try(dbDisconnect(con), silent = TRUE))

# 共享响应式状态
rv = reactiveValues(
  symbols = get_symbols(con),
  bars    = load_bars(con),
  data    = NULL,
  settings = load_settings(con),
  refresh = 0
)

reload = function() {
  bars = load_bars(con)
  rv$bars = bars
  if (nrow(bars) > 0) {
    d = prepare_data(bars)
    d$return = make_return(d$close)
    rv$data = d
  } else {
    rv$data = NULL
  }
  rv$symbols = get_symbols(con)
  rv$refresh = isolate(rv$refresh) + 1
}

reload()
