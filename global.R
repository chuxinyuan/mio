# global.R — 全局初始化：加载依赖、核心源码与数据库，构建共享响应式状态
# 约定：从项目根目录运行（runApp(".")），路径一律用相对路径

# 加载依赖包
library(shiny)        # 框架
library(bs4Dash)      # 深色赛博主题布局
library(shinyWidgets) # 增强控件
library(echarts4r)    # ECharts 图表（K线/净值曲线）
library(DT)           # 数据表
library(zoo)          # 时间序列（回测宽矩阵）
library(data.table)   # 数据操作
library(future)       # 异步调度
library(promises)     # 异步回调
library(shinyjs)      # 按钮启停等 JS 交互

# DataTables 中文界面文案（datatable() 全局默认，所有表格统一中文）
DT_LANG_ZH = list(
  search = "搜索",
  lengthMenu = "显示 _MENU_ 条",
  info = "显示 _START_ 至 _END_ 条，共 _TOTAL_ 条",
  infoEmpty = "显示 0 条",
  infoFiltered = "（筛选自 _MAX_ 条）",
  zeroRecords = "未找到匹配记录",
  emptyTable = "暂无数据",
  loadingRecords = "加载中...",
  processing = "处理中...",
  paginate = list(first = "首页", previous = "上一页", `next` = "下一页", last = "末页")
)
options(DT.options = list(language = DT_LANG_ZH))

# 异步调度：参数优化等耗时任务放后台进程，不阻塞 UI
future::plan(future::multisession, workers = 2)

# 加载项目源码（按依赖顺序，与目录编号一致：配置 → 数据 → 设置 → 工具 → 组件 → 页面）
source("config.R")                                    # 业务常量（根目录）
source("00_fetch/market_data.R")                      # 数据获取（腾讯历史 + 东财延时行情）
source("01_settings/path.R")                          # 路径常量
source("01_settings/color.R")                         # 色板常量
suppressMessages(source("02_utils/db.R"))             # SQLite 存储封装
suppressMessages(source("02_utils/indicators.R"))     # 信号生成（MACD + Sharpe）
suppressMessages(source("02_utils/backtest.R"))       # 回测引擎（准备/模拟/评估）
suppressMessages(source("02_utils/optimize.R"))       # 参数优化
suppressMessages(source("02_utils/account.R"))        # 模拟账户与撮合
suppressMessages(source("02_utils/market_time.R"))    # 交易时段判定与刷新节奏
suppressMessages(source("02_utils/views.R"))          # 视图层（页面显示数据组装）
source("03_components/charts.R")                      # 图表组件
source("03_components/value_box.R")                   # KPI 卡组件

# 页面模块（.ui.R 与 .server.R 成对，进全局环境供 ui / server 共用）
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

# 数据库连接（应用级单例：所有会话共用同一连接；应用退出时关闭）
con = connect_db()
init_db(con)          # 建表（幂等）
init_account(con)     # 首次写入初始资金快照
onStop(\() try(dbDisconnect(con), silent = TRUE))

# ---- 共享响应式状态 rv 契约（各页面模块共用，契约见 AGENTS.md「共享状态 rv 契约」）----
# symbols : data.table(code, name, board, is_valid)  标的池，reload()/refresh 后更新
# bars    : data.table(symbol, date, open, high, low, close, volume)  全量日K原始行情
# data    : list(close/open/high/low/volume = zoo 宽矩阵, return = zoo) 或 NULL
#           回测准备数据，由 reload() 构建（页面用 req_data(rv) 校验后再用）
# settings: list(max_assets, starting_cash, slip_factor, flat_commission, commission_rate, stamp_duty)
#           交易/账户参数，DB 覆盖默认值（设置页保存后更新）
# refresh : integer  数据刷新计数器，reload() 自增（触发相关输出重算）
rv = reactiveValues(
  symbols = get_symbols(con),
  bars = load_bars(con),
  data = NULL,
  settings = load_settings(con),
  refresh = 0
)

# 重建行情数据：加载 bars → prepare_data 宽矩阵 → 生成收益率矩阵
reload = \() {
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

reload()    # 启动时初始化 rv$data
