# mio 项目规则与说明

## 项目概况

mio 是一个基于 R 的量化交易系统，以 bs4Dash（深色赛博朋克主题）+ ECharts 构建，标的池为**上证50（上交所）**。策略引擎沿用《Automated Trading with R》的组合模拟与 MACD+Sharpe 排名逻辑，数据与交易/账户/日志统一存于 SQLite 单文件。

## 目录结构

- `config.R` — 业务常量（策略/组合/交易参数）
- `00_fetch/` — 数据获取（`data_eastmoney.R` 取数 + 刷新）
- `01_settings/` — 配置（`color.R` 色板常量、`path.R` 路径常量）
- `02_utils/` — 引擎函数
  - `db.R` — SQLite 封装（建表、行情/交易/账户/日志/设置读写）
  - `indicators.R` — MACD 交叉 + Sharpe 排名信号（`make_signals` / `latest_signals`）
  - `backtest.R` — 数据准备、组合模拟 `simulate`、评估 `evaluate`、绩效 `calc_metrics`
  - `optimize.R` — 广义模式搜索参数优化 `optimize_params`
  - `account.R` — 模拟账户与订单撮合（paper trading）
- `03_components/` — 可复用组件（`charts.R` 图表、`value_box.R` KPI 卡）
- `04_pages/` — 页面模块，`.ui.R` 与 `.server.R` 成对（总览/行情/策略/订单/账户/日志/设置）
- `50_data/` — 数据目录（`mio.db` SQLite 单文件）
- `www/` — 静态资源（favicon、CSS）
- `global.R` / `ui.R` / `server.R` — bs4Dash 应用入口（根目录，深色赛博朋克主题）

## 数据约定

- 数据源：东方财富免费 HTTP 接口（`secid` 前缀 `1.` = 上交所）
- 复权：前复权（`fqt=1`）；因前复权会随除权整体漂移，**更新行情一律删旧插新（`replace_bars`），不做逐行追加**
- 数据库 Schema 见 `02_utils/db.R` 的 `init_db()`
- 标的池动态获取（`fetch_sse50()`，`fs=b:BK0611`），不写死成分

## 运行约定

- 首次/更新数据：`cd <项目根> && Rscript 00_fetch/refresh.R`
- 启动应用：`shiny::runApp(".")`
- 脚本入口先 `source("config.R")` 和 `01_settings/`，再 `source` 对应模块
- 东方财富接口可能限流：`refresh_universe` 已内置逐只限速与重试，失败标的跳过不中断

## 规则（必须遵守）

- 赋值统一用 `=`，不用 `<-`；多行管道最终结果用 `->` 赋值
- 管道统一用 `|>`，不用 `%>%`；管道符后换行写，不用单管道
- 命名：**常量用大写 + 下划线**（`MAX_ASSETS`），**其余变量/函数用小写 + 下划线**（`entry_func`），禁止驼峰；不用中文/拼音命名
- 运算符两端、逗号后、`if (`、`# ` 后加空格
- 参数名尽量写全，不省略
- **匿名函数用 `\(x)` lambda 语法**（需 R ≥ 4.1），如 `lapply(x, \(v) ...)`、`error = \(e) NULL`；**命名函数定义** `foo = function(...)` 保持 `function`
- **多参调用：要么整行放下、要么逐参换行**——一行放得下的短调用（`c(1, 2, 3)`、`list(a = 1)`）保持整行；凡跨行或较长/多命名参数的调用一律「左括号后换行、每参一行、右括号独立成行」，禁止悬垂缩进对齐（如 `foo(a,\n     b)`）写法
- 业务配置放 `config.R` 常量，色板放 `01_settings/color.R`，路径放 `01_settings/path.R`
- 路径一律用**相对路径**，禁止绝对路径；所有入口从项目根目录运行
- 行情/交易数据只通过 `02_utils/db.R` 的封装读写，不直接操作 SQLite

## 记忆约定

- 重要的项目事实（设计决定、踩坑原因、尝试过但失败的方案、为什么这么做）用 `memory` 工具保存，而不是只靠对话：
  - 存项目记忆：`memory({ mode: "add", content: "..." })`
  - 检索记忆：`memory({ mode: "search", query: "..." })`
  - 查跨项目用户偏好：`memory({ mode: "profile" })`
- 分工：**稳定、长期必须遵守的规则**写在本文件；**随工作增长的经验**交给 memory（含 auto-capture）沉淀
