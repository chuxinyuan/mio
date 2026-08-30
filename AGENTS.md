# mio 项目规则与说明

## 项目概况

mio 是一个基于 R 的量化交易系统，以 bs4Dash（深色赛博朋克主题）+ ECharts 构建，标的池为**上证50（上交所）**。策略引擎沿用《Automated Trading with R》的组合模拟与 MACD+Sharpe 排名逻辑，数据与交易/账户/日志统一存于 SQLite 单文件。

## 目录结构

- `config.R` — 业务常量（策略/组合/交易参数）
- `00_fetch/` — 数据获取（`market_data.R` 取数 + 刷新）
- `01_settings/` — 配置（`color.R` 色板常量、`path.R` 路径常量）
- `02_utils/` — 引擎函数
  - `db.R` — SQLite 封装（建表、行情/交易/账户/日志/设置读写）
  - `indicators.R` — MACD 交叉 + Sharpe 排名信号（`make_signals` / `latest_signals`）
  - `backtest.R` — 数据准备、组合模拟 `simulate`、评估 `evaluate`、绩效 `calc_metrics`
  - `optimize.R` — 广义模式搜索参数优化 `optimize_params`
  - `account.R` — 模拟账户与订单撮合（paper trading）
  - `market_time.R` — 交易时段判定与刷新节奏（`in_trading_hours` / `tick`）
  - `views.R` — **视图层**：页面显示数据组装（纯函数，可单测；页面 server 只做取数→渲染）
- `03_components/` — 可复用组件（`charts.R` 图表、`value_box.R` KPI 卡）
- `04_pages/` — 页面模块，`.ui.R` 与 `.server.R` 成对（总览/行情/策略/订单/账户/日志/设置）
- `data/` — 数据目录（`mio.db` SQLite 单文件）
- `www/` — 静态资源（favicon、CSS）
- `global.R` / `ui.R` / `server.R` — bs4Dash 应用入口（根目录，深色赛博朋克主题）

## 数据约定

- 数据源：**历史日线单一来源为腾讯 fqkline**（稳定）；行情/标的池/上证指数走东方财富延时 15 分钟接口（`push2delay`，`secid` 前缀 `1.` = 上交所）
- 复权：**后复权（hfq）**——长史不会因累计分红取负，回测稳健（前复权 qfq 对高分红股长史会取负，破坏回测）；`replace_bars` 删旧插新保证一致性
- 数据库 Schema 见 `02_utils/db.R` 的 `init_db()`
- 标的池动态获取（`fetch_sse50()`，`fs=b:BK0611`），不写死成分

## 共享状态 rv 契约

`global.R` 的 `rv`（reactiveValues）是各页面模块共用的响应式状态，字段契约如下，页面只读、不修改：

| 字段 | 类型 | 说明 | 消费者 |
|---|---|---|---|
| `symbols` | data.table(code, name, board, is_valid) | 标的池，`reload()`/刷新后更新 | 各页标的选择/名称映射 |
| `bars` | data.table(symbol,date,open,high,low,close,volume) | 全量日K原始行情 | 行情/账户/自动下单 |
| `data` | list(close/open/high/low/volume=zoo, return=zoo) 或 NULL | 回测准备数据，`reload()` 构建 | 策略页（用 `req_data(rv)` 校验） |
| `settings` | list(max_assets, starting_cash, slip_factor, flat_commission, commission_rate, stamp_duty) | 交易/账户参数，DB 覆盖默认 | 回测/撮合/下单 |
| `refresh` | integer | 刷新计数器，`reload()` 自增 | 触发相关输出重算 |

- 名称映射统一用 `views.R` 的 `sym_map(rv$symbols)`；回测数据校验用 `req_data(rv)`
- 新增页面如需新字段：先在 `global.R` 与上表登记契约，再使用

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
- **函数定义统一用 `\(...)` lambda 语法**（需 R ≥ 4.1），如 `foo = \(x) x`、`error = \(e) NULL`、`\()` 表示零参；不再使用 `function(...)`
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
