# config.R — 全局配置
# 约定：从项目根目录运行，路径一律用相对路径

# ---- 策略/组合 ----
MAX_ASSETS = 10               # 组合最大持仓数
MAX_ITER   = 100              # 优化最大迭代次数
YEAR       = 2016             # 训练年份

# 参数优化范围与初值（n1, n_fact, n_sharpe, sh_thresh）
MIN_VAL      = c(n1 = 1, n_fact = 1, n_sharpe = 1, sh_thresh = .01)
MAX_VAL      = c(n1 = 150, n_fact = 5, n_sharpe = 200, sh_thresh = .99)
PARAM_NAUGHT = c(n1 = -2, n_fact = -2, n_sharpe = -2, sh_thresh = 0)

# ---- 数据源与存储 ----
UNIVERSE = "SSE50"            # 标的池：上证50
ADJUST   = "qfq"              # 复权：qfq 前复权 / hfq 后复权 / none 不复权
FROM     = "2016-01-01"       # 历史数据起始日
BOARD    = "SSE"              # 交易所：上交所

# ---- 交易/账户（默认值，可在 App 设置页覆盖并持久化） ----
STARTING_CASH        = 100000
SLIP_FACTOR          = 0.001
SPREAD_ADJUST        = 0.01
FLAT_COMMISSION      = 3.5
PER_SHARE_COMMISSION = 0
COMMISSION_RATE      = 0.0003    # 佣金 万3
STAMP_DUTY           = 0.0005    # 印花税 万5（卖出）
