# refresh.R — 批量刷新上证 50 历史行情入库（首次建库 / 定期更新）
# 用法：cd <项目根> && Rscript 00_fetch/refresh.R

# 独立进程，不与 global.R 共享，需自行统一北京时间（Sys.Date()/时间戳正确）
Sys.setenv(TZ = "Asia/Shanghai")

source("config.R")
source("00_fetch/market_data.R")
source("01_settings/path.R")
source("02_utils/db.R")

con = connect_db()
on.exit(dbDisconnect(con), add = TRUE)

sse = refresh_universe(con, throttle_s = 1)
cat("\n完成：共", nrow(sse), "只标的\n")
