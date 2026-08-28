# refresh.R — 批量刷新上证50历史行情入库（首次建库 / 定期更新）
# 用法：cd <项目根> && Rscript 00_fetch/refresh.R

source("config.R")
source("01_settings/path.R")
source("02_utils/db.R")
source("00_fetch/data_eastmoney.R")

con = connect_db()
on.exit(dbDisconnect(con), add = TRUE)

sse = refresh_universe(con, throttle_s = 1)
cat("\n完成：共", nrow(sse), "只标的\n")
