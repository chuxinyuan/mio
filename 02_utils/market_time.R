# R/market_time.R — 交易时段判定与刷新节奏
# 收盘后（含午休/周末）页面停止高频刷新，仅静默探底以便次日开盘自动恢复

# 是否处于 A 股交易时段（周一至周五 09:30-11:30 / 13:00-15:00）
in_trading_hours = \(x = Sys.time()) {
  wd = as.POSIXlt(x)$wday
  if (wd %in% c(0, 6)) return(FALSE)
  t = format(x, "%H:%M")
  (t >= TRADING_START_AM && t <= TRADING_END_AM) ||
    (t >= TRADING_START_PM && t < TRADING_END_PM)
}

# 刷新节奏：交易时段按 ms，其余时间用 MARKET_WATCH_MS 静默探测
tick = \(ms, session) {
  invalidateLater(if (in_trading_hours()) ms else MARKET_WATCH_MS, session)
}
