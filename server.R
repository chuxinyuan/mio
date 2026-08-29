# server.R — 服务端

server = \(input, output, session) {
  market_server("market", con, rv)
  strategy_server("strategy", con, rv)
  order_server("order", con, rv)
  account_server("account", con, rv)
  log_server("log", con, rv)
  settings_server("settings", con, rv)
}
