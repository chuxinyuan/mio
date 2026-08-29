# R/data_eastmoney.R — 东方财富行情获取（上交所）
# 依赖：httr + jsonlite + data.table

library(httr)
library(jsonlite)
library(data.table)

adjust_code = function(adjust) {
  switch(adjust, qfq = "1", hfq = "2", none = "0", "1")
}

secid = function(code) paste0("1.", code)   # 1 = 上交所

# 带重试的 GET JSON
get_json = function(url, query = list(), n = 3L, timeout_s = 20) {
  ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
  last = NULL
  for (i in seq_len(n)) {
    res = tryCatch(
      GET(
        url,
        query = query,
        timeout(timeout_s),
        add_headers("User-Agent" = ua, "Referer" = "https://quote.eastmoney.com/")
      ),
      error = \(e) NULL
    )
    if (!is.null(res) && status_code(res) == 200) {
      txt = content(res, as = "text", encoding = "UTF-8")
      parsed = tryCatch(fromJSON(txt), error = \(e) NULL)
      if (!is.null(parsed)) return(parsed)
    }
    last = res
    Sys.sleep(1.0 * i)
  }
  if (!is.null(last)) stop_for_status(last)
  stop("数据获取失败：", url)
}

# 上证50成分
fetch_sse50 = function() {
  parsed = get_json(
    "http://push2.eastmoney.com/api/qt/clist/get",
    query = list(
      pn = 1, pz = 200, po = 1, np = 1,
      fltt = 2, invt = 2, fs = "b:BK0611",
      fields = "f12,f14"
    )
  )
  diff = parsed$data$diff
  if (is.null(diff)) return(data.table(code = character(), name = character()))
  dt = as.data.table(diff)
  setnames(dt, c("f12", "f14"), c("code", "name"))
  dt[, board := BOARD]
  dt[, .(code, name, board)]
}

# 前复权日K（字段顺序：date, open, close, high, low, volume, ...）
# from / adjust 取自 config.R 全局常量
fetch_daily = function(code, to = Sys.Date()) {
  parsed = get_json(
    "http://push2his.eastmoney.com/api/qt/stock/kline/get",
    query = list(
      secid   = secid(code),
      fields1 = "f1,f2,f3,f4,f5,f6",
      fields2 = "f51,f52,f53,f54,f55,f56,f57,f58,f59,f60,f61",
      klt     = 101,
      fqt     = adjust_code(ADJUST),
      beg     = gsub("-", "", as.character(FROM)),
      end     = gsub("-", "", as.character(to))
    ),
    timeout_s = 30
  )
  klines = parsed$data$klines
  if (is.null(klines) || length(klines) == 0) {
    return(
      data.table(
        date = character(),
        open = numeric(),
        high = numeric(),
        low = numeric(),
        close = numeric(),
        volume = numeric()
      )
    )
  }
  m = tstrsplit(klines, ",", fixed = TRUE)
  dt = data.table(
    date   = m[[1]],
    open   = as.numeric(m[[2]]),
    close  = as.numeric(m[[3]]),
    high   = as.numeric(m[[4]]),
    low    = as.numeric(m[[5]]),
    volume = as.numeric(m[[6]])
  )
  dt[, symbol := code]
  dt[, .(symbol, date, open, high, low, close, volume)]
}

# 实时行情（最新价/今开/最高/最低/成交量/成交额/涨跌/涨跌幅）
# 优先批量 clist/get（一次取全池，fltt=2 下价格/涨跌额已为元、涨跌幅已为 %），失败回退逐只
fetch_realtime = function(codes) {
  dt = tryCatch(fetch_realtime_batch(codes), error = \(e) data.table())
  if (nrow(dt) > 0) return(dt)

  # 回退：逐只 stock/get（字段为分，需 /100）
  res = lapply(codes, \(code) {
    parsed = get_json(
      "http://push2.eastmoney.com/api/qt/stock/get",
      query = list(
        secid = secid(code),
        fields = "f43,f44,f45,f46,f47,f48,f57,f58,f169,f170"
      ),
      timeout_s = 15
    )
    d = parsed$data
    if (is.null(d)) return(NULL)
    data.table(
      code    = code,
      name    = ifelse(is.null(d$f58), NA_character_, d$f58),
      price   = (d$f43 %||% NA_real_) / 100,
      open    = (d$f46 %||% NA_real_) / 100,
      high    = (d$f44 %||% NA_real_) / 100,
      low     = (d$f45 %||% NA_real_) / 100,
      volume  = d$f47 %||% NA_real_,          # 单位：手
      amount  = d$f48 %||% NA_real_,          # 单位：元
      change  = (d$f169 %||% NA_real_) / 100, # 涨跌额（元）
      pct     = (d$f170 %||% NA_real_) / 100  # 涨跌幅（%）
    )
  })
  rbindlist(res, fill = TRUE)
}

# 批量实时行情（单次 clist/get 取全池；codes 非空时过滤）
fetch_realtime_batch = function(codes = NULL, pool_fs = "b:BK0611") {
  parsed = get_json(
    "http://push2.eastmoney.com/api/qt/clist/get",
    query = list(
      pn = 1, pz = 100, po = 1, np = 1,
      fltt = 2, invt = 2, fid = "f3",
      fs = pool_fs,
      fields = "f12,f14,f2,f3,f4,f5,f6,f15,f16,f17,f18"
    ),
    timeout_s = 15
  )
  dt = parse_realtime_batch(parsed$data$diff)
  if (!is.null(codes) && length(codes) && nrow(dt) > 0)
    dt = dt[code %in% codes]
  dt
}

# 纯解析：clist/get 的 diff → 统一 data.table（可离线测试）
parse_realtime_batch = function(diff) {
  if (is.null(diff)) return(data.table())
  d = as.data.table(diff)
  if (nrow(d) == 0) return(data.table())
  d = d[
    ,
    .(
      code = as.character(f12),
      name = f14,
      price = as.numeric(f2),
      open = as.numeric(f17),
      high = as.numeric(f15),
      low = as.numeric(f16),
      volume = as.numeric(f5),
      amount = as.numeric(f6),
      change = as.numeric(f4),
      pct = as.numeric(f3)
    )
  ]
  d
}

`%||%` = function(a, b) if (is.null(a)) b else a

# 全量刷新标的池历史行情入库（前复权漂移 → 删旧插新，逐只限速）
# from 取自 config.R 全局变量
refresh_universe = function(
  con = NULL,
  to = Sys.Date(),
  throttle_s = 1,
  verbose = TRUE
) {
  own = is.null(con)
  if (own) con = connect_db()
  on.exit(if (own) dbDisconnect(con), add = TRUE)

  init_db(con)
  sse = fetch_sse50()
  if (verbose) cat("上证50成分:", nrow(sse), "只\n")
  upsert_symbols(con, sse)

  for (i in seq_len(nrow(sse))) {
    code = sse$code[i]
    d = tryCatch(fetch_daily(code, to = to), error = \(e) NULL)
    if (is.null(d) || nrow(d) == 0) {
      if (verbose) cat(sprintf("[%02d/%02d] %s 失败\n", i, nrow(sse), code))
      next
    }
    replace_bars(con, code, d)
    if (verbose) cat(
      sprintf(
        "[%02d/%02d] %s %s %d 行\n",
        i, nrow(sse), code,
        sse$name[i], nrow(d)
      )
    )
    Sys.sleep(throttle_s)
  }
  invisible(sse)
}
