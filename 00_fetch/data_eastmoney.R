# R/data_eastmoney.R — 行情数据获取（上交所）
# 历史日线：腾讯 fqkline（单一来源）；行情/标的池/指数：东方财富延时接口
# 依赖：httr + jsonlite + data.table

library(httr)
library(jsonlite)
library(data.table)

secid = \(code) paste0("1.", code)   # 1 = 上交所

# 带重试的 GET JSON
get_json = \(url, query = list(), n = 3L, timeout_s = 20, simplify = TRUE) {
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
      parsed = tryCatch(fromJSON(txt, simplifyVector = simplify), error = \(e) NULL)
      if (!is.null(parsed)) return(parsed)
    }
    last = res
    Sys.sleep(1.0 * i)
  }
  if (!is.null(last)) stop_for_status(last)
  stop("数据获取失败：", url)
}

# 依次尝试行情 host（path 形如 /api/qt/clist/get）
# 模拟/学习用途优先延时 15 分钟接口（主接口易被限流），失败再回退主接口
em_get = \(path, query = list(), n = 3L, timeout_s = 20) {
  last = NULL
  for (host in c(EM_HOST_DELAY, EM_HOST)) {
    parsed = tryCatch(
      get_json(paste0(host, path), query = query, n = n, timeout_s = timeout_s),
      error = \(e) {
        last = e
        NULL
      }
    )
    if (!is.null(parsed)) return(parsed)
  }
  stop(conditionMessage(last))
}

# 上证50成分
fetch_sse50 = \() {
  parsed = em_get(
    "/api/qt/clist/get",
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

# 日K（字段顺序：date, open, close, high, low, volume）
# 复权口径见 config.R ADJUST（后复权 hfq：长史不会取负、回测稳健）
# 历史日线单一来源：腾讯 fqkline
fetch_daily = \(code, to = Sys.Date()) {
  d = tryCatch(fetch_daily_tencent(code, from = FROM, to = to), error = \(e) NULL)
  if (is_valid_bars(d)) return(d)
  data.table(
    symbol = code, date = character(), open = numeric(),
    high = numeric(), low = numeric(), close = numeric(), volume = numeric()
  )
}

# 有效性检查：有数据且价格为正（复权数据异常取负则丢弃）
is_valid_bars = \(d) {
  !is.null(d) && nrow(d) > 0 && all(d$close > 0, na.rm = TRUE)
}

# 腾讯代码（上证 6 开头 → sh，其余 → sz）
tencent_symbol = \(code) paste0(ifelse(substr(code, 1, 1) == "6", "sh", "sz"), code)

# 腾讯日K（单一来源，稳定；接口每次最多返回 640 行，故按日期倒序分段回取）
# 复权按 config.R ADJUST（默认 hfq 后复权，长史不会取负）；行格式：date, open, close, high, low, volume
fetch_daily_tencent = \(code, from = FROM, to = Sys.Date()) {
  symbol = tencent_symbol(code)
  empty = data.table(
    symbol = code, date = character(), open = numeric(),
    high = numeric(), low = numeric(), close = numeric(), volume = numeric()
  )
  parts = list()
  end = as.Date(to)
  repeat {
    parsed = tryCatch(
      get_json(
        "https://web.ifzq.gtimg.cn/appstock/app/fqkline/get",
        query = list(
          param = sprintf("%s,day,%s,%s,640,%s", symbol, as.character(FROM), as.character(end), ADJUST)
        ),
        timeout_s = 20,
        simplify = FALSE
      ),
      error = \(e) NULL
    )
    rows = if (!is.null(parsed)) {
      parsed$data[[symbol]]$hfqday %||% parsed$data[[symbol]]$qfqday %||% parsed$data[[symbol]]$day
    } else {
      NULL
    }
    if (is.null(rows) || length(rows) == 0) break
    parts[[length(parts) + 1]] = rows
    first = as.Date(rows[[1]][[1]])
    if (first <= as.Date(FROM)) break
    end = first - 1
  }
  if (length(parts) == 0) return(empty)

  # 各段内部升序、段与段按新旧倒序收集 → 整体反转段序后拼接为时间正序
  klines = do.call(c, rev(parts))
  dt = rbindlist(
    lapply(klines, \(k) {
      data.table(
        date = k[[1]],
        open = as.numeric(k[[2]]),
        close = as.numeric(k[[3]]),
        high = as.numeric(k[[4]]),
        low = as.numeric(k[[5]]),
        volume = as.numeric(k[[6]])
      )
    })
  )
  dt[, symbol := code]
  dt[, .(symbol, date, open, high, low, close, volume)]
}

# 实时行情（最新价/今开/最高/最低/成交量/成交额/涨跌/涨跌幅）
# 优先批量 clist/get（一次取全池，fltt=2 下价格/涨跌额已为元、涨跌幅已为 %），失败回退逐只
fetch_realtime = \(codes) {
  dt = tryCatch(
    fetch_realtime_batch(codes), error = \(e) data.table()
  )
  if (nrow(dt) > 0) return(dt)

  # 回退：逐只 stock/get（字段为分，需 /100）
  res = lapply(codes, \(code) {
    parsed = em_get(
      "/api/qt/stock/get",
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
fetch_realtime_batch = \(codes = NULL, pool_fs = "b:BK0611") {
  parsed = em_get(
    "/api/qt/clist/get",
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

# 上证指数实时行情（导航栏展示；延时接口，字段为分需 /100）
fetch_index = \() {
  parsed = em_get(
    "/api/qt/stock/get",
    query = list(
      secid = "1.000001",
      fields = "f43,f44,f45,f46,f47,f48,f57,f58,f169,f170"
    ),
    timeout_s = 15
  )
  d = parsed$data
  if (is.null(d)) return(NULL)
  data.table(
    name = d$f58 %||% "上证指数",
    price = (d$f43 %||% NA_real_) / 100,
    change = (d$f169 %||% NA_real_) / 100,
    pct = (d$f170 %||% NA_real_) / 100
  )
}

# 纯解析：clist/get 的 diff → 统一 data.table（可离线测试）
parse_realtime_batch = \(diff) {
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

`%||%` = \(a, b) if (is.null(a)) b else a

# 全量刷新标的池历史行情入库（前复权漂移 → 删旧插新，逐只限速）
# from 取自 config.R 全局变量
refresh_universe = \(
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
  write_log(con, sprintf("开始刷新标的池行情（%d 只）", nrow(sse)), "info", "fetch")

  for (i in seq_len(nrow(sse))) {
    code = sse$code[i]
    d = tryCatch(fetch_daily(code, to = to), error = \(e) NULL)
    if (is.null(d) || nrow(d) == 0) {
      write_log(con, sprintf("[%02d/%02d] %s 获取失败", i, nrow(sse), code), "warn", "fetch")
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
  write_log(con, sprintf("标的池行情刷新完成（%d 只）", nrow(sse)), "info", "fetch")
  invisible(sse)
}
