# R/backtest.R — 组合回测引擎（数据准备 + 模拟 + 评估）
# simulate 沿用原书 compute/functions.R 的逐日组合模拟逻辑

library(zoo)
library(caTools)
library(data.table)

# ------------------------------
# 数据准备
# ------------------------------

# bars: data.table(symbol, date, open, high, low, close, volume)
# 返回宽 zoo 矩阵列表（行=日期，列=标的）
prepare_data = \(bars) {
  bars = bars[order(date)]
  w = \(col) {
    d = dcast(bars, date ~ symbol, value.var = col)
    m = as.matrix(d[, -1, drop = FALSE])
    zoo(m, order.by = as.Date(d$date))
  }
  list(
    close = w("close"),
    open = w("open"),
    high = w("high"),
    low = w("low"),
    volume = w("volume")
  )
}

# 沿用原书 return.R 口径：return_mat = close_mat / lag(close_mat, -1) - 1（首行 NA）
make_return = \(close_mat) {
  na_pad = zoo(
    matrix(NA, nrow = 1, ncol = ncol(close_mat)),
    order.by = index(close_mat)[1]
  )
  names(na_pad) = names(close_mat)
  rbind(na_pad, (close_mat / lag(close_mat, k = -1)) - 1)
}

# ------------------------------
# 组合模拟（沿袭原书 simulate）
# 注意：此处如实扣除 flat_commission；原书中 "- flatCommission" 因独立成行实为死代码未生效
# ------------------------------

equ_na = \(v) {
  o = which(!is.na(v))[1]
  ifelse(is.na(o), length(v) + 1, o)
}

simulate = \(
  open_mat,
  close_mat,
  entry,
  exit,
  favor,
  max_lookback,
  max_assets,
  starting_cash,
  slip_factor,
  spread_adjust,
  flat_commission,
  per_share_commission,
  verbose = FALSE,
  fail_thresh = 0,
  init_pos = NULL,
  init_price = NULL
) {

  if (
    any(dim(entry) != dim(exit)) |
    any(dim(exit) != dim(favor)) |
    any(dim(favor) != dim(close_mat)) |
    any(dim(close_mat) != dim(open_mat))
  )
    stop("Mismatching dimensions in entry, exit, favor, close_mat, or open_mat.")

  if (
    any(names(entry) != names(exit)) |
    any(names(exit) != names(favor)) |
    any(names(favor) != names(close_mat)) |
    any(names(close_mat) != names(open_mat)) |
    is.null(names(entry)) |
    is.null(names(exit)) |
    is.null(names(favor)) |
    is.null(names(close_mat)) |
    is.null(names(open_mat))
  )
    stop("Mismatching or missing column names in entry, exit, favor, close_mat, or open_mat.")

  # 转普通矩阵加速（避免循环内 [.zoo / Ops.zoo 方法分派；列名即标的）
  favor = replace(as.matrix(favor), is.na(as.matrix(favor)), 0)
  open_mat = as.matrix(open_mat)
  close_mat = as.matrix(close_mat)
  entry = as.matrix(entry)
  exit = as.matrix(exit)

  n_pos = 0
  cash_vec = rep(starting_cash, times = nrow(close_mat))
  syms = colnames(close_mat)

  pos_qty = entry_price = matrix(0, ncol = ncol(close_mat), nrow = nrow(close_mat))
  colnames(pos_qty) = colnames(entry_price) = syms

  if (!is.null(init_pos) & !is.null(init_price)) {
    pos_qty[1:max_lookback, ] = matrix(
      init_pos,
      ncol = length(init_pos),
      nrow = max_lookback,
      byrow = TRUE
    )
    entry_price[1:max_lookback, ] = matrix(
      init_price,
      ncol = length(init_price),
      nrow = max_lookback,
      byrow = TRUE
    )
  }

  equity = rep(NA, nrow(close_mat))

  rm_na = pmax(
    apply(favor, 2, equ_na),
    apply(entry, 2, equ_na),
    apply(exit, 2, equ_na)
  )

  for (j in 1:ncol(entry)) {
    to_rm = rm_na[j]
    if (to_rm > (max_lookback + 1) & to_rm < nrow(entry)) {
      favor[1:(to_rm - 1), j] = NA
      entry[1:(to_rm - 1), j] = NA
      exit[1:(to_rm - 1), j]  = NA
    }
  }

  # 区间过短（nrow-1 < max_lookback 时 `:` 会生成递减序列误执行）→ 直接返回
  if (nrow(close_mat) - 1 < max_lookback) {
    return(
      list(
        equity = equity,
        cash = cash_vec,
        pos_qty = pos_qty,
        entry_price = entry_price
      )
    )
  }

  for (i in max_lookback:(nrow(close_mat) - 1)) {

    cash_vec[i + 1] = cash_vec[i]
    pos_qty[i + 1, ] = as.numeric(pos_qty[i, ])
    entry_price[i + 1, ] = as.numeric(entry_price[i, ])

    long_pos = syms[which(pos_qty[i, ] > 0)]
    short_pos = syms[which(pos_qty[i, ] < 0)]
    n_pos = length(long_pos) + length(short_pos)

    long_trigger = setdiff(syms[which(entry[i, ] == 1)], long_pos)
    short_trigger = setdiff(syms[which(entry[i, ] == -1)], short_pos)
    trigger = c(long_trigger, short_trigger)

    if (length(trigger) > max_assets) {
      keep_trigger = trigger[order(c(as.numeric(favor[i, long_trigger]),
                                     -as.numeric(favor[i, short_trigger])),
                                   decreasing = TRUE)][1:max_assets]
      long_trigger = long_trigger[long_trigger %in% keep_trigger]
      short_trigger = short_trigger[short_trigger %in% keep_trigger]
      trigger = c(long_trigger, short_trigger)
    }

    trigger_type = c(rep(1, length(long_trigger)), rep(-1, length(short_trigger)))

    long_exit_trigger = long_pos[long_pos %in% syms[which(exit[i, ] == 1 | exit[i, ] == 999)]]
    short_exit_trigger = short_pos[short_pos %in% syms[which(exit[i, ] == -1 | exit[i, ] == 999)]]
    exit_trigger = c(long_exit_trigger, short_exit_trigger)

    need_to_exit = max((length(trigger) - length(exit_trigger)) - (max_assets - n_pos), 0)

    if (need_to_exit > 0) {
      to_exit_long_pos = setdiff(long_pos, exit_trigger)
      to_exit_short_pos = setdiff(short_pos, exit_trigger)
      to_exit = character(0)

      for (counter in 1:need_to_exit) {
        if (length(to_exit_long_pos) > 0 & length(to_exit_short_pos) > 0) {
          if (min(favor[i, to_exit_long_pos]) < min(-favor[i, to_exit_short_pos])) {
            pull_min = which.min(favor[i, to_exit_long_pos])
            to_exit = c(to_exit, to_exit_long_pos[pull_min])
            to_exit_long_pos = to_exit_long_pos[-pull_min]
          } else {
            pull_min = which.min(-favor[i, to_exit_short_pos])
            to_exit = c(to_exit, to_exit_short_pos[pull_min])
            to_exit_short_pos = to_exit_short_pos[-pull_min]
          }
        } else if (length(to_exit_long_pos) > 0 & length(to_exit_short_pos) == 0) {
          pull_min = which.min(favor[i, to_exit_long_pos])
          to_exit = c(to_exit, to_exit_long_pos[pull_min])
          to_exit_long_pos = to_exit_long_pos[-pull_min]
        } else if (length(to_exit_long_pos) == 0 & length(to_exit_short_pos) > 0) {
          pull_min = which.min(-favor[i, to_exit_short_pos])
          to_exit = c(to_exit, to_exit_short_pos[pull_min])
          to_exit_short_pos = to_exit_short_pos[-pull_min]
        }
      }

      long_exit_trigger = c(long_exit_trigger, long_pos[long_pos %in% to_exit])
      short_exit_trigger = c(short_exit_trigger, short_pos[short_pos %in% to_exit])
    }

    exit_trigger = c(long_exit_trigger, short_exit_trigger)
    exit_trigger_type = c(rep(1, length(long_exit_trigger)), rep(-1, length(short_exit_trigger)))

    if (length(exit_trigger) > 0) {
      for (j in 1:length(exit_trigger)) {
        exit_price = as.numeric(open_mat[i + 1, exit_trigger[j]])
        effective_price = exit_price * (1 - exit_trigger_type[j] * slip_factor) -
          exit_trigger_type[j] * (per_share_commission + spread_adjust)

        if (exit_trigger_type[j] == 1) {
          cash_vec[i + 1] = cash_vec[i + 1] + (as.numeric(pos_qty[i, exit_trigger[j]]) * effective_price) - flat_commission
        } else {
          cash_vec[i + 1] = cash_vec[i + 1] - (as.numeric(pos_qty[i, exit_trigger[j]]) *
            (2 * as.numeric(entry_price[i, exit_trigger[j]]) - effective_price)) - flat_commission
        }

        pos_qty[i + 1, exit_trigger[j]] = 0
        entry_price[i + 1, exit_trigger[j]] = 0
        n_pos = n_pos - 1
      }
    }

    if (length(trigger) > 0) {
      for (j in 1:length(trigger)) {
        entry_price_cur = as.numeric(open_mat[i + 1, trigger[j]])
        effective_price = entry_price_cur * (1 + trigger_type[j] * slip_factor) +
          trigger_type[j] * (per_share_commission + spread_adjust)

        pos_qty[i + 1, trigger[j]] = trigger_type[j] *
          floor(((cash_vec[i + 1] - flat_commission) / (max_assets - n_pos)) / effective_price)

        entry_price[i + 1, trigger[j]] = effective_price

        cash_vec[i + 1] = cash_vec[i + 1] -
          (trigger_type[j] * as.numeric(pos_qty[i + 1, trigger[j]]) * effective_price) - flat_commission

        n_pos = n_pos + 1
      }
    }

    equity[i] = cash_vec[i + 1]
    for (s in syms[which(pos_qty[i + 1, ] > 0)]) {
      equity[i] = equity[i] + as.numeric(pos_qty[i + 1, s]) * as.numeric(open_mat[i + 1, s])
    }
    for (s in syms[which(pos_qty[i + 1, ] < 0)]) {
      equity[i] = equity[i] - as.numeric(pos_qty[i + 1, s]) *
        (2 * as.numeric(entry_price[i + 1, s]) - as.numeric(open_mat[i + 1, s]))
    }

    if (equity[i] < fail_thresh) {
      warning("\n*** Failure Threshold Breached ***\n")
      break
    }
  }

  list(equity = equity, cash = cash_vec, pos_qty = pos_qty, entry_price = entry_price)
}

# ------------------------------
# 评估（沿袭原书 evaluate）
# ------------------------------

evaluate = \(
  data,
  param,
  year,
  settings = NULL,
  transform = TRUE,
  negative = FALSE,
  transform_only = FALSE,
  return_data = FALSE,
  account_params = NULL
) {

  if (is.null(settings)) settings = default_settings()

  if (transform | transform_only) {
    param = MIN_VAL + (MAX_VAL - MIN_VAL) * (1 + exp(-unlist(param)))^-1
    if (transform_only) return(param)
  }

  max_assets = settings$max_assets

  n1 = max(round(param[["n1"]]), 2)
  n2 = max(round(param[["n_fact"]] * param[["n1"]]), 3, n1 + 1)
  n_sharpe = max(round(param[["n_sharpe"]]), 2)
  sh_thresh = max(0, min(param[["sh_thresh"]], .99))
  max_lookback = max(n1, n2, n_sharpe) + 1

  period = index(data$close) >= as.Date(paste0(year[1], "-01-01")) &
    index(data$close) < as.Date(paste0(year[length(year)] + 1, "-01-01"))

  period = period |
    ((seq_len(nrow(data$close)) > (which(period)[1] - max_lookback)) &
       (seq_len(nrow(data$close)) <= (which(period)[sum(period)]) + 1))

  close_mat = data$close[period, ]
  open_mat  = data$open[period, ]
  sub_return = data$return[period, ]

  sig = make_signals(close_mat, sub_return, n1, n2, n_sharpe, sh_thresh)

  args = list(
    open_mat = open_mat,
    close_mat = close_mat,
    entry = sig$entry,
    exit = sig$exit,
    favor = sig$favor,
    max_lookback = max_lookback,
    max_assets = max_assets,
    slip_factor = settings$slip_factor,
    spread_adjust = settings$spread_adjust,
    flat_commission = settings$flat_commission,
    per_share_commission = settings$per_share_commission,
    verbose = FALSE,
    fail_thresh = 0
  )

  if (!is.null(account_params)) {
    args$starting_cash = account_params[["cash"]]
    args$init_pos = account_params[["pos_qty"]]
    args$init_price = account_params[["entry_price"]]
  } else {
    args$starting_cash = settings$starting_cash
  }

  results = do.call(simulate, args)

  if (!return_data) {
    v = results[["equity"]]
    returns = (v[-1] / v[-length(v)]) - 1
    out = mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE)
    if (!is.nan(out)) {
      if (negative) {
        return(-out)
      } else {
        return(out)
      }
    } else {
      return(0)
    }
  } else {
    results
  }
}

# ------------------------------
# 回测入口 + 绩效
# ------------------------------

run_backtest = \(bars, param, year, settings = NULL) {
  data = prepare_data(bars)
  data$return = make_return(data$close)
  results = evaluate(data, param, year = year, settings = settings, return_data = TRUE)
  equity = results[["equity"]]
  dates = index(data$close)
  list(equity = equity, dates = dates, results = results)
}

calc_metrics = \(equity, dates = NULL) {
  v = equity[!is.na(equity)]
  if (length(v) < 2) return(NULL)
  ret = (v[-1] / v[-length(v)]) - 1
  total = v[length(v)] / v[1] - 1
  n = length(ret)
  ann = (1 + total)^(252 / n) - 1
  sharpe = mean(ret) / sd(ret) * sqrt(252)
  dd = v / cummax(v) - 1
  mdd = min(dd)
  list(
    total_return = total,
    annualized = ann,
    sharpe = sharpe,
    max_drawdown = mdd,
    n_days = n
  )
}
