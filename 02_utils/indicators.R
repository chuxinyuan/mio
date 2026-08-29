# R/indicators.R — 信号生成（MACD 交叉 + Sharpe 排名）
# 沿用原书 evaluate/decisionGen 中的指标口径

library(zoo)
library(caTools)

# 入场判定：MACD 上穿 0 且 favor 达到 sh_thresh 分位
entry_func = \(v, sh_thresh) {
  cols = ncol(v) / 2
  as.numeric(
    v[1, 1:cols] <= 0 &
    v[2, 1:cols] > 0 &
    v[2, (cols + 1):(2 * cols)] >
      quantile(v[2, (cols + 1):(2 * cols)], sh_thresh, na.rm = TRUE)
  )
}

# 出场判定：MACD 下穿 0（与入场镜像，主动离场）
exit_func = \(v) {
  cols = ncol(v) / 2
  as.numeric(v[1, 1:cols] >= 0 & v[2, 1:cols] < 0)
}

# 由宽矩阵 close_mat / return_mat 生成 entry / exit / favor
# 向量化实现：入场/出场/分位全部用矩阵运算，避免 rollapply 逐窗口调用 R 函数
make_signals = \(close_mat, return_mat, n1, n2, n_sharpe, sh_thresh) {
  indic = zoo(
    runmean(close_mat, n1, endrule = "NA", align = "right") -
      runmean(close_mat, n2, endrule = "NA", align = "right"),
    order.by = index(close_mat)
  )
  names(indic) = names(close_mat)

  r_mean = zoo(
    runmean(return_mat, n1, endrule = "NA", align = "right"),
    order.by = index(return_mat)
  )
  favor = r_mean / runmean(
    (return_mat - r_mean)^2,
    n_sharpe,
    endrule = "NA",
    align = "right"
  )
  names(favor) = names(close_mat)

  ind = as.matrix(indic)
  fav = as.matrix(favor)
  nr = nrow(ind)
  nc = ncol(ind)

  # 入场：MACD 上穿 0 且当日 favor 超过当日横截面分位（首行 NA，与 rollapply 口径一致）
  fq = apply(fav, 1, quantile, probs = sh_thresh, na.rm = TRUE)
  entry = matrix(NA, nrow = nr, ncol = nc)
  if (nr >= 2) {
    entry[2:nr, ] = as.numeric(
      ind[1:(nr - 1), , drop = FALSE] <= 0 &
        ind[2:nr, , drop = FALSE] > 0 &
        fav[2:nr, , drop = FALSE] > matrix(fq[2:nr], nrow = nr - 1, ncol = nc)
    )
  }
  entry = zoo(entry, order.by = index(close_mat))
  names(entry) = names(close_mat)

  # 出场：MACD 下穿 0（与入场镜像，主动离场）
  exit = matrix(NA, nrow = nr, ncol = nc)
  if (nr >= 2) {
    exit[2:nr, ] = as.numeric(
      ind[1:(nr - 1), , drop = FALSE] >= 0 &
        ind[2:nr, , drop = FALSE] < 0
    )
  }
  exit = zoo(exit, order.by = index(close_mat))
  names(exit) = names(close_mat)

  list(entry = entry, exit = exit, favor = favor)
}

# 最新一期的信号（今日建议）：entry==1 的标的按 favor 降序
latest_signals = \(data, n1, n2, n_sharpe, sh_thresh) {
  sig = make_signals(
    data$close,
    data$return,
    n1,
    n2,
    n_sharpe,
    sh_thresh
  )
  entry = as.numeric(sig$entry[nrow(sig$entry), ])
  favor = as.numeric(sig$favor[nrow(sig$favor), ])
  dt = data.table(symbol = names(data$close), entry = entry, favor = favor)
  dt[order(-favor)]
}
