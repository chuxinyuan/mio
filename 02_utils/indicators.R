# R/indicators.R — 信号生成（MACD 交叉 + Sharpe 排名）
# 沿用原书 evaluate/decisionGen 中的指标口径

library(zoo)
library(caTools)

# 入场判定：MACD 上穿 0 且 favor 达到 sh_thresh 分位
entry_func = function(v, sh_thresh) {
  cols = ncol(v) / 2
  as.numeric(
    v[1, 1:cols] <= 0 &
    v[2, 1:cols] > 0 &
    v[2, (cols + 1):(2 * cols)] >
      quantile(v[2, (cols + 1):(2 * cols)], sh_thresh, na.rm = TRUE)
  )
}

# 由宽矩阵 close_mat / return_mat 生成 entry / exit / favor
make_signals = function(close_mat, return_mat, n1, n2, n_sharpe, sh_thresh) {
  indic = zoo(runmean(close_mat, n1, endrule = "NA", align = "right") -
                runmean(close_mat, n2, endrule = "NA", align = "right"),
              order.by = index(close_mat))
  names(indic) = names(close_mat)

  r_mean = zoo(runmean(return_mat, n1, endrule = "NA", align = "right"),
               order.by = index(return_mat))
  favor = r_mean / runmean((return_mat - r_mean)^2, n_sharpe, endrule = "NA", align = "right")
  names(favor) = names(close_mat)

  entry = rollapply(cbind(indic, favor),
                    FUN = function(v) entry_func(v, sh_thresh),
                    width = 2, fill = NA, align = "right", by.column = FALSE)
  names(entry) = names(close_mat)

  exit = zoo(matrix(0, ncol = ncol(close_mat), nrow = nrow(close_mat)),
             order.by = index(close_mat))
  names(exit) = names(close_mat)

  list(entry = entry, exit = exit, favor = favor)
}

# 最新一期的信号（今日建议）：entry==1 的标的按 favor 降序
latest_signals = function(data, n1, n2, n_sharpe, sh_thresh) {
  sig = make_signals(data$close, data$return, n1, n2, n_sharpe, sh_thresh)
  entry = as.numeric(sig$entry[nrow(sig$entry), ])
  favor = as.numeric(sig$favor[nrow(sig$favor), ])
  dt = data.table(symbol = names(data$close), entry = entry, favor = favor)
  dt[order(-favor)]
}
