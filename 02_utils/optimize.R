# R/optimize.R — 广义模式搜索参数优化（沿袭原书 optimizeFunc.R）

optimize_params = \(data, year) {

  max_iter = MAX_ITER
  delta_thresh = 0.05
  delta = delta_naught = 1
  sigma = 2

  param = param_naught = PARAM_NAUGHT
  np = length(param)

  optim_df = data.frame(matrix(NA, nrow = max_iter * (4 * np + 1), ncol = np + 1))
  names(optim_df) = c(names(param), "obj")
  o = 1

  fmin = fmin_naught = evaluate(data, param, year = year, negative = TRUE)
  optim_df[o, ] = c(param, fmin); o = o + 1

  for (k in 1:max_iter) {

    # SEARCH 子程序
    for (l in 1:np) {
      net = (2 * rbinom(np, 1, .5) - 1) * runif(np, delta, sigma * delta)
      for (m in c(-1, 1)) {
        testpoint = param + m * net
        ftest = evaluate(data, testpoint, year = year, negative = TRUE)
        optim_df[o, ] = c(testpoint, ftest); o = o + 1
      }
    }

    if (any(optim_df$obj[(o - (2 * np)):(o - 1)] < fmin)) {
      min_pos = which.min(optim_df$obj[(o - (2 * np)):(o - 1)])
      param = (optim_df[(o - (2 * np)):(o - 1), 1:np])[min_pos, ]
      fmin = (optim_df[(o - (2 * np)):(o - 1), np + 1])[min_pos]
      delta = sigma * delta
    } else {
      # POLL 子程序
      for (l in 1:np) {
        net = delta * as.numeric(1:np == l)
        for (m in c(-1, 1)) {
          testpoint = param + m * net
          ftest = evaluate(data, testpoint, year = year, negative = TRUE)
          optim_df[o, ] = c(testpoint, ftest); o = o + 1
        }
      }

      if (any(optim_df$obj[(o - (2 * np)):(o - 1)] < fmin)) {
        min_pos = which.min(optim_df$obj[(o - (2 * np)):(o - 1)])
        param = (optim_df[(o - (2 * np)):(o - 1), 1:np])[min_pos, ]
        fmin = (optim_df[(o - (2 * np)):(o - 1), np + 1])[min_pos]
        delta = sigma * delta
      } else {
        delta = delta / sigma
      }
    }

    # 步长过小则随机重启
    if (delta < delta_thresh) {
      delta = delta_naught
      fmin = fmin_naught
      param = param_naught + runif(n = np, min = -delta * sigma, max = delta * sigma)
      ftest = evaluate(data, param, year = year, negative = TRUE)
      optim_df[o, ] = c(param, ftest); o = o + 1
    }
  }

  # 返回未变换（实际）参数
  evaluate(
    data,
    optim_df[which.min(optim_df$obj), 1:np],
    year = year,
    transform_only = TRUE
  )
}
