# 03_components/charts.R — ECharts 图表组件（深色霓虹）

kline_chart = \(d) {
  d |>
    e_charts(date) |>
    e_candle(
      open,
      close,
      low,
      high,
      name = "K线",
      itemStyle = list(
        color = RED_UP,
        color0 = GREEN_DOWN,
        borderColor = RED_UP,
        borderColor0 = GREEN_DOWN
      )
    ) |>
    e_line(ma5, name = "MA5", lineStyle = list(width = 1, color = YELLOW)) |>
    e_line(ma10, name = "MA10", lineStyle = list(width = 1, color = CYAN)) |>
    e_line(ma20, name = "MA20", lineStyle = list(width = 1, color = MAGENTA)) |>
    e_bar(
      volume,
      name = "成交量",
      y_index = 1,
      itemStyle = list(color = "rgba(0,240,255,0.35)")
    ) |>
    e_grid(
      left = "1%",
      right = "1%",
      containLabel = TRUE,
      height = c("56%", "22%")
    ) |>
    e_x_axis(
      type = "category",
      axisLabel = list(color = AXIS_TEXT),
      axisLine = list(lineStyle = list(color = "rgba(0,240,255,0.3)"))
    ) |>
    e_y_axis(
      index = 0,
      scale = TRUE,
      axisLabel = list(color = AXIS_TEXT),
      splitLine = list(lineStyle = list(color = "rgba(0,240,255,0.08)"))
    ) |>
    e_y_axis(
      index = 1,
      scale = TRUE,
      axisLabel = list(show = FALSE),
      splitLine = list(show = FALSE)
    ) |>
    e_datazoom(type = "slider", height = 16, toolbox = FALSE) |>
    e_tooltip(
      trigger = "axis",
      axisPointer = list(type = "cross"),
      backgroundColor = "rgba(9,13,22,0.95)",
      borderColor = "rgba(0,240,255,0.5)",
      textStyle = list(color = TEXT_MAIN)
    ) |>
    e_legend(bottom = 20, textStyle = list(color = TEXT_CYAN)) |>
    e_theme("dark")
}

# 净值曲线（策略回测 + 账户权益共用）：渐变面积 + 起始基准线 + 最大回撤区间 + 缩放
equity_chart = \(d) {
  eq = as.numeric(d$equity)
  ts = as.character(d$ts)
  keep = !is.na(eq)
  eq = eq[keep]
  ts = ts[keep]

  baseline = if (length(eq)) eq[1] else 100000
  grad = htmlwidgets::JS(
    "new echarts.graphic.LinearGradient(0, 0, 0, 1, ",
    "[{offset:0,color:'rgba(0,240,255,0.35)'},{offset:1,color:'rgba(0,240,255,0.02)'}])"
  )

  dd = eq / cummax(eq) - 1
  trough = which.min(dd)
  peak = which.max(eq[seq_len(trough)])
  show_dd = length(eq) >= 2 && dd[trough] < 0

  fmt_js = "function(v){ if(v>=1e8) return (v/1e8).toFixed(2)+'亿'; return (v/1e4).toFixed(1)+'万'; }"

  e = d[keep, .(ts, equity)] |>
    e_charts(ts) |>
    e_line(
      equity,
      name = "权益",
      symbol = "none",
      lineStyle = list(
        width = 2,
        color = CYAN,
        shadowColor = "rgba(0,240,255,0.6)",
        shadowBlur = 10
      ),
      itemStyle = list(color = CYAN),
      areaStyle = list(color = grad)
    ) |>
    e_mark_line(
      data = list(yAxis = baseline),
      silent = TRUE,
      itemStyle = list(color = TEXT_DIM, type = "dashed", lineStyle = list(type = "dashed")),
      label = list(
        formatter = "起始",
        color = TEXT_DIM,
        position = "insideEndTop"
      )
    )

  if (show_dd) {
    e = e |>
      e_mark_area(
        data = list(
          list(xAxis = ts[peak], itemStyle = list(color = "rgba(255,59,48,0.10)")),
          list(xAxis = ts[trough], itemStyle = list(color = "rgba(255,59,48,0.10)"))
        ),
        silent = TRUE
      )
  }

  e |>
    e_datazoom(type = "slider", height = 16, toolbox = FALSE) |>
    e_grid(left = 8, right = 8, top = 40, bottom = 52, containLabel = TRUE) |>
    e_x_axis(
      axisLabel = list(color = AXIS_TEXT),
      axisLine = list(lineStyle = list(color = "rgba(0,240,255,0.3)"))
    ) |>
    e_y_axis(
      scale = TRUE,
      axisLabel = list(color = AXIS_TEXT, formatter = htmlwidgets::JS(fmt_js)),
      splitLine = list(lineStyle = list(color = "rgba(0,240,255,0.08)"))
    ) |>
    e_tooltip(
      trigger = "axis",
      axisPointer = list(type = "cross"),
      backgroundColor = "rgba(9,13,22,0.95)",
      borderColor = "rgba(0,240,255,0.5)",
      textStyle = list(color = TEXT_MAIN),
      formatter = htmlwidgets::JS(
        "function(params){ var p=params[0]; if(!p) return ''; ",
        "var label = p.axisValueLabel || p.axisValue; ",
        "var v = Array.isArray(p.value) ? p.value[1] : p.value; ",
        "if (v == null || isNaN(v)) return label + '<br/>权益: --'; ",
        "v = Number(v); ",
        "var s = v>=1e8 ? (v/1e8).toFixed(2)+'亿' : (v/1e4).toFixed(2)+'万'; ",
        "return label + '<br/>权益: ' + s; }"
      )
    ) |>
    e_legend(textStyle = list(color = TEXT_CYAN)) |>
    e_theme("dark")
}

fmt_money = \(x) paste0("¥", format(round(x, 2), big.mark = ",", scientific = FALSE))
fmt_signed = \(x) paste0(ifelse(x >= 0, "+", ""), format(round(x, 2), big.mark = ",", scientific = FALSE))
