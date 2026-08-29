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
    e_grid(height = c("56%", "22%")) |>
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
    e_datazoom(type = "slider", height = 16) |>
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

equity_chart = \(d) {
  d |>
    e_charts(ts) |>
    e_line(
      equity,
      name = "权益",
      lineStyle = list(
        width = 2,
        color = CYAN,
        shadowColor = "rgba(0,240,255,0.6)",
        shadowBlur = 10
      )
    ) |>
    e_area(
      equity,
      name = "权益",
      lineStyle = list(width = 2, color = CYAN),
      itemStyle = list(color = "rgba(0,240,255,0.20)")
    ) |>
    e_x_axis(
      axisLabel = list(color = AXIS_TEXT),
      axisLine = list(lineStyle = list(color = "rgba(0,240,255,0.3)"))
    ) |>
    e_y_axis(
      scale = TRUE,
      axisLabel = list(color = AXIS_TEXT),
      splitLine = list(lineStyle = list(color = "rgba(0,240,255,0.08)"))
    ) |>
    e_tooltip(
      trigger = "axis",
      backgroundColor = "rgba(9,13,22,0.95)",
      borderColor = "rgba(0,240,255,0.5)",
      textStyle = list(color = TEXT_MAIN)
    ) |>
    e_legend(textStyle = list(color = TEXT_CYAN)) |>
    e_theme("dark")
}

fmt_money = \(x) paste0("¥", format(round(x, 2), big.mark = ",", scientific = FALSE))
fmt_signed = \(x) paste0(ifelse(x >= 0, "+", ""), format(round(x, 2), big.mark = ",", scientific = FALSE))
