# 03_components/value_box.R — 赛博 KPI 卡（sparkline + 涨跌箭头）

# 内联 SVG sparkline（纯函数，可在 renderUI 中使用）
inline_sparkline = \(values, color, width = 90, height = 24) {
  v = as.numeric(values)
  v = v[!is.na(v)]
  if (length(v) < 2) return(HTML(""))
  min_v = min(v)
  max_v = max(v)
  rng = max_v - min_v
  if (rng <= 0) rng = 1
  x = seq(1, width, length.out = length(v))
  y = height - (v - min_v) / rng * (height - 4) - 2
  pts = paste0(round(x, 1), ",", round(y, 1), collapse = " ")
  HTML(sprintf(
    '<svg width="%d" height="%d" class="cyber-spark"><polyline points="%s" fill="none" stroke="%s" stroke-width="1.5" stroke-linejoin="round" stroke-linecap="round"/></svg>',
    width, height, pts, color
  ))
}

change_arrow = \(x) ifelse(x >= 0, "▲", "▼")

value_box = \(value, label, accent = CYAN, change = NULL, sparkline = NULL) {
  show_change = !is.null(change) && !is.na(change) && change != 0
  tags$div(
    class = "cyber-box",
    style = paste0("--accent:", accent, ";"),
    tags$div(class = "cyber-box-value", value),
    tags$div(class = "cyber-box-label", label),
    tags$div(
      class = "cyber-box-foot",
      if (show_change)
        tags$span(
          class = "cyber-box-change",
          style = paste0("color:", ifelse(change >= 0, RED_UP, GREEN_DOWN), ";"),
          HTML(paste0(change_arrow(change), " ", fmt_signed(change)))
        ) else NULL,
      if (!is.null(sparkline))
        tags$span(class = "cyber-box-spark", inline_sparkline(sparkline, accent)) else NULL
    )
  )
}
