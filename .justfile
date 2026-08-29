default: shiny

# 启动 Shiny 应用（本地访问）
shiny:
    Rscript -e 'shiny::runApp(".")'

# 首次/更新行情数据
refresh:
    Rscript 00_fetch/refresh.R
