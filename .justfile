default: shiny

# 启动 Shiny 应用（本地访问）
shiny:
    Rscript -e 'shiny::runApp(".")'

# 首次/更新行情数据
refresh:
    Rscript 00_fetch/refresh.R

# 运行单元测试（tests/testthat/，全部离线）
test:
    Rscript -e 'testthat::test_dir("tests/testthat")'
