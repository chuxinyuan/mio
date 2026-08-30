# tests/testthat.R — testthat 入口（test_dir 驱动 tests/testthat/）
# 同时声明测试/CI 依赖（testthat/lintr）供 renv 锁定；本文件不被 source，仅静态声明

library(testthat)
library(lintr)
