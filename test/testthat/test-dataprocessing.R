source("data_processing.R")
library(testthat)
context("checks data_processing functions")

test_that("user given path returns list length 5", {
  data = loadData("../")
  expect_that(length(data), equals(5))
})

test_that("argument repo returns list length 5", {
  data = loadData("repo")
  expect_that(length(data), equals(5))
})

test_that("required column names in rodent df", {
  data = loadData("repo")
  rats = data[[1]]
  rat_cols = colnames(rats)
  expect_true('mo' %in% rat_cols)
  expect_true('yr' %in% rat_cols)
  expect_true('plot' %in% rat_cols)
  expect_true('species' %in% rat_cols)
  expect_true('period' %in% rat_cols)
  expect_true('dy' %in% rat_cols)
})

# test_that("required column names in species df", {
#   data = loadData("repo")
#   sp = data[[2]]
#   sp_cols = colnames(sp)
#   expect_true('species' %in% sp_cols)
#   expect_true('yr' %in% rat_cols)
#   expect_true('plot' %in% rat_cols)
#   expect_true('species' %in% rat_cols)
#   expect_true('period' %in% rat_cols)
#   expect_true('dy' %in% rat_cols)
# })
