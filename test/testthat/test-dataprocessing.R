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


