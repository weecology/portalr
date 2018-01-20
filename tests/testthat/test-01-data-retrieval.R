context("Check data retrieval")

test_that("download_observations works", {
  expect_error(download_observations(".", release_only = FALSE), NA)
  expect_error(download_observations("."), NA)
})

test_that("user given path returns list length 5", {
  data <- loadData(".")
  expect_equal(length(data), 5)
})

test_that("loadData returns list of length 5", {
  data = loadData("repo")
  expect_equal(length(data), 5)
})

test_that("rodent data.frame has correct column names", {
  data <- loadData(".")
  rats = data[[1]]
  rat_cols = colnames(rats)
  expect_true('month' %in% rat_cols)
  expect_true('year' %in% rat_cols)
  expect_true('plot' %in% rat_cols)
  expect_true('species' %in% rat_cols)
  expect_true('period' %in% rat_cols)
  expect_true('day' %in% rat_cols)
})

test_that("species data.frame has correct column names", {
  data <- loadData(".")
  sp = data[[2]]
  sp_cols = colnames(sp)
  expect_true('species' %in% sp_cols)
  expect_true('unidentified' %in% sp_cols)
  expect_true('censustarget' %in% sp_cols)
  expect_true('granivore' %in% sp_cols)
})
