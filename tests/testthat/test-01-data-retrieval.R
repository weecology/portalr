context("Check data retrieval")

test_that("download_observations works", {
  expect_error(download_observations(".", release_only = FALSE), NA)
  expect_error(download_observations("."), NA)
})

test_that("loadData works", {
  expect_error(data_tables <- loadData("repo"), NA)
  expect_equal(length(data_tables), 5)
  expect_error(data_tables <- loadData("."), NA)
  expect_equal(length(data_tables), 5)
})

