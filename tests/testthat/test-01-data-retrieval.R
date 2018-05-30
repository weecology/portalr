context("Check data retrieval")

test_that("download_observations works", {
  expect_error(download_observations(".", version = "1.20.0"), NA)
  expect_error(download_observations(".", version = "1.5"), NA)
  expect_error(download_observations(".", version = "1.5.9"))
  expect_error(download_observations("."), NA)
})

test_that("load_data works", {
  expect_error(data_tables <- load_data("repo"), NA)
  expect_equal(length(data_tables), 5)
  expect_error(data_tables <- load_data("."), NA)
  expect_equal(length(data_tables), 5)
  expect_error(data_tables <- load_data(tempdir(), download_if_missing = FALSE))
  expect_warning(data_tables <- load_data(tempdir()))
  expect_equal(length(data_tables), 5)
})

test_that("check_for_newer_data works", {
  expect_false(check_for_newer_data("."))

  download_observations(".", version = "1.2.0")
  expect_true(check_for_newer_data("."))

  download_observations(".", version = "1.30.0")
  expect_true(check_for_newer_data("."))
})
