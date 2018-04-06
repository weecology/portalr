context("Check data retrieval")

test_that("download_observations works", {
  expect_error(download_observations(".", release_only = FALSE), NA)
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
  data_tables <- load_data(".") # should be latest release based on previous tests
  expect_false(check_for_newer_data(data_tables$newmoons_table))
  expect_true(check_for_newer_data(data_tables$newmoons_table, release_only = FALSE))

  data_tables <- load_data("repo")
  expect_false(check_for_newer_data(data_tables$newmoons_table))
  expect_false(check_for_newer_data(data_tables$newmoons_table, release_only = FALSE))
})
