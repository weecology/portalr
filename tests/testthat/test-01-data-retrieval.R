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
  expect_equal(names(data_tables),
               c("rodent_data", "species_table", "trapping_table",
                 "newmoons_table", "plots_table"))
  expect_error(data_tables <- load_data("."), NA)
  expect_equal(length(data_tables), 5)
  expect_equal(names(data_tables),
               c("rodent_data", "species_table", "trapping_table",
                 "newmoons_table", "plots_table"))
})

test_that("load_data downloads data if missing", {
  expect_error(data_tables <- load_data(tempdir(), download_if_missing = FALSE))
  expect_warning(data_tables <- load_data(tempdir()))
  expect_equal(length(data_tables), 5)
  expect_equal(names(data_tables),
               c("rodent_data", "species_table", "trapping_table",
                 "newmoons_table", "plots_table"))
  unlink(file.path(tempdir(), "PortalData"), recursive = TRUE)
})

test_that("check_for_newer_data works", {
  expect_false(check_for_newer_data("."))

  download_observations(".", version = "1.2.0")
  expect_true(check_for_newer_data("."))

  download_observations(".", version = "1.30.0")
  expect_true(check_for_newer_data("."))
})

test_that("load_plant_data works", {
  expect_error(data_tables <- load_plant_data("repo"), NA)
  expect_equal(length(data_tables), 5)
  expect_equal(names(data_tables),
               c("quadrat_data", "species_table", "census_table",
                 "date_table", "plots_table"))

  expect_error(data_tables <- load_plant_data("."), NA)
  expect_equal(length(data_tables), 5)
  expect_equal(names(data_tables),
               c("quadrat_data", "species_table", "census_table",
                 "date_table", "plots_table"))
})

test_that("load_plant_data downloads data if missing", {
  expect_error(data_tables <- load_plant_data(tempdir(), download_if_missing = FALSE))
  expect_warning(data_tables <- load_plant_data(tempdir()))
  expect_equal(length(data_tables), 5)
  expect_equal(names(data_tables),
               c("quadrat_data", "species_table", "census_table",
                 "date_table", "plots_table"))
})

test_that("load_ant_data works", {
  expect_error(data_tables <- load_ant_data("repo"), NA)
  expect_equal(length(data_tables), 4)
  expect_equal(names(data_tables),
               c("bait_data", "colony_data", "species_table",
                 "plots_table"))

  expect_error(data_tables <- load_ant_data("."), NA)
  expect_equal(length(data_tables), 4)
  expect_equal(names(data_tables),
               c("bait_data", "colony_data", "species_table",
                 "plots_table"))
})

test_that("load_ant_data downloads data if missing", {
  expect_error(data_tables <- load_ant_data(tempdir(), download_if_missing = FALSE))
  expect_warning(data_tables <- load_ant_data(tempdir()))
  expect_equal(length(data_tables), 4)
  expect_equal(names(data_tables),
               c("bait_data", "colony_data", "species_table",
                 "plots_table"))
})
