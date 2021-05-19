context("Check data retrieval")

portal_data_path <- tempdir()

test_that("download_observations and check_for_newer_data work", {
    skip_on_cran() # these download checks take a while to run
    expect_error(download_observations(portal_data_path, version = "1.20.0"), NA)
    expect_true(check_for_newer_data(portal_data_path))
    httptest::without_internet({
        expect_false(check_for_newer_data(portal_data_path))
    })
    unlink(file.path(portal_data_path, "PortalData"), recursive = TRUE)

    expect_error(download_observations(portal_data_path, version = "1.6.0"), NA)
    expect_true(check_for_newer_data(portal_data_path))
    unlink(file.path(portal_data_path, "PortalData"), recursive = TRUE)
    expect_true(check_for_newer_data(portal_data_path))

    expect_error(download_observations(portal_data_path, version = "1.5.9"))
    expect_error(download_observations(portal_data_path, version = "1.000.0"))

    expect_error(download_observations(portal_data_path, from_zenodo = TRUE), NA)
    expect_false(check_for_newer_data(portal_data_path))
    #unlink(file.path(portal_data_path, "PortalData"), recursive = TRUE)

    expect_error(download_observations(portal_data_path), NA)
    expect_false(check_for_newer_data(portal_data_path))
    unlink(file.path(portal_data_path, "PortalData"), recursive = TRUE)
})

test_that("load_rodent_data downloads data if missing", {
    skip_on_cran()
    expect_error(data_tables <- load_rodent_data(portal_data_path, download_if_missing = FALSE))
    expect_warning(data_tables <- load_rodent_data(portal_data_path))
})


test_that("load_rodent_data has the right format", {
    skip_on_cran()
    expect_error(data_tables <- load_rodent_data("repo"), NA)
    expect_equal(length(data_tables), 5)
    expect_equal(names(data_tables),
                 c("rodent_data", "species_table", "trapping_table",
                   "newmoons_table", "plots_table"))

    data_tables <- load_rodent_data(portal_data_path)
    expect_equal(length(data_tables), 5)
    expect_equal(names(data_tables),
                 c("rodent_data", "species_table", "trapping_table",
                   "newmoons_table", "plots_table"))
})

test_that("load_plant_data has the right format", {
    skip_on_cran()
    expect_error(data_tables <- load_plant_data("repo"), NA)
    expect_equal(length(data_tables), 7)
    expect_equal(names(data_tables),
                 c("quadrat_data", "species_table", "census_table",
                   "date_table", "plots_table", "transect_data", "oldtransect_data"))

    expect_error(data_tables <- load_plant_data(portal_data_path), NA)
    expect_equal(length(data_tables), 7)
    expect_equal(names(data_tables),
                 c("quadrat_data", "species_table", "census_table",
                   "date_table", "plots_table", "transect_data", "oldtransect_data"))
})

test_that("load_ant_data works", {
    skip_on_cran()
    expect_error(data_tables <- load_ant_data("repo"), NA)
    expect_equal(length(data_tables), 4)
    expect_equal(names(data_tables),
                 c("bait_data", "colony_data", "species_table",
                   "plots_table"))

    expect_error(data_tables <- load_ant_data(portal_data_path), NA)
    expect_equal(length(data_tables), 4)
    expect_equal(names(data_tables),
                 c("bait_data", "colony_data", "species_table",
                   "plots_table"))
})

test_that("default data path functions work if unset", {
    Sys.unsetenv("PORTALR_DATA_PATH")
    expect_warning(result <- check_default_data_path(MESSAGE_FUN = warning),
                   "You don't appear to have a defined location for storing Portal data.")
    expect_false(result)

    m <- capture_messages(check_default_data_path())
    expect_match(m, "You don't appear to have a defined location for storing Portal data.", all = FALSE)
    expect_match(m, "Call .+ if you wish to set the default data path.", all = FALSE)
    expect_match(m, "Portal data will be downloaded into .+ otherwise.", all = FALSE)

    expect_error(use_default_data_path())

    data_path <- tempdir()
    expect_error(m <- capture_messages(use_default_data_path(data_path)), NA)
    expect_match(m, "Call `usethis::edit_r_environ()` to open '.Renviron'", fixed = TRUE, all = FALSE)
    expect_match(m, "Store your data path with a line like:", all = FALSE)
    expect_match(m, "PORTALR_DATA_PATH=", all = FALSE)
    expect_match(m, "Make sure '.Renviron' ends with a newline!", all = FALSE)
})

test_that("default data path functions work if set", {
    Sys.setenv("PORTALR_DATA_PATH" = tempdir())
    expect_true(check_default_data_path())
    expect_equal(get_default_data_path(), tempdir())
})
