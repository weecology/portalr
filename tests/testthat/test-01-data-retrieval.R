# Check data retrieval
portal_data_path <- tempdir()

test_that("download_observations and check_for_newer_data work", {
    skip_on_cran() # these download checks take a while to run
    skip_if_not_installed("httptest")
    expect_no_error(download_observations(portal_data_path, version = "1.20.0"))
    expect_true(check_for_newer_data(portal_data_path))
    httptest::without_internet({
        expect_false(check_for_newer_data(portal_data_path))
    })
    unlink(file.path(portal_data_path, "PortalData"), recursive = TRUE)

    # Expect a message if requesting latest data
    expect_message(download_observations(portal_data_path))
    # Expecting a message that latest data is already available
    expect_message(download_observations(portal_data_path))
    unlink(file.path(portal_data_path, "PortalData"), recursive = TRUE)

    expect_no_error(download_observations(portal_data_path, version = "1.6.0"))
    expect_true(check_for_newer_data(portal_data_path))
    unlink(file.path(portal_data_path, "PortalData"), recursive = TRUE)
    expect_true(check_for_newer_data(portal_data_path))

    expect_error(download_observations(portal_data_path, version = "1.5.9"))
    expect_error(download_observations(portal_data_path, version = "1.000.0"))

    # Error here
    # do not know how to convert 'pub_date' to class "Date"
    expect_no_error(download_observations(portal_data_path, source = "zenodo", timeout = 300))
    expect_error(download_observations(portal_data_path, source = "xxx"), "`source` must be either 'zenodo' or 'github'")
    expect_false(check_for_newer_data(portal_data_path))
    #unlink(file.path(portal_data_path, "PortalData"), recursive = TRUE)

    expect_no_error(download_observations(portal_data_path))
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
    expect_no_error(data_tables <- load_rodent_data("repo"))
    expect_length(data_tables, 5)
    expect_named(data_tables,
                 c("rodent_data", "species_table", "trapping_table",
                   "newmoons_table", "plots_table"))

    data_tables <- load_rodent_data(portal_data_path)
    expect_length(data_tables, 5)
    expect_named(data_tables,
                 c("rodent_data", "species_table", "trapping_table",
                   "newmoons_table", "plots_table"))
})

test_that("load_plant_data has the right format", {
    skip_on_cran()
    expect_no_error(data_tables <- load_plant_data("repo"))
    expect_length(data_tables, 7)
    expect_named(data_tables,
                 c("quadrat_data", "species_table", "census_table",
                   "date_table", "plots_table", "transect_data", "oldtransect_data"))

    expect_no_error(data_tables <- load_plant_data(portal_data_path))
    expect_equal(length(data_tables), 7)
    expect_equal(names(data_tables),
                 c("quadrat_data", "species_table", "census_table",
                   "date_table", "plots_table", "transect_data", "oldtransect_data"))
})

test_that("load_ant_data works", {
    skip_on_cran()
    expect_no_error(data_tables <- load_ant_data("repo"))
    expect_length(data_tables, 4)
    expect_named(data_tables,
                 c("bait_data", "colony_data", "species_table",
                   "plots_table"))

    expect_no_error(data_tables <- load_ant_data(portal_data_path))
    expect_length(data_tables, 4)
    expect_named(data_tables,
                 c("bait_data", "colony_data", "species_table",
                   "plots_table"))
})

test_that("default data path functions work if unset", {
    Sys.unsetenv("PORTALR_DATA_PATH")
    expect_warning(result <- check_default_data_path(MESSAGE_FUN = warning),
                   "You don't appear to have a defined location for storing Portal data.")
    expect_false(result)

    # Use snapshot to test for no error + message
    expect_snapshot(
    	check_default_data_path(),
    	# transform user path (in case cli uses double quotes on path one day)
    	# If check_default_data_path message changes, the regex here will need to be adjusted
    	transform = function(x) sub("into ['\"].+['\"]", "into '<user_path>'", x)
    )

    expect_error(use_default_data_path())

    data_path <- tempdir()
    # Use snapshot to test for no error + message
    expect_snapshot(
    	use_default_data_path(data_path),
    	# avoid showing temp path in snapshot
    	# If use_default_data_path message changes, the regex here will need to be adjusted
    	transform = function(x) sub("=\".+\"", "=\"<portalr_path>\"", x)
    )
})

test_that("default data path functions work if set", {
    Sys.setenv("PORTALR_DATA_PATH" = tempdir())
    expect_true(check_default_data_path())
    expect_equal(get_default_data_path(), tempdir())
})
