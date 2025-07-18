# checks NDVI summary output

portal_data_path <- tempdir()
monthly_ndvi <- ndvi("monthly", path = portal_data_path)
newmoon_ndvi <- ndvi("newmoon", path = portal_data_path)

test_that("Monthly option returns 2 columns", {
  expect_equal(NCOL(monthly_ndvi), 2)
  expect_equal(colnames(monthly_ndvi), c("date", "ndvi"))
})

test_that("Newmoon option returns 2 columns", {
  expect_equal(dim(newmoon_ndvi)[2], 2)
  expect_named(newmoon_ndvi, c("newmoonnumber", "ndvi"))
})

test_that("Missing monthly option data are filled", {
  ndvi_NA <- monthly_ndvi
  ndvi_NA[10, "ndvi"] <- NA
  expect_true(anyNA(ndvi_NA[, "ndvi"]))
  lasttime <- max(ndvi_NA$date)
  ndvi_NA <- fill_missing_ndvi(ndvi_NA, "monthly", lasttime)
  expect_false(anyNA(ndvi_NA[, "ndvi"]))
})

test_that("Missing moon option data are filled", {
  ndvi_NA <- newmoon_ndvi
  ndvi_NA[10, "ndvi"] <- NA
  expect_true(anyNA(ndvi_NA[, "ndvi"]))
  lasttime <- max(ndvi_NA$newmoonnumber)
  ndvi_NA <- fill_missing_ndvi(ndvi_NA, "newmoon", lasttime)
  expect_false(anyNA(ndvi_NA[, "ndvi"]))
})

test_that("Monthly NDVI can be forecast", {
  fcast_vals <- fcast_ndvi(monthly_ndvi, "monthly", 1)
  expect_type(fcast_vals$ndvi, "double")
})

test_that("Newmoon NDVI can be forecast", {
  moons <- read.csv(
    file.path(portal_data_path, "PortalData", "Rodents", "moon_dates.csv"),
    na.strings = c(""), stringsAsFactors = FALSE)
  fcast_vals <- suppressWarnings(fcast_ndvi(newmoon_ndvi, "newmoon", 1, moons))
  expect_type(fcast_vals$ndvi, "double")
})
