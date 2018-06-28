context("checks NDVI summary output")

portal_data_path <- tempdir()
monthly_ndvi = ndvi("monthly",path = portal_data_path)
newmoon_ndvi = ndvi("newmoon", path = portal_data_path)

test_that("Monthly option returns 2 columns", {
  expect_that(dim(monthly_ndvi)[2], equals(2))
  expect_equal(colnames(monthly_ndvi), c("ndvi", "date"))
})

test_that("Newmoon option returns 2 columns", {
  expect_that(dim(newmoon_ndvi)[2], equals(2))
  expect_equal(colnames(newmoon_ndvi), c("newmoonnumber", "ndvi"))
})
